defmodule ZapierTriggers.Workers.DeliveryWorker do
  @moduledoc """
  Oban worker for delivering events to webhook URLs.
  Implements retry logic with exponential backoff.
  """
  use Oban.Worker,
    queue: :delivery,
    max_attempts: 5,
    priority: 1

  require Logger

  alias ZapierTriggers.{Repo, Events}
  alias ZapierTriggers.Events.{Event, EventDelivery}
  alias ZapierTriggers.Organizations.Organization

  import Ecto.Query

  @impl Oban.Worker
  def perform(%Oban.Job{args: %{"event_id" => event_id, "delivery_id" => delivery_id}, attempt: attempt}) do
    # Load event, delivery, and organization
    event = Repo.get!(Event, event_id) |> Repo.preload(:organization)
    delivery = Repo.get!(EventDelivery, delivery_id)
    organization = event.organization

    # Skip if webhook delivery is disabled (for testing)
    if Application.get_env(:zapier_triggers, :disable_webhook_delivery, false) do
      Logger.info("Webhook delivery disabled, skipping", event_id: event_id)
      return :ok
    end

    # Skip if no webhook URL configured
    unless organization.webhook_url do
      Logger.warning("No webhook URL configured for organization #{organization.id}, marking as failed")
      update_delivery(delivery, "failed", nil, "No webhook URL configured")
      return :ok
    end

    # Attempt delivery
    case deliver_webhook(event, organization) do
      {:ok, status_code} when status_code in 200..299 ->
        Logger.info("Event #{event_id} delivered successfully",
          event_id: event_id,
          status_code: status_code,
          attempt: attempt
        )
        update_delivery(delivery, "delivered", status_code, nil)
        :ok

      {:ok, status_code} ->
        error_msg = "Webhook returned #{status_code}"
        Logger.warning("Event #{event_id} delivery failed: #{error_msg}",
          event_id: event_id,
          status_code: status_code,
          attempt: attempt
        )
        update_delivery(delivery, "failed", status_code, error_msg)

        # Move to dead letter queue if max attempts reached
        if attempt >= 5 do
          update_delivery(delivery, "dead_letter", status_code, error_msg)
          {:discard, "Max attempts reached, moved to DLQ"}
        else
          {:error, error_msg}
        end

      {:error, reason} ->
        error_msg = "HTTP error: #{inspect(reason)}"
        Logger.error("Event #{event_id} delivery failed: #{error_msg}",
          event_id: event_id,
          error: inspect(reason),
          attempt: attempt
        )
        update_delivery(delivery, "failed", nil, error_msg)

        if attempt >= 5 do
          update_delivery(delivery, "dead_letter", nil, error_msg)
          {:discard, "Max attempts reached, moved to DLQ"}
        else
          {:error, error_msg}
        end
    end
  end

  defp deliver_webhook(event, organization) do
    headers = [
      {"Content-Type", "application/json"},
      {"X-Event-ID", event.id},
      {"X-Event-Type", event.type}
    ]

    body = Jason.encode!(%{
      id: event.id,
      type: event.type,
      data: event.payload,
      timestamp: event.inserted_at
    })

    case HTTPoison.post(organization.webhook_url, body, headers, recv_timeout: 30_000) do
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:ok, status_code}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end

  defp update_delivery(delivery, status, response_status, error) do
    delivery
    |> EventDelivery.changeset(%{
      status: status,
      attempts: delivery.attempts + 1,
      response_status: response_status,
      last_error: error
    })
    |> Repo.update!()
  end
end
