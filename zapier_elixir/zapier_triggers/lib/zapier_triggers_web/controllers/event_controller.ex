defmodule ZapierTriggersWeb.EventController do
  use Phoenix.Controller, formats: [:json]

  require Logger
  alias ZapierTriggers.Events.EventQueue

  @doc """
  Create event - ASYNC INGESTION for instant response (<10ms).

  Flow:
  1. Fast validations (rate limit, payload size) - handled by plugs
  2. Insert to event_queue table (~5-10ms)
  3. Return 202 Accepted immediately
  4. EventQueueProcessor handles persistence asynchronously

  This ensures < 100ms response time (target < 10ms).
  """
  def create(conn, %{"type" => type, "payload" => payload} = params) do
    organization = conn.assigns.current_organization

    # Validate payload size (256KB max)
    payload_size = byte_size(Jason.encode!(payload))

    if payload_size > 256 * 1024 do
      conn
      |> put_status(:request_entity_too_large)
      |> json(%{error: "Payload exceeds 256KB limit", size: payload_size})
    else
      dedup_id = Map.get(params, "dedup_id")

      # Fast queue insertion (~5-10ms)
      case EventQueue.create_queue_item(organization.id, type, payload, dedup_id) do
        {:ok, queue_item} ->
          Logger.info("Event queued for processing",
            event_id: queue_item.id,
            type: type,
            organization_id: organization.id
          )

          # Return 202 Accepted immediately (instant response)
          conn
          |> put_status(:accepted)
          |> json(%{
            id: queue_item.id,
            status: "accepted",
            message: "Event queued for processing"
          })

        {:error, changeset} ->
          Logger.error("Failed to queue event: #{inspect(changeset.errors)}",
            organization_id: organization.id
          )

          conn
          |> put_status(:unprocessable_entity)
          |> json(%{error: "Failed to queue event", details: changeset.errors})
      end
    end
  end

  def create(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "Missing required fields: type, payload"})
  end
end
