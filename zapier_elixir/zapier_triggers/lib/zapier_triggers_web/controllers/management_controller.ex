defmodule ZapierTriggersWeb.ManagementController do
  use Phoenix.Controller, formats: [:json]

  import Ecto.Query
  alias ZapierTriggers.{Repo, Organizations}
  alias ZapierTriggers.Organizations.Organization
  alias ZapierTriggers.Events.{Event, EventDelivery}

  @doc """
  Configure webhook URL for the organization.
  """
  def configure_webhook(conn, %{"webhook_url" => webhook_url}) do
    organization = conn.assigns.current_organization

    changeset = Organization.changeset(organization, %{webhook_url: webhook_url})

    case Repo.update(changeset) do
      {:ok, _updated_org} ->
        conn
        |> json(%{
          message: "Webhook URL configured successfully",
          webhook_url: webhook_url
        })

      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Failed to configure webhook", details: changeset.errors})
    end
  end

  def configure_webhook(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "Missing required field: webhook_url"})
  end

  @doc """
  List events in the inbox with optional filtering.
  Supports status filter and pagination.
  """
  def inbox(conn, params) do
    organization = conn.assigns.current_organization
    status_filter = Map.get(params, "status")
    limit = Map.get(params, "limit", "50") |> String.to_integer()
    offset = Map.get(params, "offset", "0") |> String.to_integer()

    # Build query
    query =
      from e in Event,
        join: d in EventDelivery,
        on: d.event_id == e.id,
        where: e.organization_id == ^organization.id,
        order_by: [desc: e.inserted_at],
        limit: ^limit,
        offset: ^offset,
        select: %{
          id: e.id,
          type: e.type,
          payload: e.payload,
          dedup_id: e.dedup_id,
          status: d.status,
          attempts: d.attempts,
          response_status: d.response_status,
          last_error: d.last_error,
          created_at: e.inserted_at,
          updated_at: d.updated_at
        }

    # Apply status filter if provided
    query =
      if status_filter do
        from [e, d] in query, where: d.status == ^status_filter
      else
        query
      end

    events = Repo.all(query)
    total_count =
      from(e in Event, where: e.organization_id == ^organization.id, select: count(e.id))
      |> Repo.one()

    conn
    |> json(%{
      events: events,
      total: total_count,
      limit: limit,
      offset: offset
    })
  end

  @doc """
  Manually acknowledge/delete an event.
  """
  def acknowledge(conn, %{"event_id" => event_id}) do
    organization = conn.assigns.current_organization

    # Load event and verify ownership
    case Repo.get_by(Event, id: event_id, organization_id: organization.id) do
      nil ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Event not found"})

      event ->
        # Delete event (cascades to deliveries)
        Repo.delete!(event)

        conn
        |> json(%{message: "Event acknowledged and deleted", event_id: event_id})
    end
  end

  def acknowledge(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "Missing required field: event_id"})
  end
end
