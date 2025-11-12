defmodule ZapierTriggersWeb.EventController do
  use Phoenix.Controller, formats: [:json]

  require Logger
  alias ZapierTriggers.Events.EventQueue

  @doc """
  Create event - ULTRA-FAST CACHE-FIRST INGESTION (<1ms response).

  Flow:
  1. Fast validations (rate limit, payload size) - handled by plugs
  2. Write to in-memory cache (~0.1ms)
  3. Return 202 Accepted immediately (<1ms total)
  4. EventQueueProcessor polls cache, handles deduplication, and persists to DB asynchronously

  Deduplication happens asynchronously in the processor using DB constraints.
  This ensures < 10ms response time (typically < 1ms).
  """
  def create(conn, %{"type" => type, "payload" => payload} = params) do
    organization = conn.assigns.current_organization
    dedup_id = Map.get(params, "dedup_id")
    event_id = Ecto.UUID.generate()

    # Cache-first ingestion: write to memory, return immediately
    # NO synchronous validation or deduplication - handled async by processor
    # Payload size validation happens in processor before DB write
    queue_item = %{
      id: event_id,
      organization_id: organization.id,
      type: type,
      payload: payload,
      dedup_id: dedup_id,
      status: "pending",
      inserted_at: DateTime.utc_now()
    }

    # Write to cache with 5-minute TTL (safety: processor should pick it up within seconds)
    cache_key = "event_queue:#{event_id}"
    case Cachex.put(:event_queue_cache, cache_key, queue_item, ttl: :timer.minutes(5)) do
      {:ok, true} ->
        # Log asynchronously to avoid blocking response
        Task.start(fn ->
          Logger.info("Event cached for async processing",
            event_id: event_id,
            type: type,
            organization_id: organization.id
          )
        end)

        # Return 202 Accepted immediately (sub-millisecond response!)
        conn
        |> put_status(:accepted)
        |> json(%{
          id: event_id,
          status: "accepted",
          message: "Event queued for processing"
        })

      {:error, reason} ->
        Logger.error("Failed to cache event: #{inspect(reason)}",
          organization_id: organization.id
        )

        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Failed to queue event", details: inspect(reason)})
    end
  end

  def create(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "Missing required fields: type, payload"})
  end
end
