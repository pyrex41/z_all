defmodule ZapierTriggers.Workers.EventQueueProcessor do
  @moduledoc """
  Polls the event_queue table and processes events asynchronously.
  This enables instant HTTP response (<10ms) while handling persistence in the background.
  """
  use GenServer
  require Logger

  alias ZapierTriggers.{Repo, Events}
  alias ZapierTriggers.Events.{Event, EventDelivery, EventQueue}
  alias ZapierTriggers.Workers.DeliveryWorker

  import Ecto.Query

  @poll_interval 100  # Poll every 100ms
  @batch_size 100     # Process up to 100 events per batch

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @impl true
  def init(state) do
    # Start polling immediately
    schedule_poll()
    {:ok, state}
  end

  @impl true
  def handle_info(:poll, state) do
    process_queue_batch()
    schedule_poll()
    {:noreply, state}
  end

  defp schedule_poll do
    Process.send_after(self(), :poll, @poll_interval)
  end

  defp process_queue_batch do
    try do
      # Fetch and delete queued events atomically using SKIP LOCKED
      # This prevents multiple workers from processing the same events
      {:ok, queued_events} = Repo.transaction(fn ->
        from(q in EventQueue,
          where: q.status == "pending",
          order_by: [asc: q.inserted_at],
          limit: ^@batch_size,
          lock: "FOR UPDATE SKIP LOCKED"
        )
        |> Repo.all()
        |> Enum.map(fn queue_item ->
          # Delete from queue
          Repo.delete!(queue_item)
          queue_item
        end)
      end, timeout: 10_000)

      if length(queued_events) > 0 do
        Logger.info("Processing #{length(queued_events)} queued events")

        # Process events in parallel
        queued_events
        |> Task.async_stream(&process_single_event/1, max_concurrency: 50, timeout: 30_000)
        |> Stream.run()
      end
    rescue
      e ->
        Logger.error("Error processing queue batch: #{inspect(e)}")
    end
  end

  defp process_single_event(queue_item) do
    try do
      # 1. Check deduplication (if present)
      if queue_item.dedup_id && is_duplicate?(queue_item.dedup_id, queue_item.organization_id) do
        Logger.warning("Duplicate event #{queue_item.id} detected, skipping",
          event_id: queue_item.id,
          dedup_id: queue_item.dedup_id
        )
        return :duplicate
      end

      # 2. Mark as seen in dedup cache (24-hour TTL)
      if queue_item.dedup_id do
        cache_key = "dedup:#{queue_item.organization_id}:#{queue_item.dedup_id}"
        Cachex.put(:dedup_cache, cache_key, true, ttl: :timer.hours(24))
      end

      # 3. Persist event to database
      {:ok, event} = %Event{}
        |> Event.changeset(%{
          id: queue_item.id,
          type: queue_item.type,
          payload: queue_item.payload,
          dedup_id: queue_item.dedup_id,
          organization_id: queue_item.organization_id
        })
        |> Repo.insert()

      # 4. Create delivery record
      {:ok, delivery} = %EventDelivery{}
        |> EventDelivery.changeset(%{
          event_id: event.id,
          status: "pending",
          attempts: 0
        })
        |> Repo.insert()

      # 5. Queue webhook delivery via Oban
      %{event_id: event.id, delivery_id: delivery.id}
      |> DeliveryWorker.new()
      |> Oban.insert()

      Logger.info("Event #{event.id} processed successfully",
        event_id: event.id,
        type: event.type,
        organization_id: queue_item.organization_id
      )

      :ok
    rescue
      e ->
        Logger.error("Failed to process event #{queue_item.id}: #{inspect(e)}",
          event_id: queue_item.id,
          error: inspect(e)
        )
        :error
    end
  end

  defp is_duplicate?(dedup_id, organization_id) do
    cache_key = "dedup:#{organization_id}:#{dedup_id}"

    case Cachex.get(:dedup_cache, cache_key) do
      {:ok, nil} ->
        # Check database as fallback
        from(e in Event,
          where: e.dedup_id == ^dedup_id and e.organization_id == ^organization_id
        )
        |> Repo.exists?()

      {:ok, _value} ->
        true

      {:error, _} ->
        # Fail open - if cache is unavailable, check database
        from(e in Event,
          where: e.dedup_id == ^dedup_id and e.organization_id == ^organization_id
        )
        |> Repo.exists?()
    end
  end
end
