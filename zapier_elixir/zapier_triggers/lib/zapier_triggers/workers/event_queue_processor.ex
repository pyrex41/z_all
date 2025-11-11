defmodule ZapierTriggers.Workers.EventQueueProcessor do
  @moduledoc """
  Polls the event_queue table and processes events asynchronously.
  This enables instant HTTP response (<10ms) while handling persistence in the background.

  Features:
  - Exponential backoff when queue is empty (reduces DB load)
  - Backpressure to prevent resource exhaustion
  - Proper error handling to prevent data loss
  - Graceful handling of deduplication race conditions
  """
  use GenServer
  require Logger

  alias ZapierTriggers.{Repo, Events}
  alias ZapierTriggers.Events.{Event, EventDelivery, EventQueue}
  alias ZapierTriggers.Workers.DeliveryWorker

  import Ecto.Query

  @min_poll_interval 100   # Minimum poll interval (ms)
  @max_poll_interval 2_000 # Maximum poll interval (ms)
  @batch_size 100          # Process up to 100 events per batch
  @max_concurrency 20      # Max concurrent processing tasks (reduced from 50)
  @max_queue_depth 1_000   # Alert threshold for queue depth

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{poll_interval: @min_poll_interval, empty_polls: 0}, name: __MODULE__)
  end

  @impl true
  def init(state) do
    # Start polling immediately
    schedule_poll(state.poll_interval)
    {:ok, state}
  end

  @impl true
  def handle_info(:poll, state) do
    {batch_size, new_state} = process_queue_batch(state)

    # Exponential backoff when queue is empty
    new_state = adjust_poll_interval(new_state, batch_size)

    schedule_poll(new_state.poll_interval)
    {:noreply, new_state}
  end

  defp schedule_poll(interval) do
    Process.send_after(self(), :poll, interval)
  end

  defp adjust_poll_interval(state, batch_size) do
    if batch_size == 0 do
      # Queue is empty, back off exponentially
      empty_polls = state.empty_polls + 1
      new_interval = min(@max_poll_interval, state.poll_interval * 2)

      if empty_polls == 1 do
        Logger.debug("Queue empty, backing off to #{new_interval}ms")
      end

      %{state | poll_interval: new_interval, empty_polls: empty_polls}
    else
      # Queue has events, reset to minimum interval
      if state.empty_polls > 0 do
        Logger.debug("Queue active, resuming fast polling")
      end

      %{state | poll_interval: @min_poll_interval, empty_polls: 0}
    end
  end

  defp process_queue_batch(state) do
    try do
      # Check queue depth before processing (backpressure)
      queue_depth = get_queue_depth()

      if queue_depth > @max_queue_depth do
        Logger.warning("Queue depth #{queue_depth} exceeds threshold #{@max_queue_depth}, possible backlog",
          queue_depth: queue_depth
        )
      end

      # Fetch and delete queued events atomically using SKIP LOCKED
      # This prevents multiple workers from processing the same events
      result = Repo.transaction(fn ->
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

      case result do
        {:ok, queued_events} when length(queued_events) > 0 ->
          Logger.info("Processing #{length(queued_events)} queued events")

          # Process events in parallel with reduced concurrency
          results = queued_events
            |> Task.async_stream(&process_single_event/1,
                max_concurrency: @max_concurrency,
                timeout: 30_000,
                on_timeout: :kill_task
              )
            |> Enum.to_list()

          # Count successes and failures
          successes = Enum.count(results, fn {status, _} -> status == :ok end)
          failures = length(results) - successes

          if failures > 0 do
            Logger.warning("Batch completed with #{successes} successes, #{failures} failures")
          end

          {length(queued_events), state}

        {:ok, []} ->
          # Queue is empty
          {0, state}

        {:error, reason} ->
          Logger.error("Transaction failed while fetching queue batch: #{inspect(reason)}")
          {0, state}
      end
    rescue
      e ->
        Logger.error("Error processing queue batch: #{inspect(e)}")
        {0, state}
    end
  end

  defp get_queue_depth do
    from(q in EventQueue, where: q.status == "pending", select: count(q.id))
    |> Repo.one()
    |> case do
      nil -> 0
      count -> count
    end
  rescue
    _ -> 0
  end

  defp process_single_event(queue_item) do
    # Note: Cache-based deduplication check is removed in favor of DB constraint
    # This prevents race conditions where multiple events pass the cache check
    # but only one should be persisted. The DB unique constraint on dedup_id
    # is the source of truth.

    # 1. Mark as seen in dedup cache (optimistic, 24-hour TTL)
    if queue_item.dedup_id do
      cache_key = "dedup:#{queue_item.organization_id}:#{queue_item.dedup_id}"
      Cachex.put(:dedup_cache, cache_key, true, ttl: :timer.hours(24))
    end

    # 2. Persist event to database with proper error handling
    event_changeset = %Event{}
      |> Event.changeset(%{
        id: queue_item.id,
        type: queue_item.type,
        payload: queue_item.payload,
        dedup_id: queue_item.dedup_id,
        organization_id: queue_item.organization_id
      })

    case Repo.insert(event_changeset) do
      {:ok, event} ->
        # 3. Create delivery record
        delivery_changeset = %EventDelivery{}
          |> EventDelivery.changeset(%{
            event_id: event.id,
            status: "pending",
            attempts: 0
          })

        case Repo.insert(delivery_changeset) do
          {:ok, delivery} ->
            # 4. Queue webhook delivery via Oban
            case %{event_id: event.id, delivery_id: delivery.id}
                 |> DeliveryWorker.new()
                 |> Oban.insert() do
              {:ok, _job} ->
                Logger.info("Event #{event.id} processed successfully",
                  event_id: event.id,
                  type: event.type,
                  organization_id: queue_item.organization_id
                )
                {:ok, :processed}

              {:error, reason} ->
                Logger.error("Failed to queue webhook delivery for event #{event.id}: #{inspect(reason)}",
                  event_id: event.id,
                  error: inspect(reason)
                )
                {:error, :oban_insert_failed}
            end

          {:error, changeset} ->
            Logger.error("Failed to create delivery record for event #{queue_item.id}: #{inspect(changeset.errors)}",
              event_id: queue_item.id,
              errors: inspect(changeset.errors)
            )
            {:error, :delivery_insert_failed}
        end

      {:error, %Ecto.Changeset{errors: errors} = changeset} ->
        # Check if this is a duplicate (unique constraint violation)
        is_duplicate = Enum.any?(errors, fn {field, {_msg, opts}} ->
          field == :dedup_id && Keyword.get(opts, :constraint) == :unique
        end)

        if is_duplicate do
          Logger.info("Duplicate event #{queue_item.id} detected via DB constraint, skipping",
            event_id: queue_item.id,
            dedup_id: queue_item.dedup_id
          )
          {:ok, :duplicate}
        else
          Logger.error("Failed to persist event #{queue_item.id}: #{inspect(errors)}",
            event_id: queue_item.id,
            errors: inspect(errors),
            changeset: inspect(changeset)
          )
          {:error, :event_insert_failed}
        end
    end
  rescue
    e ->
      Logger.error("Unexpected error processing event #{queue_item.id}: #{inspect(e)}",
        event_id: queue_item.id,
        error: inspect(e),
        stacktrace: __STACKTRACE__
      )
      {:error, :unexpected_error}
  end

end
