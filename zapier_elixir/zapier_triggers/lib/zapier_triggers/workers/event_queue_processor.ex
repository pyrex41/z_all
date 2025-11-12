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

  # Configuration - can be overridden in config.exs
  @min_poll_interval Application.compile_env(:zapier_triggers, [__MODULE__, :min_poll_interval], 100)
  @max_poll_interval Application.compile_env(:zapier_triggers, [__MODULE__, :max_poll_interval], 2_000)
  @idle_poll_interval Application.compile_env(:zapier_triggers, [__MODULE__, :idle_poll_interval], 30_000) # 30 seconds when completely idle
  @batch_size Application.compile_env(:zapier_triggers, [__MODULE__, :batch_size], 100)
  @max_concurrency Application.compile_env(:zapier_triggers, [__MODULE__, :max_concurrency], 20)
  @max_queue_depth Application.compile_env(:zapier_triggers, [__MODULE__, :max_queue_depth], 1_000)
  @stuck_item_timeout Application.compile_env(:zapier_triggers, [__MODULE__, :stuck_item_timeout], 300_000) # 5 minutes
  @idle_threshold 10 # Number of empty polls before entering deep idle mode

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{poll_interval: @min_poll_interval, empty_polls: 0}, name: __MODULE__)
  end

  @impl true
  def init(state) do
    # Clean up any stuck "processing" items from previous crashes
    cleanup_stuck_items()

    # Start polling immediately
    schedule_poll(state.poll_interval)

    # Schedule periodic cleanup of stuck items (every 5 minutes)
    schedule_cleanup()

    {:ok, state}
  end

  @impl true
  def handle_info(:cleanup_stuck_items, state) do
    cleanup_stuck_items()
    schedule_cleanup()
    {:noreply, state}
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

      # Enter deep idle mode after threshold consecutive empty polls
      new_interval = if empty_polls >= @idle_threshold do
        if empty_polls == @idle_threshold do
          Logger.info("Queue idle for #{@idle_threshold} polls, entering deep idle mode (#{@idle_poll_interval}ms)")
        end
        @idle_poll_interval
      else
        min(@max_poll_interval, state.poll_interval * 2)
      end

      if empty_polls == 1 do
        Logger.debug("Queue empty, backing off to #{new_interval}ms")
      end

      %{state | poll_interval: new_interval, empty_polls: empty_polls}
    else
      # Queue has events, reset to minimum interval
      if state.empty_polls >= @idle_threshold do
        Logger.info("Queue active again, exiting deep idle mode")
      else
        if state.empty_polls > 0 do
          Logger.debug("Queue active, resuming fast polling")
        end
      end

      %{state | poll_interval: @min_poll_interval, empty_polls: 0}
    end
  end

  defp process_queue_batch(state) do
    try do
      # HYBRID APPROACH: Check cache first, then DB
      # 1. Get cached events (ultra-fast, in-memory)
      cached_events = get_cached_events(@batch_size)

      # 2. If cache has events, process them and persist to DB
      queued_events = if length(cached_events) > 0 do
        # Persist cached events to DB for durability
        Enum.map(cached_events, fn cached_item ->
          # Convert map to EventQueue struct for consistency
          struct(EventQueue, cached_item)
        end)
      else
        # 3. Fall back to DB if cache is empty (old flow for migration/testing)
        result = Repo.transaction(fn ->
          from(q in EventQueue,
            where: q.status == "pending",
            order_by: [asc: q.inserted_at],
            limit: ^@batch_size,
            lock: "FOR UPDATE SKIP LOCKED"
          )
          |> Repo.all()
          |> Enum.map(fn queue_item ->
            # Mark as processing and record when processing started (not deleted yet!)
            {:ok, updated_item} = queue_item
              |> Ecto.Changeset.change(%{
                status: "processing",
                processing_started_at: DateTime.utc_now()
              })
              |> Repo.update()
            updated_item
          end)
        end, timeout: 10_000)

        case result do
          {:ok, items} -> items
          {:error, _reason} -> []
        end
      end

      # Continue with processing
      result = {:ok, queued_events}

      case result do
        {:ok, queued_events} when length(queued_events) > 0 ->
          queue_depth = length(queued_events)

          # Only check for backlog if we got a full batch (might be more queued)
          if queue_depth >= @batch_size do
            # Quick check if there are significantly more items queued
            remaining = get_queue_depth()
            if remaining > @max_queue_depth do
              Logger.warning("Queue depth #{remaining} exceeds threshold #{@max_queue_depth}, possible backlog",
                queue_depth: remaining
              )
            end
          end

          Logger.info("Processing #{queue_depth} queued events")

          # Process events in parallel with reduced concurrency
          results = queued_events
            |> Task.async_stream(
                fn item -> {item, process_single_event(item)} end,
                max_concurrency: @max_concurrency,
                timeout: 30_000,
                on_timeout: :kill_task
              )
            |> Enum.to_list()

          # Count successes, failures, and timeouts
          stats = Enum.reduce(results, %{ok: 0, error: 0, timeout: 0}, fn
            {:ok, {queue_item, {:ok, _result}}}, acc ->
              # Success! Delete from queue
              Repo.delete(queue_item)
              %{acc | ok: acc.ok + 1}

            {:ok, {queue_item, {:error, _reason}}}, acc ->
              # Error during processing - revert to pending for retry
              queue_item
              |> Ecto.Changeset.change(%{status: "pending"})
              |> Repo.update()
              %{acc | error: acc.error + 1}

            {:exit, :timeout}, acc ->
              Logger.error("Task timeout in event processing - event remains in processing state")
              %{acc | timeout: acc.timeout + 1}
          end)

          if stats.error > 0 or stats.timeout > 0 do
            Logger.warning("Batch completed: #{stats.ok} ok, #{stats.error} errors, #{stats.timeout} timeouts",
              successes: stats.ok,
              errors: stats.error,
              timeouts: stats.timeout
            )
          end

          if stats.timeout > 0 do
            Logger.error("#{stats.timeout} tasks timed out - investigate performance or increase timeout")
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

  defp get_cached_events(limit) do
    # Scan cache for event_queue:* keys and retrieve up to `limit` items
    case Cachex.keys(:event_queue_cache) do
      {:ok, keys} when is_list(keys) ->
        keys
        |> Enum.take(limit)
        |> Enum.map(fn key ->
          case Cachex.get(:event_queue_cache, key) do
            {:ok, item} when not is_nil(item) ->
              # Remove from cache after retrieving (single-processing guarantee)
              Cachex.del(:event_queue_cache, key)
              item
            _ ->
              nil
          end
        end)
        |> Enum.filter(&(&1 != nil))

      _ ->
        []
    end
  rescue
    e ->
      Logger.error("Error retrieving cached events: #{inspect(e)}")
      []
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
    # IDEMPOTENCY CHECK: Handle case where DB crashed between phase 2 (insert event)
    # and phase 3 (delete from queue). Event exists in both tables.
    case Repo.get(Event, queue_item.id) do
      %Event{} = existing_event ->
        # Event already processed! Just return success.
        # The queue item will be deleted in the success handler.
        Logger.info("Event #{queue_item.id} already processed (idempotency), skipping reprocessing",
          event_id: queue_item.id,
          type: existing_event.type
        )
        {:ok, :already_processed}

      nil ->
        # Event not yet processed, continue with normal flow
        process_new_event(queue_item)
    end
  rescue
    e ->
      Logger.error("Unexpected error in idempotency check for event #{queue_item.id}: #{inspect(e)}",
        event_id: queue_item.id,
        error: inspect(e)
      )
      {:error, :unexpected_error}
  end

  defp process_new_event(queue_item) do
    # Note: Cache-based deduplication check is removed in favor of DB constraint
    # This prevents race conditions where multiple events pass the cache check
    # but only one should be persisted. The DB unique constraint on dedup_id
    # is the source of truth.

    # 1. Validate payload size (256KB max) - moved from sync endpoint
    payload_size = byte_size(Jason.encode!(queue_item.payload))
    if payload_size > 256 * 1024 do
      Logger.error("Event #{queue_item.id} exceeds 256KB payload limit",
        event_id: queue_item.id,
        payload_size: payload_size
      )
      {:error, :payload_too_large}
    else
      # 2. Mark as seen in dedup cache (optimistic, 24-hour TTL)
      if queue_item.dedup_id do
        cache_key = "dedup:#{queue_item.organization_id}:#{queue_item.dedup_id}"
        Cachex.put(:dedup_cache, cache_key, true, ttl: :timer.hours(24))
      end

      # 3. Persist event to database with proper error handling
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

  defp schedule_cleanup do
    # Schedule cleanup every 5 minutes
    Process.send_after(self(), :cleanup_stuck_items, 300_000)
  end

  @doc """
  Cleans up events stuck in "processing" status.
  These are events that were being processed when the worker crashed.
  Reverts them to "pending" so they can be retried.

  Uses processing_started_at (not inserted_at) to accurately identify stuck items.
  An event queued 10 minutes ago but started processing 1 minute ago is NOT stuck.
  """
  def cleanup_stuck_items do
    cutoff_time = DateTime.utc_now() |> DateTime.add(-@stuck_item_timeout, :millisecond)

    {count, _} = from(q in EventQueue,
      where: q.status == "processing"
        and not is_nil(q.processing_started_at)
        and q.processing_started_at < ^cutoff_time
    )
    |> Repo.update_all(set: [status: "pending", processing_started_at: nil])

    if count > 0 do
      Logger.warning("Cleaned up #{count} stuck queue items, reverted to pending",
        count: count,
        timeout_ms: @stuck_item_timeout
      )
    end

    count
  rescue
    e ->
      Logger.error("Failed to cleanup stuck items: #{inspect(e)}")
      0
  end

end
