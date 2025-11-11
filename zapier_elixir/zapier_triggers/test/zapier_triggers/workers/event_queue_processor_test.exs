defmodule ZapierTriggers.Workers.EventQueueProcessorTest do
  use ZapierTriggers.DataCase, async: false

  alias ZapierTriggers.{Repo, Organizations, Events}
  alias ZapierTriggers.Organizations.Organization
  alias ZapierTriggers.Events.{Event, EventQueue, EventDelivery}
  alias ZapierTriggers.Workers.EventQueueProcessor

  import Ecto.Query

  setup do
    # Create test organization
    {api_key, api_key_hash} = Organization.generate_api_key()

    {:ok, org} = %Organization{}
      |> Organization.changeset(%{
        name: "Test Org",
        api_key_hash: api_key_hash,
        webhook_url: "https://example.com/webhook",
        tier: "free"
      })
      |> Repo.insert()

    {:ok, organization: org}
  end

  describe "process_single_event/1" do
    @tag :skip
    test "successfully processes a queued event", %{organization: org} do
      # Create queue item
      queue_item = %EventQueue{
        id: Ecto.UUID.generate(),
        organization_id: org.id,
        type: "user.created",
        payload: %{"user_id" => "123"},
        dedup_id: "test-dedup-#{System.unique_integer([:positive])}",
        status: "pending",
        inserted_at: DateTime.utc_now()
      }

      # Process the event (calling private function via apply)
      result = apply(EventQueueProcessor, :process_single_event, [queue_item])

      # Verify success
      assert {:ok, :processed} = result

      # Verify event was persisted
      event = Repo.get(Event, queue_item.id)
      assert event != nil
      assert event.type == "user.created"
      assert event.organization_id == org.id

      # Verify delivery was created
      delivery = Repo.get_by(EventDelivery, event_id: event.id)
      assert delivery != nil
      assert delivery.status == "pending"
    end

    @tag :skip
    test "handles duplicate events gracefully", %{organization: org} do
      dedup_id = "duplicate-test-#{System.unique_integer([:positive])}"

      # Create first event directly in DB
      {:ok, _existing_event} = %Event{}
        |> Event.changeset(%{
          type: "user.created",
          payload: %{"user_id" => "123"},
          dedup_id: dedup_id,
          organization_id: org.id
        })
        |> Repo.insert()

      # Try to process duplicate via queue
      queue_item = %EventQueue{
        id: Ecto.UUID.generate(),
        organization_id: org.id,
        type: "user.created",
        payload: %{"user_id" => "123"},
        dedup_id: dedup_id,
        status: "pending",
        inserted_at: DateTime.utc_now()
      }

      # Process the duplicate event
      result = apply(EventQueueProcessor, :process_single_event, [queue_item])

      # Verify it was handled as duplicate
      assert {:ok, :duplicate} = result

      # Verify only one event exists
      event_count = from(e in Event, where: e.dedup_id == ^dedup_id, select: count(e.id))
        |> Repo.one()

      assert event_count == 1
    end

    @tag :skip
    test "handles event insert failure gracefully", %{organization: org} do
      # Create queue item with invalid data (missing required field)
      queue_item = %EventQueue{
        id: Ecto.UUID.generate(),
        organization_id: org.id,
        type: nil,  # Invalid: type is required
        payload: %{"user_id" => "123"},
        dedup_id: nil,
        status: "pending",
        inserted_at: DateTime.utc_now()
      }

      # Process the event
      result = apply(EventQueueProcessor, :process_single_event, [queue_item])

      # Verify it returned error
      assert {:error, :event_insert_failed} = result

      # Verify no event was created
      event = Repo.get(Event, queue_item.id)
      assert event == nil
    end

    @tag :skip
    test "without dedup_id processes successfully", %{organization: org} do
      # Create queue item without dedup_id
      queue_item = %EventQueue{
        id: Ecto.UUID.generate(),
        organization_id: org.id,
        type: "user.created",
        payload: %{"user_id" => "123"},
        dedup_id: nil,  # No deduplication
        status: "pending",
        inserted_at: DateTime.utc_now()
      }

      # Process the event
      result = apply(EventQueueProcessor, :process_single_event, [queue_item])

      # Verify success
      assert {:ok, :processed} = result

      # Verify event was persisted
      event = Repo.get(Event, queue_item.id)
      assert event != nil
      assert event.dedup_id == nil
    end
  end

  describe "exponential backoff" do
    @tag :skip
    test "increases poll interval when queue is empty" do
      initial_state = %{poll_interval: 100, empty_polls: 0}

      # Simulate empty queue (0 events processed)
      new_state = apply(EventQueueProcessor, :adjust_poll_interval, [initial_state, 0])

      # Verify interval increased
      assert new_state.poll_interval == 200
      assert new_state.empty_polls == 1

      # Simulate another empty poll
      new_state2 = apply(EventQueueProcessor, :adjust_poll_interval, [new_state, 0])

      # Verify interval doubled again
      assert new_state2.poll_interval == 400
      assert new_state2.empty_polls == 2
    end

    @tag :skip
    test "resets poll interval when events are processed" do
      backoff_state = %{poll_interval: 1000, empty_polls: 5}

      # Simulate processing events
      new_state = apply(EventQueueProcessor, :adjust_poll_interval, [backoff_state, 10])

      # Verify interval reset to minimum
      assert new_state.poll_interval == 100
      assert new_state.empty_polls == 0
    end

    @tag :skip
    test "caps poll interval at maximum" do
      state = %{poll_interval: 2000, empty_polls: 10}

      # Try to increase beyond max
      new_state = apply(EventQueueProcessor, :adjust_poll_interval, [state, 0])

      # Verify it's capped at 2000ms
      assert new_state.poll_interval == 2000
    end
  end

  describe "backpressure" do
    @tag :skip
    test "warns when queue depth exceeds threshold", %{organization: org} do
      # Create many queue items
      for i <- 1..1100 do
        %EventQueue{}
        |> EventQueue.changeset(%{
          id: Ecto.UUID.generate(),
          organization_id: org.id,
          type: "test.event",
          payload: %{"index" => i},
          dedup_id: "backpressure-#{i}",
          status: "pending",
          inserted_at: DateTime.utc_now()
        })
        |> Repo.insert!()
      end

      # Get queue depth
      depth = apply(EventQueueProcessor, :get_queue_depth, [])

      # Verify it exceeds threshold
      assert depth > 1000
    end
  end
end
