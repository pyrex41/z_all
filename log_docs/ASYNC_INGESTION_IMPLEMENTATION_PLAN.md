# Async Ingestion Implementation Plan

**Date:** 2025-11-10
**Goal:** Achieve < 100ms event ingestion latency across all implementations
**Strategy:** Option 1 - Async Everything with Durable Queues

---

## Architecture Overview

### Current Flow (Synchronous - 300-500ms)
```
HTTP Request → Validate → Persist Event → Create Delivery → Queue Job → Return 201
              (All synchronous, blocks response)
```

### New Flow (Asynchronous - Target < 10ms)
```
HTTP Request → Validate → Write to Queue → Return 202 Accepted (< 10ms)
                              ↓
                    Background Worker Pool:
                    1. Dequeue event
                    2. Persist to DB
                    3. Check deduplication (optional - moved post-persist)
                    4. Create delivery job
                    5. Deliver webhook
```

### Key Changes
1. **Response**: `201 Created` → `202 Accepted` with event ID
2. **Validation**: Fast checks only (rate limit, payload size, auth)
3. **Deduplication**: Move to background (accept duplicates, filter later)
4. **Persistence**: Asynchronous via worker pool
5. **Queue**: Durable storage for crash recovery

---

## Implementation Strategy by Language

### 1. Python: Redis Streams

**Why**: Already using Redis, Streams are purpose-built, durable, ~1-2ms XADD

**Current State**:
- ✅ Already has Redis Streams infrastructure
- ✅ Has worker.py with Stream consumer
- ❌ Currently does synchronous DB writes in HTTP path

**Changes Required**:

#### A. Update `/api/events` Endpoint
**File**: `zapier_python/src/zapier_triggers_api/routes/events.py`

**Current**:
```python
async def create_event(...) -> EventResponse:
    await check_rate_limit(org, redis)
    if event_data.dedup_id:
        await check_deduplication(...)  # REMOVE - move to worker

    # Create event and delivery - REMOVE
    event = Event(...)
    delivery = EventDelivery(...)
    session.add(event)
    session.add(delivery)
    await session.commit()  # SLOW - 50-100ms

    await queue_event(...)  # Already async!
    return EventResponse(...), 201
```

**New**:
```python
async def create_event(...) -> AcceptedResponse:
    # Fast validations only
    await check_rate_limit(org, redis)

    # Validate payload size
    payload_size = len(json.dumps(event_data.data))
    if payload_size > 256 * 1024:
        raise HTTPException(413, "Payload exceeds 256KB limit")

    # Generate event ID immediately
    event_id = uuid4()

    # Queue to Redis Streams with full payload
    await redis.xadd(settings.redis_stream_name, {
        "event_id": str(event_id),
        "org_id": str(org.id),
        "type": event_data.type,
        "payload": json.dumps(event_data.data),
        "dedup_id": event_data.dedup_id or "",
        "timestamp": datetime.utcnow().isoformat(),
    })

    # Return immediately
    return AcceptedResponse(
        id=event_id,
        status="accepted",
        message="Event queued for processing"
    ), 202
```

**Response Model**:
```python
class AcceptedResponse(BaseModel):
    id: UUID
    status: Literal["accepted"]
    message: str
```

#### B. Update Worker to Handle Full Persistence
**File**: `zapier_python/src/zapier_triggers_api/worker.py`

**Current**: Worker only handles delivery
**New**: Worker handles persistence + delivery

```python
async def process_event_message(message: dict, session: AsyncSession, redis: Redis):
    """Process event from Redis Stream - full persistence + delivery."""

    # 1. Check deduplication (now async)
    dedup_id = message.get("dedup_id")
    org_id = message["org_id"]

    if dedup_id:
        key = f"dedup:{org_id}:{dedup_id}"
        exists = await redis.get(key)
        if exists:
            logger.warning(f"Duplicate event {message['event_id']} detected, skipping")
            return  # Skip duplicate
        await redis.setex(key, 86400, "1")

    # 2. Persist event to database
    event = Event(
        id=UUID(message["event_id"]),
        org_id=UUID(org_id),
        type=message["type"],
        payload=json.loads(message["payload"]),
        dedup_id=dedup_id,
        created_at=datetime.fromisoformat(message["timestamp"]),
    )

    # 3. Create delivery record
    delivery = EventDelivery(
        id=uuid4(),
        event_id=event.id,
        status=DeliveryStatus.PENDING,
        attempts=0,
        created_at=datetime.utcnow(),
        updated_at=datetime.utcnow(),
    )

    session.add(event)
    session.add(delivery)
    await session.commit()

    # 4. Deliver webhook (existing logic)
    org = await session.get(Organization, UUID(org_id))
    if org and org.webhook_url:
        await deliver_event(event, delivery, org, session)
```

**Latency Breakdown**:
- Validate + Queue: 2-5ms
- Worker processing: Async, doesn't block response
- **Total response time: < 10ms** ✅

---

### 2. Elixir: Broadway + PostgreSQL

**Why**: Elixir best practice, Broadway is purpose-built for this, leverages BEAM

**Current State**:
- ✅ Already has PostgreSQL
- ✅ Already has Oban for jobs
- ❌ Currently does synchronous DB writes in HTTP path

**Changes Required**:

#### A. Add Broadway Dependency
**File**: `zapier_elixir/zapier_triggers/mix.exs`

```elixir
defp deps do
  [
    # ... existing deps ...
    {:broadway, "~> 1.0"},
  ]
end
```

#### B. Create Event Queue Table
**File**: `zapier_elixir/zapier_triggers/priv/repo/migrations/TIMESTAMP_create_event_queue.exs`

```elixir
defmodule ZapierTriggers.Repo.Migrations.CreateEventQueue do
  use Ecto.Migration

  def change do
    create table(:event_queue, primary_key: false) do
      add :id, :uuid, primary_key: true
      add :organization_id, references(:organizations, type: :uuid), null: false
      add :type, :string, null: false
      add :payload, :map, null: false
      add :dedup_id, :string
      add :status, :string, default: "pending", null: false
      add :inserted_at, :utc_datetime_usec, null: false
    end

    create index(:event_queue, [:status, :inserted_at])
    create index(:event_queue, [:organization_id])
  end
end
```

#### C. Create Broadway Pipeline
**File**: `zapier_elixir/zapier_triggers/lib/zapier_triggers/event_broadway.ex`

```elixir
defmodule ZapierTriggers.EventBroadway do
  use Broadway

  alias ZapierTriggers.{Repo, Events, EventDelivery}
  require Logger

  def start_link(_opts) do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module: {BroadwayPostgres.Producer,
          connection: [
            database: "zapier_triggers",
            username: "zapier",
            password: System.get_env("DB_PASSWORD"),
          ],
          query: """
          DELETE FROM event_queue
          WHERE id IN (
            SELECT id FROM event_queue
            WHERE status = 'pending'
            ORDER BY inserted_at
            LIMIT $1
            FOR UPDATE SKIP LOCKED
          )
          RETURNING *
          """,
          receive_interval: 100,
        ],
        concurrency: 1
      ],
      processors: [
        default: [
          concurrency: 50,  # 50 concurrent workers
        ]
      ],
      batchers: [
        default: [
          batch_size: 10,
          batch_timeout: 100,
          concurrency: 10
        ]
      ]
    )
  end

  @impl true
  def handle_message(_processor, message, _context) do
    # Process single event
    %{data: queue_item} = message

    # 1. Check deduplication
    if queue_item.dedup_id do
      case Cachex.get(:dedup_cache, queue_item.dedup_id) do
        {:ok, nil} ->
          Cachex.put(:dedup_cache, queue_item.dedup_id, true, ttl: :timer.hours(24))
        {:ok, _} ->
          Logger.warn("Duplicate event #{queue_item.id} detected, skipping")
          return message
      end
    end

    # 2. Persist event
    {:ok, event} = Events.create_event(%{
      id: queue_item.id,
      organization_id: queue_item.organization_id,
      type: queue_item.type,
      payload: queue_item.payload,
      dedup_id: queue_item.dedup_id,
      created_at: queue_item.inserted_at
    })

    # 3. Create delivery and queue Oban job
    {:ok, delivery} = EventDelivery.create_delivery(event)
    {:ok, _} = Oban.insert(DeliveryWorker.new(%{event_id: event.id}))

    message
  end
end
```

#### D. Update Controller
**File**: `zapier_elixir/zapier_triggers/lib/zapier_triggers_web/controllers/event_controller.ex`

```elixir
def create(conn, %{"type" => type, "payload" => payload} = params) do
  org = conn.assigns.current_organization

  # Fast validations
  with :ok <- check_rate_limit(org),
       :ok <- validate_payload_size(payload) do

    # Generate event ID
    event_id = Ecto.UUID.generate()

    # Insert to queue table (fast - ~5ms)
    {:ok, _} = Repo.insert(%EventQueue{
      id: event_id,
      organization_id: org.id,
      type: type,
      payload: payload,
      dedup_id: Map.get(params, "dedup_id"),
      status: "pending",
      inserted_at: DateTime.utc_now()
    })

    conn
    |> put_status(202)
    |> json(%{
      id: event_id,
      status: "accepted",
      message: "Event queued for processing"
    })
  end
end
```

**Latency Breakdown**:
- Validate + Insert to queue table: 5-10ms
- Broadway processing: Async
- **Total response time: < 10ms** ✅

---

### 3. Rust: PostgreSQL with SKIP LOCKED

**Why**: No new dependencies, excellent sqlx integration, durable, proven pattern

**Current State**:
- ✅ Already has PostgreSQL with sqlx
- ✅ Already has delivery worker
- ❌ Currently does synchronous DB writes in HTTP path

**Changes Required**:

#### A. Create Event Queue Table
**File**: `zapier_rust/migrations/TIMESTAMP_create_event_queue.sql`

```sql
CREATE TABLE event_queue (
    id UUID PRIMARY KEY,
    organization_id UUID NOT NULL REFERENCES organizations(id),
    event_type VARCHAR(255) NOT NULL,
    payload JSONB NOT NULL,
    dedup_id VARCHAR(255),
    status VARCHAR(50) DEFAULT 'pending' NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW() NOT NULL
);

CREATE INDEX idx_event_queue_status_created ON event_queue(status, created_at);
CREATE INDEX idx_event_queue_org ON event_queue(organization_id);
```

#### B. Update Event Handler
**File**: `zapier_rust/src/handlers/events.rs`

**Current**:
```rust
pub async fn create_event(...) -> Result<Json<EventResponse>> {
    // Validate
    check_rate_limit(&state, &org).await?;

    // Persist event - SLOW (50-100ms)
    let event = sqlx::query_as::<_, Event>(...)
        .fetch_one(&state.pool)
        .await?;

    // Create delivery - SLOW
    let delivery = sqlx::query_as::<_, EventDelivery>(...)
        .fetch_one(&state.pool)
        .await?;

    Ok(Json(EventResponse { ... }))
}
```

**New**:
```rust
#[derive(Serialize)]
pub struct AcceptedResponse {
    pub id: Uuid,
    pub status: String,
    pub message: String,
}

pub async fn create_event(...) -> Result<Json<AcceptedResponse>> {
    // Fast validations only
    check_rate_limit(&state, &org).await?;

    let payload_size = serde_json::to_string(&payload.data)?.len();
    if payload_size > 256 * 1024 {
        return Err(Error::PayloadTooLarge);
    }

    // Generate event ID
    let event_id = Uuid::new_v4();

    // Insert to queue (fast - ~5-10ms)
    sqlx::query!(
        r#"
        INSERT INTO event_queue (id, organization_id, event_type, payload, dedup_id, created_at)
        VALUES ($1, $2, $3, $4, $5, NOW())
        "#,
        event_id,
        org.id,
        payload.event_type,
        serde_json::to_value(&payload.data)?,
        payload.dedup_id,
    )
    .execute(&state.pool)
    .await?;

    // Return immediately
    Ok(Json(AcceptedResponse {
        id: event_id,
        status: "accepted".to_string(),
        message: "Event queued for processing".to_string(),
    }))
}
```

#### C. Create Queue Worker
**File**: `zapier_rust/src/workers/queue_processor.rs`

```rust
pub fn start_queue_processor(pool: PgPool, disable_webhook_delivery: bool) -> JoinHandle<()> {
    tokio::spawn(async move {
        queue_processor_loop(pool, disable_webhook_delivery).await;
    })
}

async fn queue_processor_loop(pool: PgPool, disable_webhook_delivery: bool) {
    let mut interval = tokio::time::interval(Duration::from_millis(100));

    loop {
        interval.tick().await;

        if let Err(e) = process_queue_batch(&pool, disable_webhook_delivery).await {
            tracing::error!("Error processing queue: {:?}", e);
        }
    }
}

async fn process_queue_batch(pool: &PgPool, disable_webhook_delivery: bool) -> anyhow::Result<()> {
    // Fetch batch with SKIP LOCKED
    let queued_events = sqlx::query_as!(
        QueuedEvent,
        r#"
        DELETE FROM event_queue
        WHERE id IN (
            SELECT id FROM event_queue
            WHERE status = 'pending'
            ORDER BY created_at
            LIMIT 100
            FOR UPDATE SKIP LOCKED
        )
        RETURNING *
        "#
    )
    .fetch_all(pool)
    .await?;

    if queued_events.is_empty() {
        return Ok(());
    }

    // Process in parallel
    let tasks: Vec<_> = queued_events
        .into_iter()
        .map(|queued| {
            let pool = pool.clone();
            tokio::spawn(async move {
                process_single_queued_event(pool, queued, disable_webhook_delivery).await
            })
        })
        .collect();

    for task in tasks {
        let _ = task.await;
    }

    Ok(())
}

async fn process_single_queued_event(
    pool: PgPool,
    queued: QueuedEvent,
    disable_webhook_delivery: bool
) {
    // 1. Check deduplication (if present)
    if let Some(dedup_id) = &queued.dedup_id {
        // Check if already processed
        let exists = sqlx::query_scalar!(
            "SELECT EXISTS(SELECT 1 FROM events WHERE dedup_id = $1)",
            dedup_id
        )
        .fetch_one(&pool)
        .await
        .unwrap_or(Some(false));

        if exists == Some(true) {
            tracing::warn!(dedup_id = %dedup_id, "Duplicate event detected, skipping");
            return;
        }
    }

    // 2. Persist event
    let event = sqlx::query_as!(
        Event,
        r#"
        INSERT INTO events (id, organization_id, event_type, payload, dedup_id, created_at)
        VALUES ($1, $2, $3, $4, $5, $6)
        RETURNING *
        "#,
        queued.id,
        queued.organization_id,
        queued.event_type,
        queued.payload,
        queued.dedup_id,
        queued.created_at
    )
    .fetch_one(&pool)
    .await;

    let event = match event {
        Ok(e) => e,
        Err(e) => {
            tracing::error!("Failed to persist event: {:?}", e);
            return;
        }
    };

    // 3. Create delivery record
    let delivery = sqlx::query_as!(
        EventDelivery,
        r#"
        INSERT INTO event_deliveries (id, event_id, status, attempts, created_at, updated_at)
        VALUES ($1, $2, 'pending', 0, NOW(), NOW())
        RETURNING *
        "#,
        Uuid::new_v4(),
        event.id
    )
    .fetch_one(&pool)
    .await;

    // 4. Deliver webhook (existing logic - separate worker handles this)
    tracing::info!(event_id = %event.id, "Event processed and queued for delivery");
}
```

**Latency Breakdown**:
- Validate + Insert to queue: 5-10ms
- Worker processing: Async (polling every 100ms)
- **Total response time: < 10ms** ✅

---

## Deduplication Strategy Change

### Current (Synchronous - Blocking)
```
Request → Check Redis/Cachex → 409 if duplicate → Continue
(Must wait for check before proceeding)
```

### New (Asynchronous - Best Effort)
```
Request → Accept event → Queue → Return 202
              ↓
         Worker: Check dedupe → Skip if duplicate
(Duplicates accepted but filtered in background)
```

**Trade-offs**:
- ✅ Fast response time (< 10ms)
- ⚠️ Short window where duplicate could be accepted (~100-200ms)
- ✅ Eventually consistent (duplicate filtered before persistence)
- ✅ No user-facing impact (webhook still only fires once)

**Mitigation**:
- Dedup check happens in worker before DB insert
- Database unique constraint on dedup_id as safety net
- Very small race window (< 200ms between queue and worker)

---

## Test Suite Changes

### Update Test Expectations

**File**: `unified_test_suite/tests/test_functional.py`

```python
# Current
def test_create_event(api_client):
    response = api_client.create_event(event)
    assert response.status_code == 201
    assert "id" in response.json()
    assert response.json()["status"] == "pending"

# New
def test_create_event(api_client):
    response = api_client.create_event(event)
    assert response.status_code == 202  # Changed
    assert "id" in response.json()
    assert response.json()["status"] == "accepted"  # Changed
    assert "message" in response.json()

    # Wait for processing (poll inbox)
    event_id = response.json()["id"]
    wait_for_event_processing(api_client, event_id, timeout=5.0)

def wait_for_event_processing(client, event_id, timeout=5.0):
    """Poll inbox until event appears (processed by worker)."""
    start = time.time()
    while time.time() - start < timeout:
        inbox = client.list_inbox()
        if any(e["id"] == event_id for e in inbox["events"]):
            return
        time.sleep(0.1)
    raise TimeoutError(f"Event {event_id} not processed within {timeout}s")
```

### Update Deduplication Test

```python
def test_deduplication(api_client):
    event = generate_event("user.created")

    # First request accepted
    response1 = api_client.create_event(event)
    assert response1.status_code == 202
    event_id_1 = response1.json()["id"]

    # Second request also accepted (async dedupe)
    response2 = api_client.create_event(event)
    assert response2.status_code == 202  # Accepted initially
    event_id_2 = response2.json()["id"]

    # Wait for processing
    time.sleep(1.0)

    # Check inbox - only one event should be persisted
    inbox = api_client.list_inbox()
    event_ids = [e["id"] for e in inbox["events"]]

    # One was deduplicated (either first or second, doesn't matter)
    assert event_id_1 in event_ids or event_id_2 in event_ids
    assert not (event_id_1 in event_ids and event_id_2 in event_ids)
```

---

## Performance Targets

### Ingestion Endpoint (HTTP Response)
- **Current**: 300-500ms (sync DB writes)
- **Target**: < 10ms (queue write only)
- **Spec**: < 100ms ✅ **Exceeds requirement**

### Worker Processing (Background)
- **Database persist**: 20-50ms per event
- **Deduplication check**: 1-5ms (Redis/Cachex)
- **Total per event**: 50-100ms
- **Throughput**: 500-1000 events/sec per worker

### End-to-End (Ingestion → Delivery)
- **With workers**: 100-500ms (async, doesn't block ingestion)
- **Without workers**: N/A (ingestion independent)

---

## Migration Strategy

### Phase 1: Implement Queue Infrastructure
1. Python: Already has Redis Streams ✅
2. Elixir: Create event_queue table + Broadway pipeline
3. Rust: Create event_queue table + worker

### Phase 2: Update Ingestion Endpoints
1. Change response to 202 Accepted
2. Remove sync DB writes
3. Write to queue only
4. Deploy with feature flag

### Phase 3: Update Workers
1. Python: Update worker.py to persist from queue
2. Elixir: Broadway pipeline handles full flow
3. Rust: Create queue_processor.rs

### Phase 4: Update Tests
1. Change status code expectations (201 → 202)
2. Add polling helpers for async processing
3. Update deduplication tests for async behavior

### Phase 5: Benchmark & Validate
1. Run `benchmark_single.py` for all three
2. Verify < 100ms (target < 10ms)
3. Validate 100% test pass rate
4. Document results

---

## Rollback Plan

If issues arise:
1. **Feature flag**: `ASYNC_INGESTION=false` falls back to sync
2. **Database**: Queue tables can coexist with sync path
3. **Workers**: Can be stopped without affecting sync path
4. **Tests**: Both 201 and 202 acceptable during transition

---

## Success Criteria

### Performance
- ✅ Event ingestion < 100ms (target < 10ms)
- ✅ 100% test pass rate maintained
- ✅ Throughput > 1000 req/s per implementation
- ✅ Worker processing < 100ms per event

### Reliability
- ✅ Durable queues (Redis Streams, PostgreSQL)
- ✅ Graceful degradation on worker failure
- ✅ Deduplication still works (async is acceptable)
- ✅ No data loss on crash

### Developer Experience
- ✅ Clear API response (202 Accepted)
- ✅ Tests still pass
- ✅ Easy to monitor (queue depth metrics)
- ✅ Simple rollback strategy

---

## Implementation Order

1. **Python First** (Fastest to implement - already has Redis Streams)
2. **Benchmark Python** (Validate < 10ms achieved)
3. **Elixir Second** (Broadway is powerful, proven pattern)
4. **Benchmark Elixir** (Compare with Python)
5. **Rust Third** (Most code to write)
6. **Benchmark Rust** (Final comparison)
7. **Update Test Suite** (Make compatible with all three)
8. **Final Benchmarks** (Three-way comparison)

---

## Next Steps

1. ✅ Design complete
2. 🔄 Implement Python async ingestion
3. ⏳ Implement Elixir Broadway pipeline
4. ⏳ Implement Rust queue processor
5. ⏳ Update test suite
6. ⏳ Run benchmarks
7. ⏳ Document results

---

**End of Implementation Plan**
