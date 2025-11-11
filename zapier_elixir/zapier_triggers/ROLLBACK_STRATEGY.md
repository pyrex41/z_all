# Rollback Strategy - Async Event Ingestion

## Overview

This document outlines the rollback strategy for the async event ingestion feature. The design ensures **zero data loss** during rollback scenarios.

---

## Quick Rollback

If you need to rollback immediately:

### Option 1: Stop Processing (Keep Queueing)

```bash
# Stop the EventQueueProcessor (events will queue but not process)
# In application.ex, comment out the processor:
# ZapierTriggers.Workers.EventQueueProcessor,

# Restart the application
mix phx.server
```

**Result**: Events continue to be accepted but queue up without processing. No data loss.

### Option 2: Drain Queue Before Rollback

```bash
# 1. Monitor queue depth
curl http://localhost:4000/health/ready

# 2. Wait for queue to drain (or process manually)
# Check event_queue table:
SELECT count(*) FROM event_queue WHERE status = 'pending';

# 3. Once empty, deploy rollback
```

**Result**: All queued events processed before rollback. No data loss.

---

## Rollback Scenarios

### Scenario 1: Performance Issues

**Symptoms**:
- High CPU usage
- Queue depth growing
- Slow HTTP responses

**Rollback Steps**:

1. **Immediate**: Reduce `max_concurrency` in config:
   ```elixir
   config :zapier_triggers, ZapierTriggers.Workers.EventQueueProcessor,
     max_concurrency: 5  # Reduce from 20
   ```

2. **If Still Issues**: Increase `min_poll_interval`:
   ```elixir
   config :zapier_triggers, ZapierTriggers.Workers.EventQueueProcessor,
     min_poll_interval: 500  # Slow down from 100ms
   ```

3. **Last Resort**: Stop processor, drain queue, investigate.

**Data Loss Risk**: None (events remain in queue)

---

### Scenario 2: Database Issues

**Symptoms**:
- Connection pool exhaustion
- Slow DB queries
- Transaction timeouts

**Rollback Steps**:

1. **Immediate**: Stop accepting new events (return 503):
   ```elixir
   # In event_controller.ex, add at top of create/2:
   conn
   |> put_status(:service_unavailable)
   |> json(%{error: "Service temporarily unavailable"})
   ```

2. **Drain**: Let processor finish existing queue items

3. **Investigate**: Check DB connection pool, slow queries

**Data Loss Risk**: None (events either queued or rejected)

---

### Scenario 3: Data Corruption/Bug

**Symptoms**:
- Events being processed incorrectly
- Duplicate deliveries
- Missing events

**Rollback Steps**:

1. **STOP IMMEDIATELY**: Kill the processor:
   ```bash
   # Emergency stop
   pkill -f "EventQueueProcessor"
   ```

2. **Assess Damage**:
   ```sql
   -- Check stuck items
   SELECT count(*), status FROM event_queue GROUP BY status;

   -- Check recent events
   SELECT * FROM events ORDER BY inserted_at DESC LIMIT 100;
   ```

3. **Fix Queue Items**:
   ```sql
   -- Revert stuck items to pending
   UPDATE event_queue SET status = 'pending' WHERE status = 'processing';

   -- Delete corrupted items (if identifiable)
   DELETE FROM event_queue WHERE id IN (...);
   ```

4. **Deploy Fix**: Fix bug, redeploy, resume processing

**Data Loss Risk**: Low (items remain in queue, can be manually recovered)

---

## Data Safety Guarantees

### Two-Phase Processing Prevents Loss

The current implementation uses a two-phase approach:

```elixir
# Phase 1: Mark as processing (item still in DB)
UPDATE event_queue SET status = 'processing' WHERE id = ?;

# Phase 2: Process event
INSERT INTO events (...);
INSERT INTO event_deliveries (...);

# Phase 3: Delete only on success
DELETE FROM event_queue WHERE id = ?;

# On Error: Revert to pending
UPDATE event_queue SET status = 'pending' WHERE id = ?;
```

**Guarantee**: Events are NEVER deleted from `event_queue` until successfully inserted into `events` table.

---

## Monitoring During Rollback

### Key Metrics to Watch

1. **Queue Depth**:
   ```sql
   SELECT count(*) FROM event_queue WHERE status = 'pending';
   ```
   - Normal: 0-100
   - Warning: 100-1000
   - Critical: >1000

2. **Stuck Items**:
   ```sql
   SELECT count(*) FROM event_queue WHERE status = 'processing'
     AND inserted_at < NOW() - INTERVAL '5 minutes';
   ```
   - Normal: 0
   - Warning: >0 (indicates processor crash)

3. **Processing Rate**:
   ```sql
   SELECT count(*) FROM events WHERE inserted_at > NOW() - INTERVAL '1 minute';
   ```
   - Normal: Matches ingestion rate
   - Warning: < Ingestion rate (queue growing)

---

## Rollback to Synchronous Processing

If you need to completely disable async processing:

### Step 1: Create Synchronous Endpoint

```elixir
# In event_controller.ex, add new function:
def create_sync(conn, params) do
  # Direct DB insert (no queue)
  organization = conn.assigns.current_organization

  case create_event_synchronously(organization, params) do
    {:ok, event} ->
      conn
      |> put_status(:created)
      |> json(%{id: event.id, status: "created"})

    {:error, reason} ->
      conn
      |> put_status(:unprocessable_entity)
      |> json(%{error: reason})
  end
end
```

### Step 2: Route Traffic

```elixir
# In router.ex, temporarily change route:
# post "/events", EventController, :create      # Async (comment out)
post "/events", EventController, :create_sync  # Sync (enable)
```

### Step 3: Drain Queue

```bash
# Let EventQueueProcessor finish existing items
# Monitor queue depth until 0
```

### Step 4: Stop Processor

```elixir
# In application.ex, comment out:
# ZapierTriggers.Workers.EventQueueProcessor,
```

**Result**: System reverted to synchronous processing. Slower but predictable.

---

## Recovery Procedures

### Recovering from Processor Crash

```elixir
# 1. Check for stuck items
EventQueueProcessor.cleanup_stuck_items()

# 2. Manually trigger processing if needed
GenServer.cast(EventQueueProcessor, :poll)
```

### Recovering Lost Events (Manual)

If events are somehow lost (should never happen with two-phase approach):

```sql
-- Find missing events (in queue but not in events table)
SELECT q.* FROM event_queue q
LEFT JOIN events e ON q.id = e.id
WHERE e.id IS NULL;

-- Manually requeue them
UPDATE event_queue SET status = 'pending' WHERE id IN (...);
```

---

## Testing Rollback Procedures

Before deploying to production, test rollback:

### Test 1: Stop Processor Mid-Flight

```bash
# 1. Queue 100 events
for i in {1..100}; do
  curl -X POST http://localhost:4000/api/events ...
done

# 2. Kill processor
pkill -f "EventQueueProcessor"

# 3. Verify events remain in queue
psql -c "SELECT count(*) FROM event_queue;"

# 4. Restart processor
mix phx.server

# 5. Verify events processed
psql -c "SELECT count(*) FROM events;"
```

**Expected**: All 100 events processed, no loss.

### Test 2: Revert to Synchronous

```bash
# 1. Switch to sync endpoint
# 2. Send events
# 3. Verify direct insertion (no queue)
# 4. Switch back to async
# 5. Verify queue processing resumes
```

---

## Feature Flags (Future Enhancement)

For more granular control, consider adding feature flag:

```elixir
# config/config.exs
config :zapier_triggers, :async_ingestion_enabled, true

# event_controller.ex
def create(conn, params) do
  if Application.get_env(:zapier_triggers, :async_ingestion_enabled, true) do
    create_async(conn, params)
  else
    create_sync(conn, params)
  end
end
```

**Benefit**: Toggle async/sync at runtime without code changes.

---

## Support Checklist

Before escalating rollback:

- [ ] Checked queue depth (`SELECT count(*) FROM event_queue`)
- [ ] Checked stuck items (`WHERE status = 'processing'`)
- [ ] Reviewed logs for errors
- [ ] Verified DB connection pool availability
- [ ] Checked Oban job queue health
- [ ] Monitored CPU/memory usage
- [ ] Tested with reduced `max_concurrency`

---

## Emergency Contacts

If rollback required:

1. **Check queue status** (`/health/ready` endpoint)
2. **Stop processor** (comment out in `application.ex`)
3. **Drain queue** (wait or manual processing)
4. **Investigate root cause**
5. **Deploy fix**
6. **Resume processing**

**Remember**: With two-phase processing, **no data is lost** during rollback. Events remain in `event_queue` until successfully persisted.

---

## Conclusion

The async event ingestion system is designed with **safety-first**:
- ✅ Two-phase processing prevents data loss
- ✅ Stuck item cleanup handles crashes
- ✅ Queue remains intact during rollback
- ✅ Can revert to sync processing if needed
- ✅ Full audit trail in database

**Rollback Risk**: Low
**Data Loss Risk**: None (with proper procedures)
**Recovery Time**: Minutes (drain queue + redeploy)
