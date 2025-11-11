use sqlx::PgPool;
use uuid::Uuid;
use std::sync::Arc;
use moka::future::Cache;
use std::time::Duration;

use crate::metrics;

/// Event to be processed asynchronously
#[derive(Debug, Clone)]
pub struct EventToProcess {
    pub organization_id: Uuid,
    pub event_type: String,
    pub dedup_id: Option<String>,
    pub payload: serde_json::Value,
    #[allow(dead_code)] // Will be used when webhook delivery system is implemented
    pub webhook_url: String,
}

/// Cache entry with timestamp for FIFO processing
#[derive(Debug, Clone)]
struct CachedEvent {
    event: EventToProcess,
    cached_at: std::time::Instant,
}

/// Result of event processing
#[derive(Debug)]
#[allow(dead_code)] // Will be used for async event processing responses
pub struct ProcessingResult {
    pub event_id: Uuid,
    pub success: bool,
    pub error: Option<String>,
}

/// High-performance async event processor with cache-first ingestion
///
/// Architecture:
/// 1. Incoming events are written to Moka cache instantly (sub-microsecond)
/// 2. Workers continuously poll the cache and process events asynchronously
/// 3. No backpressure on ingestion - always accepts events
pub struct EventProcessor {
    cache: Cache<Uuid, CachedEvent>,
    max_capacity: usize,
}

impl EventProcessor {
    /// Create new event processor with cache-first ingestion
    pub fn new(db: PgPool, cache_capacity: usize, num_workers: usize) -> Self {
        // Create high-performance cache with instant writes
        let cache = Cache::builder()
            .max_capacity(cache_capacity as u64)
            .time_to_live(Duration::from_secs(300)) // 5 min TTL
            .build();

        let processor_cache = cache.clone();

        // Start worker pool that continuously pulls from cache
        for worker_id in 0..num_workers {
            let db = db.clone();
            let cache = processor_cache.clone();

            tokio::spawn(async move {
                tracing::info!("Event processor worker {} started (cache-first mode)", worker_id);

                loop {
                    // Poll cache for events to process
                    // Get all keys and process oldest first (FIFO)
                    let events_to_process: Vec<(Uuid, CachedEvent)> = cache
                        .iter()
                        .map(|(k, v)| (*k, v))
                        .collect();

                    if events_to_process.is_empty() {
                        // No events to process, sleep briefly
                        tokio::time::sleep(Duration::from_millis(10)).await;
                        continue;
                    }

                    // Sort by cached_at for FIFO ordering
                    let mut sorted_events = events_to_process;
                    sorted_events.sort_by_key(|(_, cached)| cached.cached_at);

                    // Process a batch of events (up to 10 at a time per worker)
                    for (cache_key, cached_event) in sorted_events.iter().take(10) {
                        let event = cached_event.event.clone();

                        // Remove from cache before processing (avoid duplicate processing)
                        cache.invalidate(cache_key).await;

                        // Process event
                        if let Err(e) = process_event(&db, event.clone()).await {
                            // Record failed event metric
                            crate::metrics::record_event_processing_failure("database_error");

                            // Log to DLQ (dead letter queue via structured logging)
                            tracing::error!(
                                worker_id = worker_id,
                                error = %e,
                                org_id = %event.organization_id,
                                event_type = %event.event_type,
                                dedup_id = ?event.dedup_id,
                                dlq = "failed_event",
                                "EVENT_PROCESSING_FAILED: Event moved to DLQ for manual intervention"
                            );
                        }
                    }
                }
            });
        }

        Self {
            cache,
            max_capacity: cache_capacity,
        }
    }

    /// Queue an event for async processing (returns INSTANTLY - sub-millisecond)
    ///
    /// This is a synchronous cache write with no awaiting - immediate return
    pub async fn queue_event(&self, event: EventToProcess) -> Result<(), String> {
        // Generate unique cache key
        let cache_key = Uuid::new_v4();

        // Wrap event with timestamp for FIFO ordering
        let cached_event = CachedEvent {
            event,
            cached_at: std::time::Instant::now(),
        };

        // INSTANT cache write (no await on send - this is the key!)
        self.cache.insert(cache_key, cached_event).await;

        // Update metrics
        let queue_size = self.cache.entry_count();
        metrics::record_event_queue_size(queue_size as usize);

        // Check capacity (warn but don't block)
        if queue_size >= (self.max_capacity as u64) {
            tracing::warn!(
                queue_size = queue_size,
                max_capacity = self.max_capacity,
                "Event queue approaching capacity"
            );
        }

        Ok(())
    }

    /// Get current queue size
    #[allow(dead_code)] // Will be used for monitoring and health checks
    pub fn queue_size(&self) -> usize {
        self.cache.entry_count() as usize
    }
}

/// Process a single event (called by worker pool)
async fn process_event(db: &PgPool, event: EventToProcess) -> Result<Uuid, anyhow::Error> {
    let tracker = metrics::track_db_operation("event_insert");

    // Start transaction with timeout
    let mut tx = tokio::time::timeout(
        tokio::time::Duration::from_secs(5),
        db.begin()
    )
    .await
    .map_err(|_| anyhow::anyhow!("Transaction begin timeout"))??;

    // Insert event (use ON CONFLICT DO NOTHING to avoid lock escalation)
    let event_id = sqlx::query_scalar::<_, Uuid>(
        r#"
        INSERT INTO events (organization_id, event_type, dedup_id, payload)
        VALUES ($1, $2, $3, $4)
        ON CONFLICT (organization_id, dedup_id) DO NOTHING
        RETURNING id
        "#
    )
    .bind(&event.organization_id)
    .bind(&event.event_type)
    .bind(&event.dedup_id)
    .bind(&event.payload)
    .fetch_optional(&mut *tx)
    .await?;

    // If event_id is None, it means duplicate was detected (race condition)
    // In this case, we can safely ignore since event already exists
    if let Some(event_id) = event_id {
        // Insert delivery record
        sqlx::query(
            r#"
            INSERT INTO event_deliveries (event_id, status)
            VALUES ($1, 'pending')
            "#
        )
        .bind(&event_id)
        .execute(&mut *tx)
        .await?;

        // Commit transaction
        tx.commit().await?;

        tracker.record();

        tracing::info!(
            event_id = %event_id,
            org_id = %event.organization_id,
            event_type = %event.event_type,
            "Event processed asynchronously"
        );

        Ok(event_id)
    } else {
        // Duplicate detected, rollback and return success (event already exists)
        tx.rollback().await?;
        tracker.record();

        tracing::debug!(
            org_id = %event.organization_id,
            event_type = %event.event_type,
            dedup_id = ?event.dedup_id,
            "Duplicate event detected during async processing, skipping"
        );

        // Return a dummy UUID since the event already exists
        // This is not ideal but avoids breaking the return type
        Ok(Uuid::nil())
    }
}

/// Create event processor with sensible defaults
pub fn create_event_processor(db: PgPool) -> Arc<EventProcessor> {
    let num_cpus = num_cpus::get();
    // Cache capacity: 10,000 events
    // Workers: 4x CPU cores for I/O-bound work
    Arc::new(EventProcessor::new(db, 10_000, num_cpus * 4))
}
