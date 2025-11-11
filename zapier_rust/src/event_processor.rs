use sqlx::PgPool;
use tokio::sync::mpsc;
use uuid::Uuid;
use std::sync::Arc;

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

/// Result of event processing
#[derive(Debug)]
#[allow(dead_code)] // Will be used for async event processing responses
pub struct ProcessingResult {
    pub event_id: Uuid,
    pub success: bool,
    pub error: Option<String>,
}

/// Async event processor with channel-based queue
pub struct EventProcessor {
    sender: mpsc::Sender<EventToProcess>,
    max_capacity: usize,
}

impl EventProcessor {
    /// Create new event processor with specified queue capacity
    pub fn new(db: PgPool, queue_capacity: usize, num_workers: usize) -> Self {
        let (tx, rx) = mpsc::channel::<EventToProcess>(queue_capacity);
        let rx = Arc::new(tokio::sync::Mutex::new(rx));

        // Start worker pool
        for worker_id in 0..num_workers {
            let db = db.clone();
            let rx = Arc::clone(&rx);

            tokio::spawn(async move {
                tracing::info!("Event processor worker {} started", worker_id);

                loop {
                    let event = {
                        let mut receiver = rx.lock().await;
                        receiver.recv().await
                    };

                    match event {
                        Some(event) => {
                            if let Err(e) = process_event(&db, event.clone()).await {
                                // Record failed event metric
                                crate::metrics::record_event_processing_failure("database_error");

                                // Log to DLQ (dead letter queue via structured logging)
                                // This can be picked up by log aggregation systems
                                tracing::error!(
                                    worker_id = worker_id,
                                    error = %e,
                                    org_id = %event.organization_id,
                                    event_type = %event.event_type,
                                    dedup_id = ?event.dedup_id,
                                    dlq = "failed_event",
                                    "EVENT_PROCESSING_FAILED: Event moved to DLQ for manual intervention"
                                );

                                // TODO: Implement proper DLQ table or external queue (e.g., Redis, SQS)
                                // For now, failed events are logged and can be replayed manually
                            }
                        }
                        None => {
                            tracing::info!("Event processor worker {} stopped (channel closed)", worker_id);
                            break;
                        }
                    }
                }
            });
        }

        Self {
            sender: tx,
            max_capacity: queue_capacity,
        }
    }

    /// Queue an event for async processing (returns immediately)
    pub async fn queue_event(&self, event: EventToProcess) -> Result<(), String> {
        // Calculate used capacity (max - remaining)
        let remaining_capacity = self.sender.capacity();
        let used_capacity = self.max_capacity - remaining_capacity;

        // Update queue size metric with used capacity
        metrics::record_event_queue_size(used_capacity);

        self.sender
            .send(event)
            .await
            .map_err(|e| format!("Failed to queue event: {}", e))
    }

    /// Get current used queue size
    #[allow(dead_code)] // Will be used for monitoring and health checks
    pub fn queue_size(&self) -> usize {
        self.max_capacity - self.sender.capacity()
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
            ON CONFLICT (event_id) DO NOTHING
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
    // Queue capacity: 10,000 events
    // Workers: 4x CPU cores for I/O-bound work
    Arc::new(EventProcessor::new(db, 10_000, num_cpus * 4))
}
