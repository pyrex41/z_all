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
    pub webhook_url: String,
}

/// Result of event processing
#[derive(Debug)]
pub struct ProcessingResult {
    pub event_id: Uuid,
    pub success: bool,
    pub error: Option<String>,
}

/// Async event processor with channel-based queue
pub struct EventProcessor {
    sender: mpsc::Sender<EventToProcess>,
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
                            if let Err(e) = process_event(&db, event).await {
                                tracing::error!(worker_id = worker_id, error = %e, "Failed to process event");
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

        Self { sender: tx }
    }

    /// Queue an event for async processing (returns immediately)
    pub async fn queue_event(&self, event: EventToProcess) -> Result<(), String> {
        // Update queue size metric
        metrics::record_event_queue_size(self.sender.capacity());

        self.sender
            .send(event)
            .await
            .map_err(|e| format!("Failed to queue event: {}", e))
    }

    /// Get current queue size
    pub fn queue_size(&self) -> usize {
        self.sender.capacity()
    }
}

/// Process a single event (called by worker pool)
async fn process_event(db: &PgPool, event: EventToProcess) -> Result<Uuid, anyhow::Error> {
    let tracker = metrics::track_db_operation("event_insert");

    // Start transaction
    let mut tx = db.begin().await?;

    // Insert event
    let event_id = sqlx::query_scalar::<_, Uuid>(
        r#"
        INSERT INTO events (organization_id, event_type, dedup_id, payload)
        VALUES ($1, $2, $3, $4)
        ON CONFLICT (organization_id, dedup_id)
        DO UPDATE SET organization_id = EXCLUDED.organization_id
        RETURNING id
        "#
    )
    .bind(&event.organization_id)
    .bind(&event.event_type)
    .bind(&event.dedup_id)
    .bind(&event.payload)
    .fetch_one(&mut *tx)
    .await?;

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
}

/// Create event processor with sensible defaults
pub fn create_event_processor(db: PgPool) -> Arc<EventProcessor> {
    let num_cpus = num_cpus::get();
    // Queue capacity: 10,000 events
    // Workers: 4x CPU cores for I/O-bound work
    Arc::new(EventProcessor::new(db, 10_000, num_cpus * 4))
}
