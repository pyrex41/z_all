use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;
use uuid::Uuid;

#[derive(Debug, Clone, FromRow, Serialize, Deserialize)]
#[allow(dead_code)] // Will be used when webhook delivery system is implemented
pub struct EventDelivery {
    pub id: Uuid,
    pub event_id: Uuid,
    pub status: String,
    pub attempts: i32,
    pub last_attempt_at: Option<DateTime<Utc>>,
    pub response_status: Option<i32>,
    pub error_message: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, FromRow)]
pub struct PendingDelivery {
    pub delivery_id: Uuid,
    pub event_id: Uuid,
    pub event_type: String,
    pub payload: serde_json::Value,
    pub webhook_url: String,
    pub attempts: i32,
}
