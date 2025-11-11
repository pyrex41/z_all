use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;
use uuid::Uuid;

#[derive(Debug, Clone, FromRow, Serialize, Deserialize)]
pub struct Event {
    pub id: Uuid,
    pub organization_id: Uuid,
    pub event_type: String,
    pub dedup_id: Option<String>,
    pub payload: serde_json::Value,
    pub created_at: DateTime<Utc>,
}
