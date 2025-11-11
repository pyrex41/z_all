use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    Json,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

use crate::{
    error::ApiError,
    middleware::AuthenticatedOrg,
    models::Event,
    state::AppState,
};

// ===== POST /api/events =====

#[derive(Debug, Deserialize)]
pub struct CreateEventRequest {
    #[serde(rename = "type")]
    pub event_type: String,
    pub dedup_id: Option<String>,
    pub payload: serde_json::Value,
}

#[derive(Debug, Serialize)]
pub struct CreateEventResponse {
    pub id: Uuid,
    pub status: String,
}

pub async fn create_event(
    State(state): State<Arc<AppState>>,
    auth: AuthenticatedOrg,
    Json(req): Json<CreateEventRequest>,
) -> Result<(StatusCode, Json<CreateEventResponse>), ApiError> {
    let ingestion_tracker = crate::metrics::track_event_ingestion();

    // Rate limiting
    let rate_tracker = crate::metrics::track_rate_limit_check();
    state.rate_limiter.check(&auth.org.id, auth.org.rate_limit_per_minute).await?;
    rate_tracker.record();

    // Payload size check (256KB)
    let payload_size = serde_json::to_vec(&req.payload)
        .map_err(|e| {
            crate::metrics::record_validation_error("invalid_json");
            ApiError::InvalidRequest(format!("Invalid JSON: {}", e))
        })?
        .len();

    if payload_size > 256 * 1024 {
        crate::metrics::record_validation_error("payload_too_large");
        return Err(ApiError::PayloadTooLarge);
    }

    // Validate webhook configured
    let webhook_url = auth.org.webhook_url.as_ref()
        .ok_or_else(|| {
            crate::metrics::record_validation_error("webhook_not_configured");
            ApiError::WebhookNotConfigured
        })?
        .clone();

    // Fast deduplication check (if dedup_id provided)
    if let Some(dedup_id) = &req.dedup_id {
        let dedup_tracker = crate::metrics::track_db_operation("dedup_check");
        let exists = sqlx::query_scalar::<_, bool>(
            "SELECT EXISTS(SELECT 1 FROM events WHERE organization_id = $1 AND dedup_id = $2)"
        )
        .bind(&auth.org.id)
        .bind(dedup_id)
        .fetch_one(&state.db)
        .await?;
        dedup_tracker.record();

        if exists {
            crate::metrics::record_validation_error("duplicate_event");
            return Err(ApiError::DuplicateEvent);
        }
    }

    // Queue event for async processing (returns immediately)
    let event_to_process = crate::event_processor::EventToProcess {
        organization_id: auth.org.id,
        event_type: req.event_type.clone(),
        dedup_id: req.dedup_id.clone(),
        payload: req.payload,
        webhook_url,
    };

    state.event_processor.queue_event(event_to_process).await
        .map_err(|_| ApiError::SystemCapacityExceeded)?;

    ingestion_tracker.record();

    // Generate tracking ID for immediate response
    // Note: Actual event ID will be assigned during async processing
    let tracking_id = Uuid::new_v4();

    tracing::info!(
        tracking_id = %tracking_id,
        org_id = %auth.org.id,
        event_type = %req.event_type,
        "Event accepted and queued for processing"
    );

    // Return immediately with 202 Accepted (not waiting for DB or webhook)
    Ok((
        StatusCode::ACCEPTED,
        Json(CreateEventResponse {
            id: tracking_id,
            status: "accepted".to_string(),
        }),
    ))
}

// ===== GET /api/inbox =====

#[derive(Debug, Deserialize)]
pub struct InboxQuery {
    #[serde(default = "default_limit")]
    pub limit: i64,
    pub status: Option<String>,
    pub cursor: Option<Uuid>,
}

fn default_limit() -> i64 {
    100
}

#[derive(Debug, Serialize)]
pub struct InboxResponse {
    pub events: Vec<EventWithDelivery>,
    pub next_cursor: Option<Uuid>,
}

#[derive(Debug, Serialize)]
pub struct EventWithDelivery {
    pub id: Uuid,
    pub event_type: String,
    pub payload: serde_json::Value,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub delivery_status: String,
    pub attempts: i32,
}

pub async fn list_inbox(
    State(state): State<Arc<AppState>>,
    auth: AuthenticatedOrg,
    Query(query): Query<InboxQuery>,
) -> Result<Json<InboxResponse>, ApiError> {
    // Validate limit
    let limit = query.limit.min(1000).max(1);

    let mut sql = r#"
        SELECT
            e.id,
            e.event_type,
            e.payload,
            e.created_at,
            ed.status as delivery_status,
            ed.attempts
        FROM events e
        JOIN event_deliveries ed ON ed.event_id = e.id
        WHERE e.organization_id = $1
    "#.to_string();

    // Add status filter
    if let Some(status) = &query.status {
        sql.push_str(&format!(" AND ed.status = '{}'", status));
    }

    // Add cursor
    if let Some(cursor) = query.cursor {
        sql.push_str(&format!(" AND e.id < '{}'", cursor));
    }

    sql.push_str(" ORDER BY e.created_at DESC LIMIT $2");

    let events = sqlx::query_as::<_, EventWithDelivery>(&sql)
        .bind(&auth.org.id)
        .bind(limit + 1) // Fetch one extra to determine if there's a next page
        .fetch_all(&state.db)
        .await?;

    let has_more = events.len() as i64 > limit;
    let mut events = events;
    let next_cursor = if has_more {
        events.pop().map(|e| e.id)
    } else {
        None
    };

    Ok(Json(InboxResponse {
        events,
        next_cursor,
    }))
}

// ===== POST /api/ack/:event_id =====

#[derive(Debug, Serialize)]
pub struct AckResponse {
    pub success: bool,
}

pub async fn acknowledge_event(
    State(state): State<Arc<AppState>>,
    auth: AuthenticatedOrg,
    Path(event_id): Path<Uuid>,
) -> Result<Json<AckResponse>, ApiError> {
    // Verify event belongs to organization
    let _event = sqlx::query_as::<_, Event>(
        "SELECT * FROM events WHERE id = $1 AND organization_id = $2"
    )
    .bind(&event_id)
    .bind(&auth.org.id)
    .fetch_optional(&state.db)
    .await?
    .ok_or(ApiError::EventNotFound)?;

    // Update delivery status
    sqlx::query(
        r#"
        UPDATE event_deliveries
        SET status = 'delivered', updated_at = NOW()
        WHERE event_id = $1
        "#
    )
    .bind(&event_id)
    .execute(&state.db)
    .await?;

    tracing::info!(
        event_id = %event_id,
        org_id = %auth.org.id,
        "Event acknowledged"
    );

    Ok(Json(AckResponse { success: true }))
}

// ===== POST /api/webhook/config =====

#[derive(Debug, Deserialize)]
pub struct WebhookConfigRequest {
    pub webhook_url: String,
}

#[derive(Debug, Serialize)]
pub struct WebhookConfigResponse {
    pub success: bool,
}

pub async fn configure_webhook(
    State(state): State<Arc<AppState>>,
    auth: AuthenticatedOrg,
    Json(req): Json<WebhookConfigRequest>,
) -> Result<Json<WebhookConfigResponse>, ApiError> {
    // Validate URL
    if !req.webhook_url.starts_with("http://") && !req.webhook_url.starts_with("https://") {
        return Err(ApiError::InvalidRequest("Invalid webhook URL".to_string()));
    }

    sqlx::query(
        "UPDATE organizations SET webhook_url = $1, updated_at = NOW() WHERE id = $2"
    )
    .bind(&req.webhook_url)
    .bind(&auth.org.id)
    .execute(&state.db)
    .await?;

    // Invalidate auth cache for this organization to ensure fresh data
    state.auth_cache.invalidate_org(&auth.org.id).await;

    tracing::info!(
        org_id = %auth.org.id,
        webhook_url = %req.webhook_url,
        "Webhook configured and cache invalidated"
    );

    Ok(Json(WebhookConfigResponse { success: true }))
}

// Implement FromRow for EventWithDelivery
impl sqlx::FromRow<'_, sqlx::postgres::PgRow> for EventWithDelivery {
    fn from_row(row: &sqlx::postgres::PgRow) -> Result<Self, sqlx::Error> {
        use sqlx::Row;
        Ok(EventWithDelivery {
            id: row.try_get("id")?,
            event_type: row.try_get("event_type")?,
            payload: row.try_get("payload")?,
            created_at: row.try_get("created_at")?,
            delivery_status: row.try_get("delivery_status")?,
            attempts: row.try_get("attempts")?,
        })
    }
}
