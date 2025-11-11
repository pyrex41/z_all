use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde_json::json;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ApiError {
    #[error("Unauthorized")]
    Unauthorized,

    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Duplicate event")]
    DuplicateEvent,

    #[error("Payload too large")]
    PayloadTooLarge,

    #[error("Webhook not configured")]
    WebhookNotConfigured,

    #[error("Rate limit exceeded")]
    RateLimitExceeded,

    #[error("System capacity exceeded")]
    SystemCapacityExceeded,

    #[error("Event not found")]
    EventNotFound,

    #[error("Invalid request: {0}")]
    InvalidRequest(String),

    #[error("Internal error: {0}")]
    Internal(#[from] anyhow::Error),
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        let (status, error_message) = match self {
            ApiError::Unauthorized => (StatusCode::UNAUTHORIZED, "Unauthorized".to_string()),
            ApiError::Database(ref e) => {
                tracing::error!("Database error: {:?}", e);
                (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error".to_string())
            }
            ApiError::DuplicateEvent => (StatusCode::CONFLICT, "Duplicate event".to_string()),
            ApiError::PayloadTooLarge => (StatusCode::PAYLOAD_TOO_LARGE, "Payload too large (max 256KB)".to_string()),
            ApiError::WebhookNotConfigured => (StatusCode::BAD_REQUEST, "Webhook URL not configured".to_string()),
            ApiError::RateLimitExceeded => (StatusCode::TOO_MANY_REQUESTS, "Rate limit exceeded".to_string()),
            ApiError::SystemCapacityExceeded => (StatusCode::TOO_MANY_REQUESTS, "System capacity exceeded, please try again later".to_string()),
            ApiError::EventNotFound => (StatusCode::NOT_FOUND, "Event not found".to_string()),
            ApiError::InvalidRequest(ref msg) => (StatusCode::BAD_REQUEST, msg.clone()),
            ApiError::Internal(ref e) => {
                tracing::error!("Internal error: {:?}", e);
                (StatusCode::INTERNAL_SERVER_ERROR, "Internal server error".to_string())
            }
        };

        let body = Json(json!({
            "error": error_message,
        }));

        (status, body).into_response()
    }
}
