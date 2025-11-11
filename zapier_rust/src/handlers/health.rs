use axum::{http::StatusCode, Json};
use serde_json::{json, Value};

pub async fn health_check() -> (StatusCode, Json<Value>) {
    (StatusCode::OK, Json(json!({"status": "healthy"})))
}

pub async fn metrics_handler() -> String {
    // Placeholder - will be replaced with actual Prometheus metrics
    "# Metrics endpoint\n".to_string()
}
