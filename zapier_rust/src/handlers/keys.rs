use axum::{extract::State, http::StatusCode, Json};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

use crate::{
    error::ApiError,
    middleware::{auth::{generate_api_key, hash_api_key}, AuthenticatedOrg},
    state::AppState,
};

// ===== POST /api/keys/generate =====

#[derive(Debug, Deserialize)]
pub struct GenerateKeyRequest {
    #[serde(alias = "org_name")]
    pub organization_name: String,
    #[serde(default = "default_tier")]
    pub tier: String,
}

fn default_tier() -> String {
    "free".to_string()
}

#[derive(Debug, Serialize)]
pub struct GenerateKeyResponse {
    pub api_key: String,
    pub organization_id: Uuid,
    pub organization_name: String,
    pub tier: String,
}

pub async fn generate_key(
    State(state): State<Arc<AppState>>,
    Json(req): Json<GenerateKeyRequest>,
) -> Result<(StatusCode, Json<GenerateKeyResponse>), ApiError> {
    // Generate API key
    let api_key = generate_api_key();
    let api_key_hash = hash_api_key(&api_key, &state.config.api_key_salt)?;

    // Determine rate limit based on tier
    let rate_limit = match req.tier.as_str() {
        "free" => 100,
        "pro" => 1000,
        "business" => 10000,
        "enterprise" => 100000,
        _ => 100,
    };

    // Insert organization
    let org_id = sqlx::query_scalar::<_, Uuid>(
        r#"
        INSERT INTO organizations (name, api_key_hash, tier, rate_limit_per_minute)
        VALUES ($1, $2, $3, $4)
        RETURNING id
        "#
    )
    .bind(&req.organization_name)
    .bind(&api_key_hash)
    .bind(&req.tier)
    .bind(rate_limit)
    .fetch_one(&state.db)
    .await?;

    tracing::info!(
        org_id = %org_id,
        org_name = %req.organization_name,
        tier = %req.tier,
        "API key generated"
    );

    Ok((
        StatusCode::CREATED,
        Json(GenerateKeyResponse {
            api_key,
            organization_id: org_id,
            organization_name: req.organization_name.clone(),
            tier: req.tier.clone(),
        }),
    ))
}

// ===== GET /api/keys =====

#[derive(Debug, Serialize)]
pub struct KeyInfoResponse {
    pub organization_id: Uuid,
    pub organization_name: String,
    pub tier: String,
    pub rate_limit_per_minute: i32,
    pub webhook_url: Option<String>,
}

pub async fn get_key_info(
    State(_state): State<Arc<AppState>>,
    auth: AuthenticatedOrg,
) -> Result<Json<KeyInfoResponse>, ApiError> {
    Ok(Json(KeyInfoResponse {
        organization_id: auth.org.id,
        organization_name: auth.org.name,
        tier: auth.org.tier,
        rate_limit_per_minute: auth.org.rate_limit_per_minute,
        webhook_url: auth.org.webhook_url,
    }))
}

// ===== POST /api/keys/rotate =====

#[derive(Debug, Serialize)]
pub struct RotateKeyResponse {
    pub api_key: String,
}

pub async fn rotate_key(
    State(state): State<Arc<AppState>>,
    auth: AuthenticatedOrg,
) -> Result<Json<RotateKeyResponse>, ApiError> {
    // Generate new API key
    let new_api_key = generate_api_key();
    let new_api_key_hash = hash_api_key(&new_api_key, &state.config.api_key_salt)?;

    // Update organization
    sqlx::query(
        "UPDATE organizations SET api_key_hash = $1, updated_at = NOW() WHERE id = $2"
    )
    .bind(&new_api_key_hash)
    .bind(&auth.org.id)
    .execute(&state.db)
    .await?;

    tracing::info!(
        org_id = %auth.org.id,
        "API key rotated"
    );

    Ok(Json(RotateKeyResponse {
        api_key: new_api_key,
    }))
}
