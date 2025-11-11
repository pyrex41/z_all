use axum::{
    async_trait,
    extract::{FromRef, FromRequestParts},
    http::request::Parts,
};
use argon2::{Argon2, PasswordHasher};
use argon2::password_hash::SaltString;

use crate::{error::ApiError, models::Organization, state::AppState};
use std::sync::Arc;

pub struct AuthenticatedOrg {
    pub org: Organization,
}

#[async_trait]
impl<S> FromRequestParts<S> for AuthenticatedOrg
where
    S: Send + Sync,
    Arc<AppState>: FromRef<S>,
{
    type Rejection = ApiError;

    async fn from_request_parts(parts: &mut Parts, state: &S) -> Result<Self, Self::Rejection> {
        let auth_tracker = crate::metrics::track_authentication();
        let state = Arc::<AppState>::from_ref(state);

        // Extract API key from header
        let api_key = parts
            .headers
            .get("X-API-Key")
            .and_then(|v| v.to_str().ok())
            .ok_or(ApiError::Unauthorized)?;

        // Hash the provided API key
        let hashed_key = hash_api_key(api_key, &state.config.api_key_salt)?;

        // Try cache first
        if let Some(org) = state.auth_cache.get(&hashed_key).await {
            auth_tracker.record();
            return Ok(AuthenticatedOrg { org });
        }

        // Cache miss - fetch from database
        let org = sqlx::query_as::<_, Organization>(
            "SELECT * FROM organizations WHERE api_key_hash = $1"
        )
        .bind(&hashed_key)
        .fetch_optional(&state.db)
        .await?
        .ok_or(ApiError::Unauthorized)?;

        // Store in cache for future requests
        state.auth_cache.set(hashed_key, org.clone()).await;

        auth_tracker.record();
        Ok(AuthenticatedOrg { org })
    }
}

pub fn hash_api_key(api_key: &str, salt: &str) -> Result<String, ApiError> {
    let argon2 = Argon2::default();
    let salt_string = SaltString::from_b64(salt)
        .map_err(|e| ApiError::Internal(anyhow::anyhow!("Invalid salt: {}", e)))?;

    let hash = argon2
        .hash_password(api_key.as_bytes(), &salt_string)
        .map_err(|e| ApiError::Internal(anyhow::anyhow!("Failed to hash API key: {}", e)))?;

    Ok(hash.to_string())
}

pub fn generate_api_key() -> String {
    use rand::Rng;
    let mut rng = rand::thread_rng();
    let random_bytes: Vec<u8> = (0..32).map(|_| rng.gen()).collect();
    format!("zap_live_{}", hex::encode(random_bytes))
}
