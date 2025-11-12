use axum::{
    async_trait,
    extract::{FromRef, FromRequestParts},
    http::request::Parts,
};
use argon2::{Argon2, PasswordHasher};
use argon2::password_hash::SaltString;
use dashmap::DashMap;
use std::sync::OnceLock;

use crate::{error::ApiError, models::Organization, state::AppState};
use std::sync::Arc;

// PERFORMANCE OPTIMIZATION: Cache hashed API keys to avoid expensive Argon2 on every request
// Since API keys rarely change, this cache can be huge and long-lived
static HASH_CACHE: OnceLock<DashMap<String, String>> = OnceLock::new();

fn get_hash_cache() -> &'static DashMap<String, String> {
    HASH_CACHE.get_or_init(|| DashMap::with_capacity(10_000))
}

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

        // ULTRA-FAST PATH: Check if we have this API key cached (skips ALL hashing!)
        // Cache key is the plaintext API key (safe since cache is in-memory only)
        if let Some(org) = state.auth_cache.get_by_api_key(api_key).await {
            auth_tracker.record();
            return Ok(AuthenticatedOrg { org });
        }

        // Cache miss - need to hash and check database
        // OPTIMIZATION: Check hash cache first (avoids expensive Argon2 on repeat misses)
        let hash_cache = get_hash_cache();
        let hashed_key = if let Some(cached_hash) = hash_cache.get(api_key) {
            cached_hash.value().clone()
        } else {
            // Hash cache miss - compute hash and store for next time
            let computed_hash = hash_api_key(api_key, &state.config.api_key_salt)?;
            hash_cache.insert(api_key.to_string(), computed_hash.clone());
            computed_hash
        };

        // Fetch from database
        let org = sqlx::query_as::<_, Organization>(
            "SELECT * FROM organizations WHERE api_key_hash = $1"
        )
        .bind(&hashed_key)
        .fetch_optional(&state.db)
        .await?
        .ok_or(ApiError::Unauthorized)?;

        // Store in cache with both the hashed key AND the plaintext key for fast lookup
        state.auth_cache.set_with_api_key(api_key.to_string(), org.clone()).await;

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
