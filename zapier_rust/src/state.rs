use sqlx::PgPool;
use std::sync::Arc;
use tokio::sync::RwLock;
use std::collections::HashMap;
use chrono::{DateTime, Utc};

use crate::config::Config;

#[derive(Clone)]
pub struct AppState {
    pub db: PgPool,
    pub config: Config,
    pub rate_limiter: Arc<RateLimiter>,
}

impl AppState {
    pub fn new(db: PgPool, config: Config) -> Self {
        Self {
            db,
            config,
            rate_limiter: Arc::new(RateLimiter::new()),
        }
    }
}

// Simple in-memory rate limiter
pub struct RateLimiter {
    limits: RwLock<HashMap<uuid::Uuid, RateLimitEntry>>,
}

struct RateLimitEntry {
    count: u32,
    window_start: DateTime<Utc>,
}

impl RateLimiter {
    pub fn new() -> Self {
        Self {
            limits: RwLock::new(HashMap::new()),
        }
    }

    pub async fn check(&self, org_id: &uuid::Uuid, limit: i32) -> Result<(), crate::error::ApiError> {
        let mut limits = self.limits.write().await;
        let now = Utc::now();

        let entry = limits.entry(*org_id).or_insert(RateLimitEntry {
            count: 0,
            window_start: now,
        });

        // Reset window if more than 1 minute has passed
        if (now - entry.window_start).num_seconds() >= 60 {
            entry.count = 0;
            entry.window_start = now;
        }

        if entry.count >= limit as u32 {
            return Err(crate::error::ApiError::RateLimitExceeded);
        }

        entry.count += 1;
        Ok(())
    }
}
