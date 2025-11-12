use sqlx::PgPool;
use std::sync::Arc;
use dashmap::DashMap;
use chrono::{DateTime, Utc};

use crate::config::Config;
use crate::event_processor::EventProcessor;
use crate::auth_cache::AuthCache;

#[derive(Clone)]
pub struct AppState {
    pub db: PgPool,
    pub config: Config,
    pub rate_limiter: Arc<RateLimiter>,
    pub event_processor: Arc<EventProcessor>,
    pub auth_cache: Arc<AuthCache>,
}

impl AppState {
    pub fn new(
        db: PgPool,
        config: Config,
        event_processor: Arc<EventProcessor>,
        auth_cache: Arc<AuthCache>,
    ) -> Self {
        Self {
            db,
            config,
            rate_limiter: Arc::new(RateLimiter::new()),
            event_processor,
            auth_cache,
        }
    }
}

// Lock-free concurrent rate limiter using DashMap
pub struct RateLimiter {
    limits: DashMap<uuid::Uuid, RateLimitEntry>,
}

struct RateLimitEntry {
    count: u32,
    window_start: DateTime<Utc>,
}

impl RateLimiter {
    pub fn new() -> Self {
        Self {
            limits: DashMap::new(),
        }
    }

    pub fn check(&self, org_id: &uuid::Uuid, limit: i32) -> Result<(), crate::error::ApiError> {
        let now = Utc::now();

        // OPTIMIZED: Single atomic entry operation (no double lookup!)
        let mut entry = self.limits
            .entry(*org_id)
            .or_insert_with(|| RateLimitEntry {
                count: 0,
                window_start: now,
            });

        // Reset window if more than 1 minute has passed
        if (now - entry.window_start).num_seconds() >= 60 {
            entry.count = 0;
            entry.window_start = now;
        }

        // Check and increment in one operation (still holding the lock)
        if entry.count >= limit as u32 {
            return Err(crate::error::ApiError::RateLimitExceeded);
        }

        entry.count += 1;
        Ok(())
    }
}
