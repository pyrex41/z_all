use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::{DateTime, Utc, Duration};
use uuid::Uuid;

use crate::{models::Organization, metrics};

/// Cached authentication entry
#[derive(Clone, Debug)]
struct CacheEntry {
    org: Organization,
    expires_at: DateTime<Utc>,
}

/// In-memory authentication cache with TTL
pub struct AuthCache {
    cache: RwLock<HashMap<String, CacheEntry>>,
    ttl_seconds: i64,
}

impl AuthCache {
    /// Create new auth cache with specified TTL
    pub fn new(ttl_seconds: i64) -> Self {
        Self {
            cache: RwLock::new(HashMap::new()),
            ttl_seconds,
        }
    }

    /// Get organization from cache by hashed API key
    pub async fn get(&self, hashed_key: &str) -> Option<Organization> {
        let cache = self.cache.read().await;

        if let Some(entry) = cache.get(hashed_key) {
            if entry.expires_at > Utc::now() {
                metrics::record_cache_hit(true);
                return Some(entry.org.clone());
            }
        }

        metrics::record_cache_hit(false);
        None
    }

    /// Store organization in cache
    pub async fn set(&self, hashed_key: String, org: Organization) {
        let expires_at = Utc::now() + Duration::seconds(self.ttl_seconds);
        let entry = CacheEntry { org, expires_at };

        let mut cache = self.cache.write().await;
        cache.insert(hashed_key, entry);
    }

    /// Invalidate cache entry for a specific org (used when API key rotates)
    pub async fn invalidate_org(&self, org_id: &Uuid) {
        let mut cache = self.cache.write().await;
        cache.retain(|_, entry| entry.org.id != *org_id);
    }

    /// Cleanup expired entries
    pub async fn cleanup_expired(&self) {
        let mut cache = self.cache.write().await;
        let now = Utc::now();
        cache.retain(|_, entry| entry.expires_at > now);
    }

    /// Get cache statistics
    pub async fn stats(&self) -> CacheStats {
        let cache = self.cache.read().await;
        let now = Utc::now();

        let total = cache.len();
        let expired = cache.values().filter(|e| e.expires_at <= now).count();

        CacheStats {
            total_entries: total,
            expired_entries: expired,
            active_entries: total - expired,
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub total_entries: usize,
    pub expired_entries: usize,
    pub active_entries: usize,
}

/// Create auth cache with sensible defaults (5 minute TTL)
pub fn create_auth_cache() -> Arc<AuthCache> {
    Arc::new(AuthCache::new(300))
}

/// Start background cleanup task for auth cache
pub fn start_cache_cleanup_task(cache: Arc<AuthCache>) {
    tokio::spawn(async move {
        let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(60));

        loop {
            interval.tick().await;
            cache.cleanup_expired().await;

            let stats = cache.stats().await;
            tracing::debug!(
                total = stats.total_entries,
                active = stats.active_entries,
                expired = stats.expired_entries,
                "Auth cache cleanup completed"
            );
        }
    });
}
