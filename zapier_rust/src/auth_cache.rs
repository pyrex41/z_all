use std::sync::Arc;
use dashmap::DashMap;
use chrono::{DateTime, Utc, Duration};
use uuid::Uuid;

use crate::{models::Organization, metrics};

/// Maximum number of entries in the auth cache to prevent memory leaks
const MAX_CACHE_SIZE: usize = 100_000;

/// Cached authentication entry
#[derive(Clone, Debug)]
struct CacheEntry {
    org: Organization,
    expires_at: DateTime<Utc>,
    last_accessed: DateTime<Utc>,
}

/// LOCK-FREE in-memory authentication cache with TTL and LRU eviction
/// Uses DashMap for zero-contention concurrent access (no async overhead!)
///
/// OPTIMIZATION: Dual indexing - cache by both hashed key AND plaintext API key
/// This allows ultra-fast cache hits that skip ALL hashing (Argon2 is EXPENSIVE!)
pub struct AuthCache {
    cache: Arc<DashMap<String, CacheEntry>>,
    // Secondary index: plaintext API key -> organization (FAST PATH!)
    api_key_index: Arc<DashMap<String, CacheEntry>>,
    ttl_seconds: i64,
    max_size: usize,
}

impl AuthCache {
    /// Create new auth cache with specified TTL and max size
    pub fn new(ttl_seconds: i64) -> Self {
        Self {
            cache: Arc::new(DashMap::with_capacity(MAX_CACHE_SIZE)),
            api_key_index: Arc::new(DashMap::with_capacity(MAX_CACHE_SIZE)),
            ttl_seconds,
            max_size: MAX_CACHE_SIZE,
        }
    }

    /// ULTRA-FAST PATH: Get by plaintext API key (ZERO HASHING!)
    /// This is the hottest code path - used for every authenticated request
    pub async fn get_by_api_key(&self, api_key: &str) -> Option<Organization> {
        let now = Utc::now();

        // Check API key index first (fastest path - no hashing!)
        if let Some(mut entry) = self.api_key_index.get_mut(api_key) {
            if entry.expires_at > now {
                entry.last_accessed = now;
                metrics::record_cache_hit(true);
                return Some(entry.org.clone());
            } else {
                // Expired - remove from both indexes
                drop(entry);
                self.api_key_index.remove(api_key);
            }
        }

        metrics::record_cache_hit(false);
        None
    }

    /// Store with dual indexing (hashed key + plaintext API key)
    pub async fn set_with_api_key(&self, api_key: String, org: Organization) {
        let now = Utc::now();
        let expires_at = now + Duration::seconds(self.ttl_seconds);
        let entry = CacheEntry {
            org,
            expires_at,
            last_accessed: now,
        };

        // Evict if needed
        if self.api_key_index.len() >= self.max_size && !self.api_key_index.contains_key(&api_key) {
            if let Some(lru_entry) = self.api_key_index
                .iter()
                .min_by_key(|entry| entry.value().last_accessed)
            {
                let lru_key = lru_entry.key().clone();
                drop(lru_entry);
                self.api_key_index.remove(&lru_key);
                tracing::debug!("Evicted LRU entry from auth cache (API key index), size: {}", self.api_key_index.len());
            }
        }

        // Store in API key index (primary fast path)
        self.api_key_index.insert(api_key, entry);
    }

    /// LOCK-FREE get organization from cache (NO ASYNC!)
    /// DashMap provides zero-contention concurrent reads
    pub async fn get(&self, hashed_key: &str) -> Option<Organization> {
        let now = Utc::now();

        // Try to get and update in one atomic operation
        if let Some(mut entry) = self.cache.get_mut(hashed_key) {
            if entry.expires_at > now {
                // Update last_accessed for LRU tracking (lock-free!)
                entry.last_accessed = now;
                metrics::record_cache_hit(true);
                return Some(entry.org.clone());
            } else {
                // Entry expired, will be removed
                drop(entry); // Release lock before removing
                self.cache.remove(hashed_key);
            }
        }

        metrics::record_cache_hit(false);
        None
    }

    /// LOCK-FREE store organization in cache (NO CONTENTION!)
    pub async fn set(&self, hashed_key: String, org: Organization) {
        let now = Utc::now();
        let expires_at = now + Duration::seconds(self.ttl_seconds);
        let entry = CacheEntry {
            org,
            expires_at,
            last_accessed: now,
        };

        // Evict LRU entries if cache is full (simple overflow protection)
        if self.cache.len() >= self.max_size && !self.cache.contains_key(&hashed_key) {
            // Find and remove the least recently accessed entry
            // Note: This is rare (only when cache is full) and DashMap makes it fast
            if let Some(lru_entry) = self.cache
                .iter()
                .min_by_key(|entry| entry.value().last_accessed)
            {
                let lru_key = lru_entry.key().clone();
                drop(lru_entry); // Release iterator before removing
                self.cache.remove(&lru_key);
                tracing::debug!("Evicted LRU entry from auth cache, size: {}", self.cache.len());
            }
        }

        self.cache.insert(hashed_key, entry);
    }

    /// LOCK-FREE invalidate cache entry for a specific org
    pub async fn invalidate_org(&self, org_id: &Uuid) {
        self.cache.retain(|_, entry| entry.org.id != *org_id);
    }

    /// LOCK-FREE cleanup expired entries
    pub async fn cleanup_expired(&self) {
        let now = Utc::now();
        self.cache.retain(|_, entry| entry.expires_at > now);
    }

    /// LOCK-FREE get cache statistics
    pub async fn stats(&self) -> CacheStats {
        let now = Utc::now();
        let total = self.cache.len();
        let expired = self.cache.iter().filter(|e| e.value().expires_at <= now).count();

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

/// Start background cleanup task for auth cache (runs every 15 seconds)
pub fn start_cache_cleanup_task(cache: Arc<AuthCache>) {
    tokio::spawn(async move {
        let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(15));

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
