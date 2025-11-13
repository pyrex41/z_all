use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct Config {
    #[serde(default = "default_database_url")]
    pub database_url: String,

    #[serde(default = "default_port")]
    pub port: u16,

    #[serde(default = "default_host")]
    pub host: String,

    #[serde(default = "default_api_key_salt")]
    pub api_key_salt: String,

    #[serde(default = "default_webhook_secret")]
    #[allow(dead_code)] // Will be used for webhook signature verification
    pub webhook_secret: String,

    #[serde(default = "default_max_connections")]
    pub max_db_connections: u32,

    #[serde(default = "default_worker_count")]
    #[allow(dead_code)] // Will be used when delivery system is implemented
    pub delivery_worker_count: usize,

    #[serde(default = "default_disable_webhook_delivery")]
    pub disable_webhook_delivery: bool,

    #[serde(default = "default_metrics_port")]
    pub metrics_port: u16,
}

fn default_database_url() -> String {
    std::env::var("DATABASE_URL")
        .unwrap_or_else(|_| "postgres://localhost/zapier_triggers".to_string())
}

fn default_port() -> u16 {
    std::env::var("PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(8080)
}

fn default_host() -> String {
    std::env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string())
}

fn default_api_key_salt() -> String {
    std::env::var("API_KEY_SALT").expect("API_KEY_SALT must be set")
}

fn default_webhook_secret() -> String {
    std::env::var("WEBHOOK_SECRET").expect("WEBHOOK_SECRET must be set")
}

fn default_max_connections() -> u32 {
    std::env::var("MAX_DB_CONNECTIONS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(100)  // Increased from 50 for high concurrency workloads
}

fn default_worker_count() -> usize {
    num_cpus::get() * 2
}

fn default_disable_webhook_delivery() -> bool {
    std::env::var("DISABLE_WEBHOOK_DELIVERY")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(false)
}

fn default_metrics_port() -> u16 {
    std::env::var("METRICS_PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(9091)  // Use 9091 instead of 9090 to avoid conflicts
}

impl Config {
    pub fn load() -> anyhow::Result<Self> {
        dotenvy::dotenv().ok();

        Ok(Config {
            database_url: default_database_url(),
            port: default_port(),
            host: default_host(),
            api_key_salt: default_api_key_salt(),
            webhook_secret: default_webhook_secret(),
            max_db_connections: default_max_connections(),
            delivery_worker_count: default_worker_count(),
            disable_webhook_delivery: default_disable_webhook_delivery(),
            metrics_port: default_metrics_port(),
        })
    }
}
