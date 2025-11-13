mod config;
mod error;
mod handlers;
mod middleware;
mod models;
mod state;
mod workers;
mod metrics;
mod event_processor;
mod auth_cache;

use axum::{
    routing::{get, post},
    Router,
};
use sqlx::postgres::PgPoolOptions;
use std::sync::Arc;
use tower_http::{
    cors::CorsLayer,
    trace::TraceLayer,
};

use config::Config;
use state::AppState;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            std::env::var("RUST_LOG").unwrap_or_else(|_| "info,zapier_triggers=debug".to_string())
        )
        .json()
        .init();

    tracing::info!("Starting Zapier Triggers API (Rust)");

    // Load configuration
    let config = Config::load()?;
    tracing::info!("Configuration loaded");

    // Database connection pool
    let pool = PgPoolOptions::new()
        .max_connections(config.max_db_connections)
        .acquire_timeout(std::time::Duration::from_secs(3))  // Prevent indefinite waits
        .connect(&config.database_url)
        .await?;

    tracing::info!("Database connected");

    // Run migrations
    sqlx::migrate!("./migrations")
        .run(&pool)
        .await?;

    tracing::info!("Migrations complete");

    // Initialize Prometheus metrics
    metrics::init_metrics_exporter(config.metrics_port);
    tracing::info!("Metrics exporter initialized on port {}", config.metrics_port);

    // Create event processor with worker pool
    let event_processor = event_processor::create_event_processor(pool.clone());
    tracing::info!("Event processor initialized with worker pool");

    // Create auth cache with cleanup task
    let auth_cache = auth_cache::create_auth_cache();
    auth_cache::start_cache_cleanup_task(Arc::clone(&auth_cache));
    tracing::info!("Auth cache initialized with TTL cleanup");

    // Application state
    let state = Arc::new(AppState::new(
        pool.clone(),
        config.clone(),
        event_processor,
        auth_cache,
    ));

    // Start delivery worker
    let _worker_handle = workers::start_delivery_worker(pool.clone(), config.disable_webhook_delivery);
    if config.disable_webhook_delivery {
        tracing::info!("Delivery worker started (webhook delivery DISABLED for performance testing)");
    } else {
        tracing::info!("Delivery worker started");
    }

    // Build router
    let app = Router::new()
        // Event endpoints
        .route("/api/events", post(handlers::create_event))
        .route("/api/inbox", get(handlers::list_inbox))
        .route("/api/ack/:event_id", post(handlers::acknowledge_event))
        .route("/api/webhook/config", post(handlers::configure_webhook))

        // Key management endpoints
        .route("/api/keys/generate", post(handlers::generate_key))
        .route("/api/keys", get(handlers::get_key_info))
        .route("/api/keys/rotate", post(handlers::rotate_key))

        // Health and metrics
        .route("/health", get(handlers::health_check))
        .route("/metrics", get(handlers::metrics_handler))

        // Middleware
        .layer(CorsLayer::permissive())
        .layer(TraceLayer::new_for_http())
        .with_state(state);

    // Start server
    let addr = format!("{}:{}", config.host, config.port);
    let listener = tokio::net::TcpListener::bind(&addr).await?;

    tracing::info!("Server listening on {}", addr);

    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    Ok(())
}

async fn shutdown_signal() {
    use tokio::signal;

    let ctrl_c = async {
        signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {
            tracing::info!("Received Ctrl+C, shutting down gracefully");
        },
        _ = terminate => {
            tracing::info!("Received SIGTERM, shutting down gracefully");
        },
    }
}
