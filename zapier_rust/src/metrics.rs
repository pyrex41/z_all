use std::time::Instant;

/// Metrics tracker for performance monitoring
pub struct MetricsTracker {
    start: Instant,
    metric_name: &'static str,
}

impl MetricsTracker {
    pub fn new(metric_name: &'static str) -> Self {
        Self {
            start: Instant::now(),
            metric_name,
        }
    }

    pub fn record(self) {
        let duration = self.start.elapsed();
        metrics::histogram!(self.metric_name, duration.as_secs_f64());
    }
}

/// Record event ingestion latency
pub fn track_event_ingestion() -> MetricsTracker {
    metrics::counter!("events_ingested_total", 1);
    MetricsTracker::new("event_ingestion_latency_seconds")
}

/// Record authentication latency
pub fn track_authentication() -> MetricsTracker {
    metrics::counter!("auth_checks_total", 1);
    MetricsTracker::new("auth_latency_seconds")
}

/// Record rate limit checks
pub fn track_rate_limit_check() -> MetricsTracker {
    metrics::counter!("rate_limit_checks_total", 1);
    MetricsTracker::new("rate_limit_check_latency_seconds")
}

/// Record rate limit rejections
#[allow(dead_code)] // Will be used when rate limiting is fully implemented
pub fn record_rate_limit_exceeded() {
    metrics::counter!("rate_limit_exceeded_total", 1);
}

/// Record database operations
pub fn track_db_operation(operation: &str) -> MetricsTracker {
    metrics::counter!("db_operations_total", 1, "operation" => operation.to_string());
    MetricsTracker::new("db_operation_latency_seconds")
}

/// Record webhook deliveries
#[allow(dead_code)] // Will be used when webhook delivery system is implemented
pub fn record_webhook_delivery(status: &str) {
    metrics::counter!("webhook_deliveries_total", 1, "status" => status.to_string());
}

/// Record event queue size
pub fn record_event_queue_size(size: usize) {
    metrics::gauge!("event_queue_size", size as f64);
}

/// Record cache hit/miss
pub fn record_cache_hit(hit: bool) {
    let label = if hit { "hit" } else { "miss" };
    metrics::counter!("cache_lookups_total", 1, "result" => label.to_string());
}

/// Record event validation errors
pub fn record_validation_error(error_type: &str) {
    metrics::counter!("validation_errors_total", 1, "type" => error_type.to_string());
}

/// Record event processing failures
pub fn record_event_processing_failure(error_type: &str) {
    metrics::counter!("events_failed_total", 1, "error" => error_type.to_string());
}

/// Initialize Prometheus metrics exporter
pub fn init_metrics_exporter(port: u16) {
    let addr: std::net::SocketAddr = ([0, 0, 0, 0], port).into();
    let builder = metrics_exporter_prometheus::PrometheusBuilder::new();
    builder
        .with_http_listener(addr)
        .install()
        .expect("failed to install Prometheus recorder");
}

/// Get current metrics as Prometheus format string
pub fn get_metrics() -> String {
    // The metrics are exported via the global recorder
    // This function returns the current snapshot
    let recorder = metrics_exporter_prometheus::PrometheusBuilder::new()
        .build_recorder();

    recorder.handle().render()
}
