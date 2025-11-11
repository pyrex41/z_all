# Zapier Triggers API - Product Requirements Document (PRD)

**Version:** 1.0 (Rust Implementation)
**Status:** Design & Planning
**Last Updated:** November 10, 2025
**Owner:** Reuben (Technical Lead)
**Stakeholders:** Developers, Performance Engineers, Platform Architects

---

## 1. Executive Summary

### The Opportunity
Following successful Python (MVP) and Elixir (production) implementations, a Rust version will push performance boundaries for ultra-high-throughput, low-latency event ingestion at minimal resource cost.

### The Solution
A blazing-fast Rust implementation of the Zapier Triggers API using modern async runtime (Tokio), zero-copy serialization, and systems-level optimizations. Targets 10x Python throughput (~2,500+ req/s single node) with <10ms p95 latency and 50% lower memory footprint than Elixir.

### Success Criteria
- **Performance:** >2,500 req/s single node; <10ms p95 ingestion; <2s p95 delivery
- **Efficiency:** <200MB memory @ 1K req/s load; 50% lower CPU than Elixir
- **Compatibility:** 100% API parity with Python/Elixir; passes unified test suite
- **Developer Experience:** Clear setup, excellent error messages, comprehensive docs
- **Production Readiness:** 99.9% uptime capability; battle-tested dependencies

**Why Rust:** Zero-cost abstractions, memory safety without GC pauses, fearless concurrency, minimal resource footprint. Perfect for cost-sensitive, performance-critical deployments and edge computing scenarios.

---

## 2. Problem Statement & Positioning

### Current State (Python vs Elixir)
- **Python (FastAPI):** 245 req/s, 243ms p95, 512MB @ load, $90/mo
- **Elixir (Phoenix):** 892 req/s, 69ms p95, 380MB @ load, $75/mo

**Gap:** Need extreme performance for:
- High-volume customers (millions of events/day)
- Edge deployments (IoT, CDN edge functions)
- Cost optimization (serverless, spot instances)
- Compliance requirements (data residency, air-gapped)

**Rust Targets:**
- **Throughput:** 2,500+ req/s (10x Python, 3x Elixir)
- **Latency:** <10ms p95 (24x faster Python, 7x faster Elixir)
- **Memory:** <200MB @ load (2.5x less than Python, 2x less than Elixir)
- **Cost:** ~$50/mo same capacity (33% cheaper than Elixir)

---

## 3. Goals & Success Metrics

### North Star
Lowest cost-per-million-events in the comparison suite.

### Launch Goals (2 Months)
| Metric | Target | Comparison |
|--------|--------|------------|
| Throughput (single node) | 2,500+ req/s | 10x Python, 3x Elixir |
| Ingestion Latency (p95) | <10ms | 24x faster Python, 7x Elixir |
| Delivery Latency (p95) | <2s | 2.5x faster Elixir |
| Memory @ 1K req/s | <200MB | 2.5x less Python, 2x less Elixir |
| CPU @ 1K req/s | <30% | 3x less Python, 1.5x less Elixir |
| Binary Size | <20MB | Minimal container images |
| Cold Start | <100ms | Edge/serverless ready |
| Test Suite Pass Rate | 100% | Full compatibility |

**Long-Term (6 Months):** 10K+ req/s with clustering; WebAssembly edge runtime; <1ms p50 latency.

---

## 4. Target Users & Use Cases

### Primary: High-Volume Enterprise (Alex)
- **Profile:** Platform Eng at fintech/adtech (500+ employees); millions events/day
- **JTBD:** Minimize infrastructure costs while maximizing throughput
- **Pains:** AWS bills; latency SLAs; complex scaling
- **Success:** 10M events/day on single $50/mo instance; <5ms ingestion

### Secondary: Edge Computing Engineer (Jordan)
- **Profile:** DevOps at IoT platform; needs edge processing
- **JTBD:** Deploy event processors at CDN edge, IoT gateways
- **Pains:** Large runtimes; cold starts; resource limits
- **Success:** <20MB binaries; <100ms cold start; runs on ARM

### Tertiary: Cost-Conscious Startup (Sam)
- **Profile:** CTO at bootstrapped startup; tight budget
- **JTBD:** Production-grade API without premium infrastructure
- **Pains:** Can't afford dedicated servers; serverless expensive
- **Success:** Run on $5/mo VPS or AWS Lambda; scale on demand

---

## 5. User Stories (Prioritized)

### Epic 1: Core Performance (P0)
- **1.1:** As Alex, handle 2,500+ req/s on single node. *AC:* Benchmark shows >2,500 sustained with <10ms p95.
- **1.2:** As Alex, use <200MB memory. *AC:* Prometheus shows <200MB @ 1K req/s sustained load.
- **1.3:** As Jordan, deploy <20MB binary. *AC:* `cargo build --release` produces <20MB; Docker image <50MB.

### Epic 2: API Compatibility (P0)
- **2.1:** As integration dev, use identical endpoints. *AC:* All Python/Elixir endpoints work identically.
- **2.2:** As QA, pass unified test suite. *AC:* 100% functional tests pass; performance exceeds targets.
- **2.3:** As Marcus, get same error messages. *AC:* Validation/auth errors match Elixir format.

### Epic 3: Developer Experience (P1)
- **3.1:** As contributor, set up in <5 minutes. *AC:* `./scripts/setup-rust.sh` → ready to code.
- **3.2:** As ops, deploy easily. *AC:* Docker/Kubernetes manifests; one-line Fly.io deploy.
- **3.3:** As dev, understand errors. *AC:* Rust errors map to clear HTTP responses with context.

---

## 6. Functional Requirements

### Core API (P0) - Exact Parity
- **POST /api/events:** Ingest JSON (type req, payload ≤256KB, optional dedup_id). 201 w/ID; 409 dup; 429 limit. <10ms p95.
- **GET /api/inbox:** Paginated undelivered (limit 1-1K; filter status; cursor). Desc by created_at.
- **POST /api/ack/:event_id:** Mark delivered; idempotent.
- **POST /api/webhook/config:** Configure webhook URL.
- **POST /api/keys/generate:** Generate API key (org_name, tier).
- **GET /api/keys:** View API key info.
- **POST /api/keys/rotate:** Rotate API key.
- **GET /health:** Health check (simple).

### Delivery System (P0)
- **Worker:** Async task queue (Tokio tasks); POST to webhook w/headers (X-Event-ID, X-Event-Type).
- **Retries:** Exponential backoff (1s, 2s, 4s, 8s, 16s = 5 max); fail → DLQ.
- **Targets:** 95% <2s; 2,500+/sec/node; zero-copy message passing.
- **Concurrency:** Tokio runtime with configurable worker count (default: num_cpus * 2).

### Auth/Security (P0)
- **Keys:** 64-char prefixed (zap_live_/test_); hashed (Argon2id); rotation support.
- **Limits:** Tiered (free:100/min; pro:1K; business:10K; ent:100K); 429 w/Retry-After.
- **Security:** HTTPS only; HSTS; CORS; constant-time comparisons; no sensitive data logged.

### Observability (P1)
- **Metrics:** Prometheus exporter (on :9090); standard RED metrics (Rate/Error/Duration).
- **Logging:** Structured JSON (tracing + tracing-subscriber); configurable levels.
- **Tracing:** OpenTelemetry integration; distributed tracing support.

### Documentation (P1)
- **OpenAPI:** Auto-generated from code (utoipa crate).
- **Examples:** curl, Rust SDK, integration guides.
- **Deployment:** Docker, Kubernetes, Fly.io, AWS Lambda guides.

---

## 7. Non-Functional Requirements

### Performance Targets
| Metric | Target | Rationale |
|--------|--------|-----------|
| Throughput | 2,500+ req/s | 3x Elixir, 10x Python |
| Ingestion p95 | <10ms | Zero-copy, async I/O |
| Delivery p95 | <2s | Parallel HTTP clients |
| Memory | <200MB @ load | Stack allocation, no GC |
| CPU | <30% @ 1K req/s | Efficient async runtime |
| Binary Size | <20MB | Link-time optimization |
| Cold Start | <100ms | Fast initialization |

### Efficiency
- **Zero-Copy:** Avoid allocations in hot path; use `Bytes` for payload.
- **Async I/O:** Tokio for non-blocking DB/HTTP; connection pooling.
- **SIMD:** Use `simd-json` for fast parsing (10x faster than serde_json on large payloads).
- **Profiling:** CPU profiling via perf/flamegraph; memory via valgrind/heaptrack.

### Reliability
- **Durability:** PostgreSQL ACID; same backup strategy as Elixir.
- **Failures:** 503 on DB down; at-least-once delivery; DLQ for permanent fails.
- **Monitoring:** Prometheus alerts on errors>0.5%, latency>50ms, queue>5K.
- **Graceful Shutdown:** Handle SIGTERM; drain in-flight requests; max 30s.

### Security
- **Memory Safety:** Rust compiler prevents buffer overflows, data races, use-after-free.
- **Dependencies:** Audit with `cargo audit`; only vetted crates (reqwest, tokio, sqlx).
- **Crypto:** ring for hashing; rustls for TLS (no OpenSSL CVEs).
- **Sandboxing:** Seccomp profiles for containers; minimal capabilities.

---

## 8. Technical Architecture

### Stack
- **Framework:** Axum 0.7+ (Tokio-based, fast routing, type-safe extractors) OR Actix-web 4+ (proven, mature)
  - **Decision:** Start with Axum (simpler, Tower middleware, better ergonomics) → fallback to Actix if performance needed
- **Async Runtime:** Tokio 1.36+ (work-stealing scheduler, multi-threaded, industry standard)
- **Database:** PostgreSQL 16 via SQLx 0.7+ (compile-time checked queries, async, connection pooling)
- **Queue:** Postgres-backed (same as Elixir Oban pattern) OR custom in-memory with persistence (for extreme performance)
- **HTTP Client:** Reqwest 0.11+ with connection pooling (async webhook delivery)
- **Serialization:**
  - serde_json for standard cases
  - simd-json for high-throughput parsing (SIMD instructions)
- **Config:** config-rs + dotenvy (environment-based)
- **Infrastructure:** **Fly.io (Primary for Demo/Production)**
  - Firecracker microVMs for <100ms cold starts
  - Integrated PostgreSQL clusters with automatic failover
  - Internal networking (.internal DNS) for DB connections
  - Global edge distribution for multi-region deployments
  - Simple CLI-based deployments (flyctl)
  - Cost-effective: ~$10/mo for demo, scales to $50/mo production
  - Alternative options: Docker (self-hosted), Kubernetes, AWS Lambda (future)

### Key Crates
```toml
[dependencies]
# Web framework
axum = "0.7"                    # Fast, ergonomic web framework
tower = "0.4"                   # Middleware (rate limiting, timeouts, etc.)
tower-http = "0.5"              # HTTP-specific middleware (CORS, compression)

# Async runtime
tokio = { version = "1.36", features = ["full"] }

# Database
sqlx = { version = "0.7", features = ["runtime-tokio", "postgres", "chrono", "uuid", "json"] }

# HTTP client
reqwest = { version = "0.11", features = ["json", "rustls-tls"] }

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
simd-json = "0.13"             # SIMD-accelerated JSON parsing

# Authentication & crypto
argon2 = "0.5"                  # Password hashing
rand = "0.8"                    # Secure random generation
ring = "0.17"                   # Cryptographic primitives

# Observability
tracing = "0.1"                 # Structured logging
tracing-subscriber = { version = "0.3", features = ["json", "env-filter"] }
metrics = "0.21"                # Metrics collection
metrics-exporter-prometheus = "0.13"  # Prometheus exporter

# Utilities
chrono = { version = "0.4", features = ["serde"] }
uuid = { version = "1.7", features = ["v4", "serde"] }
anyhow = "1.0"                  # Error handling
thiserror = "1.0"               # Custom errors
config = "0.14"                 # Configuration management
dotenvy = "0.15"                # .env file support

[dev-dependencies]
criterion = "0.5"               # Benchmarking
```

### Database Schema (SQLx-compatible)
```sql
-- Same schema as Elixir/Python for compatibility

CREATE TABLE organizations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  api_key_hash VARCHAR(255) NOT NULL UNIQUE,
  webhook_url VARCHAR(2048),
  tier VARCHAR(50) NOT NULL DEFAULT 'free',
  rate_limit_per_minute INTEGER NOT NULL DEFAULT 100,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
  event_type VARCHAR(255) NOT NULL,
  dedup_id VARCHAR(255),
  payload JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(organization_id, dedup_id)
);

CREATE TABLE event_deliveries (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id UUID NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  status VARCHAR(50) NOT NULL DEFAULT 'pending', -- pending, delivered, failed
  attempts INTEGER NOT NULL DEFAULT 0,
  last_attempt_at TIMESTAMPTZ,
  response_status INTEGER,
  error_message TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE deduplication_cache (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
  dedup_id VARCHAR(255) NOT NULL,
  event_id UUID NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  expires_at TIMESTAMPTZ NOT NULL,
  UNIQUE(organization_id, dedup_id)
);

-- Indexes for performance
CREATE INDEX idx_events_org_created ON events(organization_id, created_at DESC);
CREATE INDEX idx_events_dedup ON events(organization_id, dedup_id) WHERE dedup_id IS NOT NULL;
CREATE INDEX idx_deliveries_status ON event_deliveries(status, created_at) WHERE status = 'pending';
CREATE INDEX idx_deliveries_event ON event_deliveries(event_id);
CREATE INDEX idx_dedup_expires ON deduplication_cache(expires_at);
```

### Key Code Patterns

#### 1. Main Application Setup
```rust
// src/main.rs
use axum::{Router, Server};
use std::net::SocketAddr;
use tower::ServiceBuilder;
use tower_http::cors::CorsLayer;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .json()
        .init();

    // Load config
    let config = Config::load()?;

    // Database connection pool
    let pool = sqlx::postgres::PgPoolOptions::new()
        .max_connections(50)
        .connect(&config.database_url)
        .await?;

    // Run migrations
    sqlx::migrate!("./migrations").run(&pool).await?;

    // Application state
    let state = Arc::new(AppState {
        db: pool,
        config: config.clone(),
        metrics: Metrics::new(),
    });

    // Background worker for deliveries
    tokio::spawn(delivery_worker(state.clone()));

    // API routes
    let app = Router::new()
        .route("/api/events", post(create_event))
        .route("/api/inbox", get(list_inbox))
        .route("/api/ack/:event_id", post(acknowledge_event))
        .route("/api/webhook/config", post(configure_webhook))
        .route("/api/keys/generate", post(generate_api_key))
        .route("/api/keys", get(get_api_key_info))
        .route("/api/keys/rotate", post(rotate_api_key))
        .route("/health", get(health_check))
        .route("/metrics", get(metrics_handler))
        .layer(
            ServiceBuilder::new()
                .layer(CorsLayer::permissive())
                .layer(TraceLayer::new_for_http())
        )
        .with_state(state);

    // Start server
    let addr = SocketAddr::from(([0, 0, 0, 0], config.port));
    tracing::info!("Starting server on {}", addr);

    Server::bind(&addr)
        .serve(app.into_make_service())
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    Ok(())
}
```

#### 2. Event Ingestion (Zero-Copy)
```rust
// src/handlers/events.rs
use axum::{extract::State, http::StatusCode, Json};
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use bytes::Bytes;

#[derive(Deserialize)]
pub struct CreateEventRequest {
    #[serde(rename = "type")]
    event_type: String,
    dedup_id: Option<String>,
    payload: serde_json::Value,
}

#[derive(Serialize)]
pub struct CreateEventResponse {
    id: Uuid,
    status: String,
}

pub async fn create_event(
    State(state): State<Arc<AppState>>,
    auth: AuthenticatedOrg,
    Json(req): Json<CreateEventRequest>,
) -> Result<(StatusCode, Json<CreateEventResponse>), ApiError> {
    // Rate limiting
    state.rate_limiter.check(&auth.org.id).await?;

    // Payload size check
    let payload_size = serde_json::to_vec(&req.payload)?.len();
    if payload_size > 256 * 1024 {
        return Err(ApiError::PayloadTooLarge);
    }

    // Deduplication check
    if let Some(dedup_id) = &req.dedup_id {
        let exists = sqlx::query_scalar::<_, bool>(
            "SELECT EXISTS(SELECT 1 FROM events WHERE organization_id = $1 AND dedup_id = $2)"
        )
        .bind(&auth.org.id)
        .bind(dedup_id)
        .fetch_one(&state.db)
        .await?;

        if exists {
            return Err(ApiError::DuplicateEvent);
        }
    }

    // Validate webhook configured
    if auth.org.webhook_url.is_none() {
        return Err(ApiError::WebhookNotConfigured);
    }

    // Insert event and delivery in transaction
    let mut tx = state.db.begin().await?;

    let event_id = sqlx::query_scalar::<_, Uuid>(
        r#"
        INSERT INTO events (organization_id, event_type, dedup_id, payload)
        VALUES ($1, $2, $3, $4)
        RETURNING id
        "#
    )
    .bind(&auth.org.id)
    .bind(&req.event_type)
    .bind(&req.dedup_id)
    .bind(&req.payload)
    .fetch_one(&mut *tx)
    .await?;

    sqlx::query(
        r#"
        INSERT INTO event_deliveries (event_id, status)
        VALUES ($1, 'pending')
        "#
    )
    .bind(&event_id)
    .execute(&mut *tx)
    .await?;

    tx.commit().await?;

    // Notify delivery worker (channel send)
    state.delivery_queue.send(event_id).await?;

    // Metrics
    state.metrics.events_created.inc();

    Ok((
        StatusCode::CREATED,
        Json(CreateEventResponse {
            id: event_id,
            status: "pending".to_string(),
        }),
    ))
}
```

#### 3. Delivery Worker (Parallel)
```rust
// src/workers/delivery.rs
use tokio::time::{sleep, Duration};
use reqwest::Client;

pub async fn delivery_worker(state: Arc<AppState>) {
    let client = Client::builder()
        .timeout(Duration::from_secs(10))
        .pool_max_idle_per_host(50)
        .build()
        .unwrap();

    let mut interval = tokio::time::interval(Duration::from_millis(100));

    loop {
        interval.tick().await;

        // Fetch pending deliveries (batch)
        let deliveries = sqlx::query_as::<_, PendingDelivery>(
            r#"
            SELECT ed.id, ed.event_id, ed.attempts, e.event_type, e.payload, o.webhook_url
            FROM event_deliveries ed
            JOIN events e ON e.id = ed.event_id
            JOIN organizations o ON o.id = e.organization_id
            WHERE ed.status = 'pending' AND ed.attempts < 5
            ORDER BY ed.created_at
            LIMIT 100
            "#
        )
        .fetch_all(&state.db)
        .await
        .unwrap_or_default();

        if deliveries.is_empty() {
            continue;
        }

        // Process in parallel
        let tasks: Vec<_> = deliveries
            .into_iter()
            .map(|delivery| {
                let client = client.clone();
                let db = state.db.clone();
                let metrics = state.metrics.clone();
                tokio::spawn(async move {
                    process_delivery(client, db, metrics, delivery).await
                })
            })
            .collect();

        // Wait for all
        for task in tasks {
            let _ = task.await;
        }
    }
}

async fn process_delivery(
    client: Client,
    db: PgPool,
    metrics: Arc<Metrics>,
    delivery: PendingDelivery,
) {
    let payload = serde_json::json!({
        "event_id": delivery.event_id,
        "type": delivery.event_type,
        "data": delivery.payload,
    });

    let result = client
        .post(&delivery.webhook_url)
        .json(&payload)
        .header("X-Event-ID", delivery.event_id.to_string())
        .header("X-Event-Type", &delivery.event_type)
        .send()
        .await;

    match result {
        Ok(resp) if resp.status().is_success() => {
            // Success
            sqlx::query(
                "UPDATE event_deliveries SET status = 'delivered', response_status = $1, updated_at = NOW() WHERE id = $2"
            )
            .bind(resp.status().as_u16() as i32)
            .bind(delivery.id)
            .execute(&db)
            .await
            .ok();

            metrics.deliveries_success.inc();
        }
        Ok(resp) => {
            // HTTP error
            update_failed_delivery(&db, delivery.id, delivery.attempts, Some(resp.status().as_u16())).await;
            metrics.deliveries_failed.inc();
        }
        Err(e) => {
            // Network/timeout error
            update_failed_delivery(&db, delivery.id, delivery.attempts, None).await;
            metrics.deliveries_failed.inc();
        }
    }
}

async fn update_failed_delivery(db: &PgPool, id: Uuid, attempts: i32, status: Option<u16>) {
    let new_attempts = attempts + 1;
    let new_status = if new_attempts >= 5 { "failed" } else { "pending" };

    sqlx::query(
        r#"
        UPDATE event_deliveries
        SET attempts = $1, status = $2, response_status = $3, last_attempt_at = NOW(), updated_at = NOW()
        WHERE id = $4
        "#
    )
    .bind(new_attempts)
    .bind(new_status)
    .bind(status.map(|s| s as i32))
    .bind(id)
    .execute(db)
    .await
    .ok();
}
```

#### 4. Authentication Middleware
```rust
// src/middleware/auth.rs
use axum::{extract::State, http::Request, middleware::Next, response::Response};
use argon2::{Argon2, PasswordHash, PasswordVerifier};

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
        let state = Arc::<AppState>::from_ref(state);

        // Extract API key from header
        let api_key = parts
            .headers
            .get("X-API-Key")
            .and_then(|v| v.to_str().ok())
            .ok_or(ApiError::Unauthorized)?;

        // Fetch org by hashed key (constant-time comparison)
        let org = sqlx::query_as::<_, Organization>(
            "SELECT * FROM organizations WHERE api_key_hash = $1"
        )
        .bind(hash_api_key(api_key))
        .fetch_optional(&state.db)
        .await?
        .ok_or(ApiError::Unauthorized)?;

        Ok(AuthenticatedOrg { org })
    }
}

fn hash_api_key(key: &str) -> String {
    let argon2 = Argon2::default();
    argon2.hash_password(key.as_bytes(), &salt).unwrap().to_string()
}
```

### Performance Optimizations

1. **Zero-Copy JSON:** Use `simd-json` for parsing; `Bytes` for payload storage
2. **Connection Pooling:** Reuse DB connections (max 50); HTTP client pooling
3. **Batch Processing:** Delivery worker fetches 100 events per cycle
4. **Parallel Delivery:** Tokio spawn per delivery (1000s concurrent)
5. **Memory Arena:** Pre-allocate buffers for hot paths
6. **Profile-Guided Optimization:** Build with PGO for 10-15% speedup
7. **Link-Time Optimization:** Enable LTO for 5-10% speedup

```toml
[profile.release]
lto = "fat"              # Link-time optimization
codegen-units = 1        # Better optimization
opt-level = 3            # Maximum optimization
strip = true             # Strip symbols (smaller binary)
```

---

## 9. Deployment Strategy

### Primary Deployment: Fly.io (Demo/Production)

**Why Fly.io:**
- Firecracker microVMs (fast cold starts <100ms)
- Built-in PostgreSQL clusters
- Global edge distribution ready
- Simple pricing: ~$5-50/mo for demo
- Zero-downtime deployments
- Internal networking for DB connections

#### Fly.io Setup & Configuration

**1. Database Provisioning:**
```bash
# Create Postgres cluster
fly postgres create --name zapier-triggers-db --region ord --vm-size shared-cpu-1x

# Attach to app
fly postgres attach zapier-triggers-db --app zapier-triggers-rust

# This sets DATABASE_URL automatically via secrets
```

**2. fly.toml Configuration:**
```toml
app = "zapier-triggers-rust"
primary_region = "ord"  # Chicago (demo) - closest to most US users

[build]
  dockerfile = "Dockerfile"

[env]
  PORT = "8080"
  RUST_LOG = "info"
  RUST_BACKTRACE = "1"

[http_service]
  internal_port = 8080
  force_https = true
  auto_stop_machines = false
  auto_start_machines = true
  min_machines_running = 1
  processes = ["app"]

  [[http_service.checks]]
    grace_period = "10s"
    interval = "30s"
    method = "GET"
    timeout = "5s"
    path = "/health"

[metrics]
  port = 9090
  path = "/metrics"

[[vm]]
  memory = "256mb"
  cpu_kind = "shared"
  cpus = 1

[[statics]]
  guest_path = "/app/public"
  url_prefix = "/public"
```

**3. Multi-Stage Dockerfile (Optimized for Fly.io):**
```dockerfile
# Build stage
FROM rust:1.76-alpine AS builder
WORKDIR /app

# Install build dependencies
RUN apk add --no-cache musl-dev pkgconfig openssl-dev

# Copy dependency files first (caching layer)
COPY Cargo.toml Cargo.lock ./
RUN mkdir src && echo "fn main() {}" > src/main.rs
RUN cargo build --release
RUN rm -rf src

# Copy actual source
COPY . .
RUN touch src/main.rs
RUN cargo build --release --locked

# Runtime stage (minimal)
FROM alpine:3.19
RUN apk add --no-cache ca-certificates libgcc

WORKDIR /app
COPY --from=builder /app/target/release/zapier-triggers /usr/local/bin/zapier-triggers

# Non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser
USER appuser

EXPOSE 8080 9090

CMD ["zapier-triggers"]
```

**Image size:** ~50MB (vs 150MB Elixir, 300MB Python)

**4. Secrets Management:**
```bash
# Set via Fly.io secrets (encrypted)
fly secrets set \
  API_KEY_SALT="$(openssl rand -hex 32)" \
  WEBHOOK_SECRET="$(openssl rand -hex 32)"

# DATABASE_URL set automatically by `fly postgres attach`
```

**5. Deployment Commands:**
```bash
# Initial launch (interactive)
fly launch --no-deploy
fly deploy

# Subsequent deploys
fly deploy

# View logs
fly logs

# SSH into VM
fly ssh console

# Scale up/down
fly scale count 2  # Multiple instances
fly scale vm dedicated-cpu-1x  # Upgrade VM

# Monitor
fly status
fly dashboard
```

**6. Database Migrations:**
```bash
# Run migrations on deploy via fly.toml:
[deploy]
  release_command = "zapier-triggers migrate"

# Or manually:
fly ssh console -C "zapier-triggers migrate"
```

**7. Monitoring & Metrics:**
- Prometheus metrics exposed on :9090
- Fly.io dashboard shows resource usage
- Custom metrics via `fly metrics` API
- Integration with Grafana Cloud optional

**8. Cost Estimate (Demo):**
- **API App:** shared-cpu-1x, 256MB RAM = ~$5/mo
- **Postgres:** shared-cpu-1x, 256MB RAM, 10GB = ~$5/mo
- **Total:** ~$10/mo for demo (can scale to $50/mo for production)

---

### Alternative Deployment Options (Future)

#### Option 2: Docker (Self-Hosted)
```dockerfile
# Use the same Dockerfile as Fly.io
docker build -t zapier-triggers-rust:latest .
docker run -p 8080:8080 -p 9090:9090 \
  -e DATABASE_URL="postgres://..." \
  zapier-triggers-rust:latest
```

#### Option 3: Kubernetes
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: zapier-triggers-rust
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: api
        image: zapier-triggers-rust:latest
        resources:
          requests:
            memory: "128Mi"
            cpu: "100m"
          limits:
            memory: "256Mi"
            cpu: "500m"
```

#### Option 4: AWS Lambda (Future)
- Use AWS Lambda Rust Runtime
- Cold start: <100ms
- Cost: Pay per invocation

---

### CI/CD Pipeline (GitHub Actions + Fly.io)
```yaml
# .github/workflows/ci.yml
name: CI/CD
on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:16-alpine
        env:
          POSTGRES_PASSWORD: postgres
          POSTGRES_DB: zapier_test
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable

      - name: Cache cargo registry
        uses: actions/cache@v3
        with:
          path: ~/.cargo/registry
          key: ${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}

      - name: Cache cargo build
        uses: actions/cache@v3
        with:
          path: target
          key: ${{ runner.os }}-cargo-build-${{ hashFiles('**/Cargo.lock') }}

      - name: Run tests
        env:
          DATABASE_URL: postgres://postgres:postgres@localhost:5432/zapier_test
        run: |
          cargo test --all-features
          cargo clippy -- -D warnings
          cargo fmt --check

  benchmark:
    runs-on: ubuntu-latest
    needs: [test]
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
      - name: Run benchmarks
        run: cargo bench
      - uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'cargo'
          output-file-path: target/criterion/output.txt

  deploy:
    needs: [test]
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Fly.io CLI
        uses: superfly/flyctl-actions/setup-flyctl@master

      - name: Deploy to Fly.io
        run: flyctl deploy --remote-only
        env:
          FLY_API_TOKEN: ${{ secrets.FLY_API_TOKEN }}
```

---

## 10. Testing Strategy

### Unit Tests
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_event() {
        let state = setup_test_state().await;
        let req = CreateEventRequest {
            event_type: "test.event".to_string(),
            dedup_id: None,
            payload: json!({"foo": "bar"}),
        };

        let result = create_event(State(state), test_auth(), Json(req)).await;
        assert!(result.is_ok());
    }
}
```

### Integration Tests (Unified Test Suite)
- **Target:** 100% pass rate on unified_test_suite
- **Strategy:** Implement exact API parity with Python/Elixir
- **CI:** Run unified tests on every PR

### Load Tests
```bash
# Benchmark target: >2,500 req/s
cargo install drill
drill --benchmark benchmark.yml --stats
```

### Property-Based Tests
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_event_roundtrip(payload in any::<serde_json::Value>()) {
        // Verify serialization roundtrip
    }
}
```

---

## 11. Risks & Mitigations

| Risk | Prob/Impact | Mitigation |
|------|-------------|------------|
| Complexity vs Python | Med/Med | Focus on ergonomics; clear error messages |
| Async Runtime Issues | Low/High | Use proven Tokio; extensive testing |
| Unsafe Code Bugs | Low/Crit | Zero unsafe blocks; audit dependencies |
| Performance Not Meeting Goals | Low/Med | Benchmark early; profile-guided optimization |
| Ecosystem Maturity | Low/Low | Use stable, mature crates (Tokio, Axum, SQLx) |

---

## 12. Timeline & Milestones

### Phase 1: MVP (Weeks 1-4)
- **Week 1:** Project setup, database schema, basic API structure
- **Week 2:** Core endpoints (events, inbox, ack), authentication
- **Week 3:** Delivery worker, webhook integration, rate limiting
- **Week 4:** Testing, docs, unified test suite integration

### Phase 2: Optimization (Weeks 5-6)
- **Week 5:** Performance profiling, SIMD JSON, connection pooling
- **Week 6:** Load testing, benchmarking vs Elixir/Python

### Phase 3: Production Ready (Weeks 7-8)
- **Week 7:** Docker/Kubernetes deployment, monitoring, alerts
- **Week 8:** Documentation, examples, launch prep

**Post-Launch:**
- Month 2: Edge deployment (Lambda, CloudFlare Workers)
- Month 3: WebAssembly runtime, clustering
- Month 4: Advanced features (batching, filtering)

---

## 13. Success Criteria

### Performance (Quantitative)
- ✅ Throughput: >2,500 req/s single node
- ✅ Latency: <10ms p95 ingestion, <2s p95 delivery
- ✅ Memory: <200MB @ 1K req/s
- ✅ CPU: <30% @ 1K req/s
- ✅ Binary: <20MB release build
- ✅ Cold start: <100ms

### Compatibility (Qualitative)
- ✅ 100% unified test suite pass
- ✅ API parity with Python/Elixir
- ✅ Drop-in replacement capability

### Developer Experience
- ✅ Setup time: <5 minutes
- ✅ Clear error messages
- ✅ Comprehensive documentation
- ✅ Easy deployment (Docker, Fly.io, K8s)

---

## 14. Open Questions

- **Queue:** In-memory (max performance) vs Postgres-backed (same as Elixir)?
  - **Decision:** Start with Postgres-backed for parity; add in-memory option for edge cases
- **Framework:** Axum vs Actix-web?
  - **Decision:** Axum (better ergonomics, Tower ecosystem); benchmark both
- **Edge Deployment:** Priority for Lambda/CloudFlare Workers?
  - **Decision:** Phase 2 (after core functionality stable)

---

## 15. Comparison Matrix (Target)

| Metric | Python | Elixir | **Rust (Target)** |
|--------|--------|--------|-------------------|
| Throughput | 245 req/s | 892 req/s | **2,500+ req/s** |
| P95 Latency | 243ms | 69ms | **<10ms** |
| Memory @ Load | 512MB | 380MB | **<200MB** |
| CPU @ Load | 85% | 45% | **<30%** |
| Binary Size | N/A | ~40MB | **<20MB** |
| Cold Start | ~500ms | N/A | **<100ms** |
| Cost/Month | $90 | $75 | **~$50** |
| Setup Time | 5 min | 10 min | **<5 min** |
| **Best For** | MVP, Prototyping | Production, Scale | **Ultra-high performance, Edge** |

---

## 16. Conclusion

The Rust implementation completes the trifecta: Python for rapid prototyping, Elixir for production reliability, and **Rust for maximum performance and efficiency**. This gives developers the ultimate choice based on their specific needs:

- **Prototype first?** → Python
- **Production-ready now?** → Elixir
- **Need extreme performance?** → **Rust**

All three share the same API, same database schema, and pass the same unified test suite—true polyglot architecture at its best.

**Next Steps:**
1. Approve PRD
2. Set up Rust project structure (`cargo new`)
3. Implement core API endpoints (Week 1)
4. Target unified test suite compatibility (Week 4)

---

**Let's build the fastest webhook API in the ecosystem. 🚀🦀**
