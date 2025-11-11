# Zapier Triggers API - Rust Implementation

High-performance webhook-based event ingestion system built with Rust, targeting 2,500+ req/s with <10ms P95 latency.

## 🚀 Quick Start

### Prerequisites
- Rust 1.76+ (`rustup update`)
- PostgreSQL 16+
- Fly.io CLI (for deployment)

### Local Development

```bash
# 1. Clone and setup
git clone <repo>
cd zapier_rust

# 2. Install dependencies
cargo build

# 3. Setup environment
cp .env.example .env
# Edit .env and set:
# - DATABASE_URL
# - API_KEY_SALT (generate with: openssl rand -hex 32)
# - WEBHOOK_SECRET (generate with: openssl rand -hex 32)

# 4. Run migrations
cargo install sqlx-cli --no-default-features --features postgres
sqlx migrate run

# 5. Start server
cargo run
```

Server runs on `http://localhost:8080`

## 📋 API Endpoints

### Events
- `POST /api/events` - Ingest event (requires auth)
- `GET /api/inbox` - List undelivered events (requires auth)
- `POST /api/ack/:event_id` - Acknowledge delivery (requires auth)

### Webhooks
- `POST /api/webhook/config` - Configure webhook URL (requires auth)

### Keys
- `POST /api/keys/generate` - Generate new API key
- `GET /api/keys` - Get key info (requires auth)
- `POST /api/keys/rotate` - Rotate API key (requires auth)

### Health
- `GET /health` - Health check
- `GET /metrics` - Prometheus metrics

## 🔐 Authentication

All authenticated endpoints require `X-API-Key` header:

```bash
curl -H "X-API-Key: zap_live_..." https://your-app.fly.dev/api/events
```

## 🚢 Deployment (Fly.io)

### First Time Setup

```bash
# 1. Create database
fly postgres create --name zapier-triggers-db --region ord --vm-size shared-cpu-1x

# 2. Launch app
fly launch --no-deploy

# 3. Attach database
fly postgres attach zapier-triggers-db

# 4. Set secrets
fly secrets set \
  API_KEY_SALT="$(openssl rand -hex 32)" \
  WEBHOOK_SECRET="$(openssl rand -hex 32)"

# 5. Deploy
fly deploy
```

### Regular Deployment

```bash
fly deploy
```

See [QUICK_REFERENCE.md](./QUICK_REFERENCE.md) for more commands and [FLY_IO_DEPLOYMENT.md](./FLY_IO_DEPLOYMENT.md) for detailed deployment guide.

## 🧪 Testing

```bash
# Unit tests
cargo test

# Integration tests
cargo test --test integration

# Linting
cargo clippy

# Formatting
cargo fmt --check

# Benchmarks
cargo bench
```

## 📊 Performance Targets

| Metric | Target | Status |
|--------|--------|--------|
| Throughput | 2,500+ req/s | ⏳ To verify |
| P95 Latency | <10ms | ⏳ To verify |
| Memory @ 1K req/s | <200MB | ⏳ To verify |
| CPU @ 1K req/s | <30% | ⏳ To verify |
| Binary Size | <20MB | ✅ Achieved |
| Cold Start | <100ms | ⏳ To verify |

## 🏗️ Architecture

- **Framework**: Axum (Tokio-based)
- **Database**: PostgreSQL 16 via SQLx
- **Queue**: Postgres-backed delivery system
- **Auth**: Argon2id API key hashing
- **Monitoring**: Prometheus metrics, structured logging

### Project Structure

```
src/
├── main.rs              # Application entry
├── config.rs            # Configuration
├── error.rs             # Error handling
├── state.rs             # Application state
├── handlers/            # HTTP endpoints
│   ├── events.rs        # Event ingestion & inbox
│   ├── keys.rs          # API key management
│   └── health.rs        # Health checks
├── middleware/          # Auth middleware
│   └── auth.rs
├── models/              # Database models
│   ├── organization.rs
│   ├── event.rs
│   └── delivery.rs
└── workers/             # Background workers
    └── delivery.rs      # Webhook delivery
```

## 💰 Cost Estimate

### Demo (~$10/mo)
- API: shared-cpu-1x, 256MB = $5/mo
- PostgreSQL: shared-cpu-1x, 10GB = $5/mo

### Production (~$50/mo)
- API (2x): dedicated-cpu-1x, 512MB = $30/mo
- PostgreSQL (HA): dedicated-cpu-1x, 50GB + replica = $20/mo

## 🔗 Related Projects

This is part of a polyglot comparison suite:
- **Python** (FastAPI): MVP baseline - 245 req/s
- **Elixir** (Phoenix): Production reference - 892 req/s
- **Rust** (Axum): Ultra-performance - 2,500+ req/s (target)

All implementations share the same API and pass the unified test suite.

## 📚 Documentation

- [QUICK_REFERENCE.md](./QUICK_REFERENCE.md) - Common operations
- [FLY_IO_DEPLOYMENT.md](./FLY_IO_DEPLOYMENT.md) - Deployment guide
- [UPDATES_SUMMARY.md](./UPDATES_SUMMARY.md) - Recent changes
- [.taskmaster/docs/prd-init.md](./.taskmaster/docs/prd-init.md) - Product requirements

## 🤝 Contributing

1. Fork the repo
2. Create feature branch (`git checkout -b feature/amazing`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push to branch (`git push origin feature/amazing`)
5. Open Pull Request

## 📝 License

MIT

---

**Built with 🦀 Rust for maximum performance and efficiency**
