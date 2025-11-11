# Zapier Triggers API - Elixir Implementation

> **Note**: This is part of a [monorepo](../../../README.md) with multiple implementations. See [comparison](../../../COMPARISON_SUMMARY.md) for performance analysis.

A production-ready event ingestion and webhook delivery system built with Elixir and Phoenix.

## Quick Start

**From monorepo root:**
```bash
./scripts/setup-elixir.sh
```

**Or manually:**
```bash
# Navigate to Elixir implementation
cd zapier_elixir/zapier_triggers

# Install dependencies
mix deps.get

# Create and migrate database
mix ecto.create && mix ecto.migrate

# Start the server
mix phx.server
```

Server runs at `http://localhost:4000`

## Documentation & API Explorer

- **Swagger UI**: http://localhost:4000/api/docs - Interactive API documentation
- **OpenAPI Spec**: http://localhost:4000/api/openapi - Machine-readable API spec
- **Health Checks**:
  - Liveness: http://localhost:4000/health/live
  - Readiness: http://localhost:4000/health/ready

## Features

- ✅ **Event Ingestion**: RESTful API for event ingestion with 256KB payload limit
- ✅ **Webhook Delivery**: Automatic delivery with retries and Dead Letter Queue
- ✅ **Rate Limiting**: Tier-based limits (100/1K/10K/100K per minute)
- ✅ **Deduplication**: 24-hour window prevents duplicate events
- ✅ **Authentication**: Secure API key management with bcrypt hashing
- ✅ **Security**: HTTPS-only with HSTS, TLS 1.2+, CORS support
- ✅ **Observability**: Structured JSON logging + Prometheus metrics (port 9568)
- ✅ **Reliability**: PostgreSQL-backed job queue with 5 retry attempts

## Documentation

- **[API Documentation](API_DOCUMENTATION.md)** - Complete API reference with examples
- **[Implementation Summary](IMPLEMENTATION_SUMMARY.md)** - Technical details and architecture

## Testing

Run the integration test script:

```bash
bash test_api.sh
```

Tests cover:
- API key generation and rotation
- Event creation and deduplication
- Webhook configuration
- Rate limiting
- Inbox listing

## API Endpoints

| Method | Path | Auth | Description |
|--------|------|------|-------------|
| POST | /api/keys/generate | No | Generate API key |
| GET | /api/keys | Yes | View API key info |
| POST | /api/keys/rotate | Yes | Rotate API key |
| POST | /api/webhook/config | Yes | Set webhook URL |
| POST | /api/events | Yes | Create event |
| GET | /api/inbox | Yes | List events |
| POST | /api/ack/:event_id | Yes | Acknowledge event |

## Example Usage

### 1. Generate API Key

```bash
curl -X POST http://localhost:4000/api/keys/generate \
  -H "Content-Type: application/json" \
  -d '{
    "organization_name": "My Organization",
    "tier": "free"
  }'
```

### 2. Configure Webhook

```bash
curl -X POST http://localhost:4000/api/webhook/config \
  -H "X-API-Key: your-api-key" \
  -H "Content-Type: application/json" \
  -d '{"webhook_url": "https://example.com/webhook"}'
```

### 3. Create Event

```bash
curl -X POST http://localhost:4000/api/events \
  -H "X-API-Key: your-api-key" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "user.created",
    "dedup_id": "unique-id-123",
    "payload": {
      "user_id": "12345",
      "email": "user@example.com"
    }
  }'
```

### 4. View Inbox

```bash
curl http://localhost:4000/api/inbox?status=pending \
  -H "X-API-Key: your-api-key"
```

## Tech Stack

- **Framework**: Phoenix 1.7+ (Elixir 1.14+)
- **Database**: PostgreSQL 14+
- **Job Queue**: Oban (Postgres-backed)
- **Caching**: Cachex (in-memory)
- **Rate Limiting**: Hammer (ETS-backed)
- **Logging**: LoggerJSON
- **Metrics**: Telemetry + Prometheus

## Monitoring

**Prometheus Metrics**: `http://localhost:9568/metrics`

Key metrics:
- `zapier_triggers_events_created_count`
- `zapier_triggers_deliveries_success_count`
- `zapier_triggers_rate_limit_exceeded_count`

## Production Deployment

### Docker (Recommended)

```bash
# Build the image
docker build -t zapier-triggers:latest .

# Run with docker-compose
docker-compose up -d

# Access services
# - API: http://localhost:4000
# - Swagger: http://localhost:4000/api/docs
# - Metrics: http://localhost:9568/metrics
```

See [DEPLOYMENT.md](DEPLOYMENT.md) for complete deployment guide including:
- Fly.io, Render, Railway deployment
- Kubernetes manifests
- Health checks and monitoring
- Backup and recovery

### Environment Variables

Required:
```bash
DATABASE_URL=ecto://user:pass@host/database
SECRET_KEY_BASE=$(mix phx.gen.secret)
PHX_HOST=api.example.com
PHX_SERVER=true
```

Optional:
```bash
PORT=4000
POOL_SIZE=10
```

### Build Release (Non-Docker)

```bash
MIX_ENV=prod mix release
_build/prod/rel/zapier_triggers/bin/zapier_triggers start
```

## Rate Limits

| Tier | Requests/Minute |
|------|-----------------|
| Free | 100 |
| Pro | 1,000 |
| Business | 10,000 |
| Enterprise | 100,000 |

## Webhook Delivery

- 5 retry attempts with exponential backoff
- 30-second timeout per request
- Dead Letter Queue for permanent failures
- Custom headers: `X-Event-ID`, `X-Event-Type`

Expected webhook response: HTTP 2xx

## Performance Benchmarking

A comprehensive benchmark tool is available to test API performance. Supports both Python and Elixir implementations with auto-detection.

```bash
# Setup (one-time)
cd benchmark_tool && uv sync && cd ..

# Quick benchmark
./benchmark_tool/run_benchmark.sh --quick

# Full benchmark with your API key
./benchmark_tool/run_benchmark.sh \
  --url http://localhost:4000 \
  --api-key YOUR_API_KEY \
  --requests 1000 \
  --concurrency 50
```

See [benchmark_tool/BENCHMARK.md](benchmark_tool/BENCHMARK.md) for complete documentation.

## Development

```bash
# Run tests
mix test

# Interactive shell
iex -S mix phx.server

# Database operations
mix ecto.create
mix ecto.migrate
mix ecto.rollback
mix ecto.reset
```

## Monorepo Context

This Elixir implementation is part of a comparative study with:
- **[Python (FastAPI)](../../../zapier_python/README.md)** - Simpler, faster development
- **[Rust](../../../zapier_rust/)** - Maximum performance (WIP)
- **[Unified Test Suite](../../../unified_test_suite/README.md)** - Cross-implementation testing

### Why Choose Elixir

✅ **Choose Elixir if:**
- Building for production from day 1
- Need high performance (>500 req/s)
- Want lower infrastructure costs (no Redis needed)
- Need built-in fault tolerance and hot code reloading
- Building real-time/concurrent systems
- Want fewer dependencies and moving parts

⚠️ **Consider Python if:**
- Rapid prototyping is priority
- Team is Python-focused
- Expected load < 500 req/s

### Performance Advantage

Benchmarked performance vs Python:
- **3.6x higher throughput** (892 vs 245 req/s)
- **72% lower latency** (69ms vs 243ms P95)
- **47% lower CPU usage** (45% vs 85% @ load)
- **26% lower memory** (380MB vs 512MB @ load)
- **17% lower cost** ($75 vs $90/mo for same capacity)

See [full comparison](../../../COMPARISON_SUMMARY.md) for detailed benchmarks.

## License

Copyright © 2025 - MIT
