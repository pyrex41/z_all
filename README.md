# Zapier Triggers API - Monorepo

A comprehensive comparison of event-driven webhook ingestion systems built in **Python**, **Elixir**, and **Rust**, with a unified test suite for performance benchmarking and correctness validation.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This monorepo implements the **Zapier Triggers API** specification in multiple languages, allowing direct comparison of:
- **Performance** (throughput, latency, resource usage)
- **Feature completeness** (rate limiting, deduplication, etc.)
- **Developer experience** (setup, testing, deployment)
- **Operational characteristics** (scalability, cost, complexity)

## Implementations

### 🐍 Python (FastAPI)
**Status**: MVP Complete | **Port**: 8000

A straightforward, rapid-prototyping implementation using modern Python async patterns.

**Tech Stack:**
- FastAPI + Uvicorn
- PostgreSQL + Redis
- SQLModel/Alembic
- UV package manager

**Strengths:**
- Simple, approachable codebase (~700 LOC)
- Familiar for Python developers
- Good for moderate loads (~250 req/s)
- Excellent documentation via FastAPI

**Best For:** MVPs, Python-heavy teams, moderate scale (<500 req/s)

📖 [Full Documentation](zapier_python/README.md)

### 💧 Elixir (Phoenix)
**Status**: Production Ready | **Port**: 4000

A production-grade implementation leveraging the BEAM VM's concurrency model.

**Tech Stack:**
- Phoenix 1.7
- PostgreSQL (with Oban)
- Cachex + Hammer
- Prometheus metrics

**Strengths:**
- 3-4x better performance (892 req/s)
- Complete feature set (10/10)
- Lower infrastructure costs (no Redis)
- Built-in fault tolerance

**Best For:** Production deployments, high scale (>500 req/s), lower ops cost

📖 [Full Documentation](zapier_elixir/zapier_triggers/README.md) | [API Docs](http://localhost:4000/api/docs)

### 🦀 Rust (Actix/Axum)
**Status**: Work in Progress | **Port**: 8080

High-performance implementation with zero-cost abstractions and memory safety.

**Tech Stack:**
- TBD (Actix-web or Axum)
- PostgreSQL
- Tokio async runtime

**Target:** Maximum performance with minimal resource footprint

📖 Documentation coming soon

## Quick Start

### Prerequisites

Choose your implementation:
- **Python**: Python 3.12+, UV, PostgreSQL, Redis
- **Elixir**: Elixir 1.14+, PostgreSQL
- **Rust**: Rust 1.70+, PostgreSQL

### Setup All Implementations

```bash
# Setup each implementation
./scripts/setup-python.sh
./scripts/setup-elixir.sh
./scripts/setup-rust.sh

# Start all services
./scripts/start-all.sh

# Run all tests
./scripts/test-all.sh
```

### Setup Individual Implementation

```bash
# Python
cd zapier_python
uv sync
uv run uvicorn src.zapier_triggers_api.main:app --reload
# Visit: http://localhost:8000/docs

# Elixir
cd zapier_elixir/zapier_triggers
mix deps.get && mix ecto.create && mix ecto.migrate
mix phx.server
# Visit: http://localhost:4000/api/docs

# Rust
cd zapier_rust
cargo build && cargo run
# Visit: http://localhost:8080
```

## Unified Test Suite

Comprehensive testing framework for comparing all implementations:

```bash
cd unified_test_suite

# Install dependencies
uv sync

# Run functional tests (correctness)
./run_tests.sh --type functional

# Run performance benchmarks
./run_tests.sh --type performance

# Interactive load testing
./run_tests.sh --type load
# Open: http://localhost:8089
```

**Features:**
- ✅ 15+ functional correctness tests
- ✅ Performance benchmarking (throughput, latency)
- ✅ Load testing with Locust (interactive UI)
- ✅ Realistic test data generation
- ✅ Side-by-side comparison reports

📖 [Test Suite Documentation](unified_test_suite/README.md)

## Performance Comparison

Based on comprehensive benchmarking (1000 requests, 50 concurrent):

| Metric | Python | Elixir | Winner |
|--------|--------|--------|--------|
| **Throughput** | 245 req/s | 892 req/s | Elixir (3.6x) 🏆 |
| **P95 Latency** | 243ms | 69ms | Elixir (72% lower) 🏆 |
| **P99 Latency** | 289ms | 89ms | Elixir (69% lower) 🏆 |
| **CPU @ Load** | 85% | 45% | Elixir 🏆 |
| **Memory @ Load** | 512MB | 380MB | Elixir 🏆 |
| **Infrastructure** | PG + Redis | PG only | Elixir 🏆 |
| **Cost (AWS)** | ~$90/mo | ~$75/mo | Elixir 🏆 |

📊 [Full Comparison Report](COMPARISON_SUMMARY.md)

## Feature Comparison

| Feature | Python | Elixir | Rust |
|---------|--------|--------|------|
| Event Ingestion | ✅ | ✅ | 🚧 |
| Webhook Delivery | ✅ | ✅ | 🚧 |
| Rate Limiting | 🟡 Basic | ✅ 4-tier | 🚧 |
| Deduplication | ❌ | ✅ 24hr | 🚧 |
| API Key Rotation | ❌ | ✅ | 🚧 |
| Payload Limits | ⚠️ | ✅ 256KB | 🚧 |
| OpenAPI/Swagger | ✅ | ✅ | 🚧 |
| Prometheus Metrics | ⚠️ | ✅ | 🚧 |
| Health Checks | ✅ Basic | ✅ Live+Ready | 🚧 |

## API Endpoints

All implementations support the same RESTful API:

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/keys/generate` | Generate API key |
| GET | `/api/keys` | View API key info |
| POST | `/api/keys/rotate` | Rotate API key (Elixir only) |
| POST | `/api/events` | Ingest event |
| GET | `/api/inbox` | List events |
| POST | `/api/ack/:id` | Acknowledge event |
| POST | `/api/webhook/config` | Configure webhook URL |
| GET | `/health` | Health check |

## Example Usage

### 1. Generate API Key

```bash
curl -X POST http://localhost:4000/api/keys/generate \
  -H "Content-Type: application/json" \
  -d '{
    "organization_name": "My Org",
    "tier": "free"
  }'
```

### 2. Send Event

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

### 3. Check Inbox

```bash
curl http://localhost:4000/api/inbox?status=pending \
  -H "X-API-Key: your-api-key"
```

## Repository Structure

```
zapier/
├── zapier_python/          # Python (FastAPI) implementation
│   ├── src/                # Source code
│   ├── tests/              # Unit tests
│   └── README.md           # Python-specific docs
│
├── zapier_elixir/          # Elixir (Phoenix) implementation
│   └── zapier_triggers/    # Phoenix project
│       ├── lib/            # Source code
│       ├── test/           # Unit tests
│       └── README.md       # Elixir-specific docs
│
├── zapier_rust/            # Rust implementation (WIP)
│   ├── src/                # Source code
│   └── tests/              # Unit tests
│
├── unified_test_suite/     # Cross-implementation testing
│   ├── tests/              # Functional + performance tests
│   ├── data/               # Test data generator
│   └── README.md           # Test suite documentation
│
├── scripts/                # Helper scripts
│   ├── setup-*.sh          # Setup each implementation
│   ├── test-all.sh         # Run all tests
│   └── start-all.sh        # Start all services
│
├── COMPARISON_SUMMARY.md   # Detailed performance comparison
├── CONTRIBUTING.md         # Contribution guidelines
├── project_spec.md         # Original PRD
└── README.md               # This file
```

## Development

### Running Tests

```bash
# Test everything
./scripts/test-all.sh

# Test specific implementation
cd zapier_python && uv run pytest
cd zapier_elixir/zapier_triggers && mix test
cd zapier_rust && cargo test

# Unified test suite
cd unified_test_suite && ./run_tests.sh
```

### Code Quality

```bash
# Python
cd zapier_python
uv run ruff check .
uv run mypy .

# Elixir
cd zapier_elixir/zapier_triggers
mix format
mix credo

# Rust
cd zapier_rust
cargo fmt
cargo clippy
```

## Decision Guide

### Choose Python If:
- ✅ Building MVP/prototype
- ✅ Team is Python developers
- ✅ Need rapid development
- ✅ Expected load < 500 req/s
- ✅ Integrating with Python ecosystem

### Choose Elixir If:
- ✅ Building for production
- ✅ Need high performance (>500 req/s)
- ✅ Want lower infrastructure costs
- ✅ Need built-in fault tolerance
- ✅ Want fewer dependencies

### Choose Rust If:
- ✅ Need maximum performance
- ✅ Minimal resource footprint critical
- ✅ Team experienced with systems programming
- ✅ Zero-cost abstractions required

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development workflow
- Code style guidelines
- Testing requirements
- PR process

## Resources

- **Original Spec**: [project_spec.md](project_spec.md)
- **Performance Analysis**: [COMPARISON_SUMMARY.md](COMPARISON_SUMMARY.md)
- **Test Suite Guide**: [unified_test_suite/README.md](unified_test_suite/README.md)
- **Python Docs**: [zapier_python/README.md](zapier_python/README.md)
- **Elixir Docs**: [zapier_elixir/zapier_triggers/README.md](zapier_elixir/zapier_triggers/README.md)

## License

MIT

## Acknowledgments

Built as a comparative study of modern web frameworks and programming paradigms for event-driven systems.

---

**Quick Links:**
- Python API: http://localhost:8000/docs
- Elixir API: http://localhost:4000/api/docs
- Test Suite: [unified_test_suite/](unified_test_suite/)
- Comparison: [COMPARISON_SUMMARY.md](COMPARISON_SUMMARY.md)
