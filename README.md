# Zapier Triggers API - Monorepo

A comprehensive comparison of event-driven webhook ingestion systems built in **Python**, **Elixir**, **Rust**, and **Common Lisp**, with a unified test suite for performance benchmarking and correctness validation.

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
- Good for moderate loads
- Excellent documentation via FastAPI

**Best For:** MVPs, Python-heavy teams, moderate scale (<500 req/s)

📖 [Full Documentation](zapier_python/README.md)

### 💧 Elixir (Phoenix)
**Status**: Production Ready | **Port**: 4000

A production-grade implementation leveraging the BEAM VM's concurrency model with cache-first architecture.

**Tech Stack:**
- Phoenix 1.7
- PostgreSQL (with Oban)
- Triple Cachex setup (dedup, auth, event queue)
- Hammer rate limiting
- Prometheus metrics

**Strengths:**
- Complete feature set (10/10)
- Multi-layer caching strategy (3 separate Cachex instances)
- Lower infrastructure costs (no Redis needed)
- Built-in fault tolerance and hot code reloading
- Event queue processor for fast ingestion

**Best For:** Production deployments, high scale (>500 req/s), lower ops cost

📖 [Full Documentation](zapier_elixir/zapier_triggers/README.md) | [API Docs](http://localhost:4000/api/docs)

### 🦀 Rust (Axum)
**Status**: Production Ready | **Port**: 8080

Ultra-high-performance implementation with dual-index authentication caching and lock-free concurrency.

**Tech Stack:**
- Axum + Tokio
- PostgreSQL (SQLx)
- DashMap (lock-free concurrent hash map)
- Argon2id password hashing

**Strengths:**
- Exceptional performance 🏆
- Dual-index auth cache (hashed + plaintext keys for zero-hashing fast path)
- Zero-cost abstractions
- Memory safety guarantees
- Lock-free concurrent operations (DashMap eliminates contention)
- LRU eviction with TTL expiration

**Best For:** Maximum performance, minimal resources, high-scale production (>10,000 req/s)

📖 [Full Documentation](zapier_rust/README.md)

### 🎨 Common Lisp (SBCL)
**Status**: Production Ready | **Port**: 5001

Dual-implementation approach with both async and synchronous architectures, featuring connection pooling and thread-safe caching.

**Tech Stack:**
- SBCL (Steel Bank Common Lisp)
- Woo (async/event-driven) OR Hunchentoot (thread-per-request)
- Postmodern (PostgreSQL with connection pooling)
- Bordeaux Threads (thread safety)
- Yason (JSON)

**Strengths:**
- Excellent performance 🥈 (119 req/s async, 70 req/s sync, 2,733 req/s health checks)
- Two implementations: Woo (production-recommended) and Hunchentoot (simple/reliable)
- Thread-safe connection pool (10 connections with on-demand creation)
- Dual-layer deduplication (in-memory cache + DB constraints)
- SBCL's high-quality native code generation
- Direct SQL (no ORM overhead)
- Lock-based thread safety for all shared state

**Best For:** Medium-traffic APIs (<10,000 req/s), teams with Lisp expertise, straightforward architectures

📖 [Full Documentation](zapier_common_lisp/README.md)

## Quick Start

### Prerequisites

Choose your implementation:
- **Python**: Python 3.12+, UV, PostgreSQL, Redis
- **Elixir**: Elixir 1.14+, PostgreSQL
- **Rust**: Rust 1.70+, PostgreSQL
- **Common Lisp**: SBCL 2.0+, PostgreSQL, Quicklisp

### Setup All Implementations

```bash
# Setup each implementation
./scripts/setup-python.sh
./scripts/setup-elixir.sh
./scripts/setup-rust.sh
./scripts/setup-commonlisp.sh

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

# Common Lisp
cd zapier_common_lisp
sbcl --load start-server.lisp
# Visit: http://localhost:5001
```

## Testing & Benchmarking

**⚠️ Important**: All testing and benchmarking should be done through the **Unified Test Suite** to ensure consistent, apples-to-apples comparisons across implementations.

See the [Unified Test Suite](#unified-test-suite) section below for:
- Running functional tests on individual implementations
- Performance benchmarking with consistent methodology
- Load testing with Locust
- Side-by-side comparison reports

Individual implementations may have their own benchmark scripts for development purposes, but official performance comparisons should use the unified suite.

## Unified Test Suite

Comprehensive testing framework for comparing all implementations.

### Quick Start

```bash
cd unified_test_suite

# Install dependencies
uv sync
```

### Test Individual Implementation

**Ensure the server is running first**, then run tests:

```bash
# Test specific implementation
./run_tests.sh --impl python      # Python on localhost:8000
./run_tests.sh --impl elixir      # Elixir on localhost:4000
./run_tests.sh --impl rust        # Rust on localhost:8080
./run_tests.sh --impl commonlisp  # Common Lisp on localhost:5001

# Test with custom URLs
./run_tests.sh --impl python --python-url http://localhost:8001
./run_tests.sh --impl rust --rust-url http://localhost:8090

# Run specific test types
./run_tests.sh --impl python --type functional   # Functional tests only
./run_tests.sh --impl rust --type performance    # Performance benchmark
./run_tests.sh --impl elixir --type load         # Load testing (opens UI)
```

**Using pytest directly** (alternative):

```bash
# Functional tests
uv run pytest tests/test_functional.py -v

# With custom base URL
TEST_PYTHON_BASE_URL=http://localhost:8001 uv run pytest tests/test_functional.py -v
```

### Run All Tests & Compare

**Prerequisites**: Start all servers you want to test:

```bash
# Terminal 1: Python
cd zapier_python && uv run uvicorn src.zapier_triggers_api.main:app --port 8000

# Terminal 2: Elixir
cd zapier_elixir/zapier_triggers && mix phx.server

# Terminal 3: Rust
cd zapier_rust && cargo run --release

# Terminal 4: Common Lisp
cd zapier_common_lisp && sbcl --load start-server.lisp
```

**Run tests across all implementations:**

```bash
cd unified_test_suite

# Functional correctness tests (all implementations)
./run_tests.sh --impl all --type functional

# Performance benchmarks (all implementations)
./run_tests.sh --impl all --type performance

# Complete test suite (functional + performance)
./run_tests.sh --impl all --type all

# Interactive load testing (opens web UI at localhost:8089)
./run_tests.sh --impl python --type load
```

### View Results

Test results are saved in `unified_test_suite/results/`:
- `test_results_YYYYMMDD_HHMMSS.json` - Individual test results
- `comparison_report_YYYYMMDD_HHMMSS.html` - Side-by-side comparison

**Features:**
- ✅ 16 functional correctness tests
- ✅ Performance benchmarking (throughput, latency, P50/P95/P99)
- ✅ Load testing with Locust (interactive UI)
- ✅ Realistic test data generation
- ✅ Automatic side-by-side comparison reports

📖 [Test Suite Documentation](unified_test_suite/README.md)

## Performance Comparison

**Note**: Comprehensive benchmarking in progress. Results will be updated soon.

| Implementation | P50 | P95 | P99 | Throughput | Status |
|---------------|-----|-----|-----|------------|--------|
| **Rust** 🏆 | TBD | TBD | TBD | TBD | Benchmark Pending |
| **Common Lisp** 🥈 | TBD | TBD | TBD | TBD | Benchmark Pending |
| **Python** 🥉 | TBD | TBD | TBD | TBD | Benchmark Pending |
| **Elixir** ⚠️ | TBD | TBD | TBD | TBD | Benchmark Pending |

**Key Features:**
- **Rust**: Dual-index auth cache (hashed + plaintext), DashMap lock-free concurrency, LRU+TTL eviction
- **Common Lisp**: Dual implementations (Woo async + Hunchentoot sync), thread-safe connection pooling (10 conns), dual-layer dedup
- **Python**: Plaintext cache optimization, async patterns
- **Elixir**: Triple Cachex caching (dedup + auth + event queue), BEAM VM concurrency, event queue processor

📊 [Detailed Comparison](COMPARISON_SUMMARY.md) | [Session Logs](log_docs/)

## Feature Comparison

| Feature | Python | Elixir | Rust | Common Lisp |
|---------|--------|--------|------|-------------|
| Event Ingestion | ✅ | ✅ | ✅ | ✅ |
| Webhook Delivery | ✅ | ✅ | ✅ | ✅ |
| Rate Limiting | 🟡 Basic | ✅ 4-tier | ✅ 3-tier | ✅ 3-tier |
| Deduplication | ❌ | ✅ 24hr | ✅ Cache | ✅ Cache+DB |
| API Key Rotation | ❌ | ✅ | ❌ | ❌ |
| Payload Limits | ⚠️ | ✅ 256KB | ✅ 256KB | ✅ 256KB |
| OpenAPI/Swagger | ✅ | ✅ | ⚠️ | ⚠️ |
| Prometheus Metrics | ⚠️ | ✅ | ⚠️ | ⚠️ |
| Health Checks | ✅ Basic | ✅ Live+Ready | ✅ Basic | ✅ Basic |
| Test Compatibility | 🟡 Partial | ✅ 16/16 | ✅ 12/16 | ✅ 16/16 |

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
├── zapier_rust/            # Rust (Axum) implementation
│   ├── src/                # Source code
│   ├── tests/              # Unit tests
│   └── README.md           # Rust-specific docs
│
├── zapier_common_lisp/     # Common Lisp (SBCL) implementation
│   ├── simple-server.lisp  # Main server implementation
│   ├── start-simple-server.lisp  # Server startup script
│   └── README.md           # Common Lisp-specific docs
│
├── unified_test_suite/     # Cross-implementation testing
│   ├── tests/              # Functional + performance tests
│   ├── data/               # Test data generator
│   └── README.md           # Test suite documentation
│
├── log_docs/               # Session logs and progress tracking
│   ├── current_progress.md # Living snapshot of project state
│   └── PROJECT_LOG_*.md    # Detailed session documentation
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
# Common Lisp tests via unified test suite

# Unified test suite (tests all implementations)
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
- ✅ Very high scale (>10,000 req/s)

### Choose Common Lisp If:
- ✅ Need excellent performance
- ✅ Want simple, maintainable architecture
- ✅ Team has Lisp expertise
- ✅ Medium-traffic applications (<10,000 req/s)
- ✅ Prefer straightforward synchronous model
- ✅ Value predictable latency profiles

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development workflow
- Code style guidelines
- Testing requirements
- PR process

## Resources

- **Original Spec**: [project_spec.md](project_spec.md)
- **Performance Analysis**: [COMPARISON_SUMMARY.md](COMPARISON_SUMMARY.md)
- **Session Logs**: [log_docs/](log_docs/) - Detailed development logs
- **Test Suite Guide**: [unified_test_suite/README.md](unified_test_suite/README.md)
- **Python Docs**: [zapier_python/README.md](zapier_python/README.md)
- **Elixir Docs**: [zapier_elixir/zapier_triggers/README.md](zapier_elixir/zapier_triggers/README.md)
- **Rust Docs**: [zapier_rust/README.md](zapier_rust/README.md)
- **Common Lisp Docs**: [zapier_common_lisp/README.md](zapier_common_lisp/README.md)

## License

MIT

## Acknowledgments

Built as a comparative study of modern web frameworks and programming paradigms for event-driven systems.

---

**Quick Links:**
- Python API: http://localhost:8000/docs
- Elixir API: http://localhost:4000/api/docs
- Rust API: http://localhost:8080
- Common Lisp API: http://localhost:5001
- Test Suite: [unified_test_suite/](unified_test_suite/)
- Session Logs: [log_docs/](log_docs/)
- Comparison: [COMPARISON_SUMMARY.md](COMPARISON_SUMMARY.md)
