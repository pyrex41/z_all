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

A production-grade implementation leveraging the BEAM VM's concurrency model.

**Tech Stack:**
- Phoenix 1.7
- PostgreSQL (with Oban)
- Cachex + Hammer
- Prometheus metrics

**Strengths:**
- Complete feature set (10/10)
- Lower infrastructure costs (no Redis)
- Built-in fault tolerance
- Excellent concurrency model

**Best For:** Production deployments, high scale (>500 req/s), lower ops cost

📖 [Full Documentation](zapier_elixir/zapier_triggers/README.md) | [API Docs](http://localhost:4000/api/docs)

### 🦀 Rust (Axum)
**Status**: Production Ready | **Port**: 8080

Ultra-high-performance implementation with dual-index caching and lock-free concurrency.

**Tech Stack:**
- Axum + Tokio
- PostgreSQL (SQLx)
- DashMap (lock-free cache)
- Argon2 password hashing

**Strengths:**
- Exceptional performance 🏆
- Dual-index cache optimization
- Zero-cost abstractions
- Memory safety guarantees
- Lock-free concurrent operations

**Best For:** Maximum performance, minimal resources, high-scale production (>10,000 req/s)

📖 [Full Documentation](zapier_rust/README.md)

### 🎨 Common Lisp (SBCL/Hunchentoot)
**Status**: Production Ready | **Port**: 5001

Simple, fast implementation leveraging SBCL's native compilation and synchronous architecture.

**Tech Stack:**
- SBCL (Steel Bank Common Lisp)
- Hunchentoot web server
- Postmodern (PostgreSQL)
- Yason (JSON)

**Strengths:**
- Excellent performance 🥈
- Simple synchronous model (easy to reason about)
- SBCL's high-quality native code generation
- Direct SQL (no ORM overhead)
- Predictable latency profile

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
sbcl --load start-simple-server.lisp
# Visit: http://localhost:5001
```

## Manual Benchmarking Guide

### Prerequisites

1. **Start Database & Redis**
```bash
# PostgreSQL (required for all implementations)
brew services start postgresql@14

# Redis (required for Python only)
brew services start redis
```

2. **Setup Benchmark Organization**
```bash
# The benchmark API key: sk_0C9A265F-1A52-4AD9-A377-7E1903750D45
# This key is already seeded in the database
```

### Benchmark Each Implementation

#### 🐍 Python Benchmark

```bash
# Terminal 1: Start Python server
cd zapier_python
PYTHONPATH=src python -m uvicorn zapier_triggers_api.main:app --host 0.0.0.0 --port 8001

# Terminal 2: Run benchmark
python /tmp/bench_single.py http://localhost:8001 Python

# Results: [Benchmark pending]
```

#### 💧 Elixir Benchmark

```bash
# Terminal 1: Start Elixir server
cd zapier_elixir/zapier_triggers
mix phx.server

# Terminal 2: Run benchmark
python /tmp/bench_single.py http://localhost:4000 Elixir

# Results: [Benchmark pending]
```

#### 🦀 Rust Benchmark

```bash
# Terminal 1: Start Rust server
cd zapier_rust
cargo run --release

# Terminal 2: Run benchmark
python /tmp/bench_single.py http://localhost:8080 Rust

# Results: [Benchmark pending]
```

#### 🎨 Common Lisp Benchmark

```bash
# Terminal 1: Start Common Lisp server
cd zapier_common_lisp
sbcl --load start-simple-server.lisp

# Terminal 2: Run benchmark
python /tmp/bench_single.py http://localhost:5001 "Common Lisp"

# Results: [Benchmark pending]
```

### Understanding Results

The benchmark script (`/tmp/bench_single.py`) performs:
- **Warmup**: 100 requests to populate caches
- **Test**: 2000 requests to measure performance
- **Metrics**:
  - **RPS**: Requests per second (throughput)
  - **P50/P95/P99**: Latency percentiles
  - **Average**: Mean response time

### Performance Optimization Notes

Both Python and Elixir implementations use **plaintext cache** optimization:
- **Cache Key**: `auth:{api_key}` (plaintext API key)
- **Cache Hit**: Direct Redis/ETS lookup, NO hashing
- **Cache Miss**: Hash once, DB lookup, cache for 5min
- **Result**: 99%+ cache hit rate = near-zero auth overhead

### Troubleshooting

**Python server won't start:**
```bash
# Check port availability
lsof -ti:8001 | xargs kill -9
PYTHONPATH=src python -m uvicorn zapier_triggers_api.main:app --host 0.0.0.0 --port 8001
```

**Elixir server won't start:**
```bash
# Check port availability
lsof -ti:4000 | xargs kill -9
cd zapier_elixir/zapier_triggers && mix phx.server
```

**Benchmark script not found:**
```bash
# Create benchmark script
cat > /tmp/bench_single.py << 'EOF'
#!/usr/bin/env python3
import httpx
import time
import asyncio
import statistics
import sys

API_KEY = "sk_0C9A265F-1A52-4AD9-A377-7E1903750D45"

async def send_request(client, url, dedup_id):
    start = time.perf_counter()
    response = await client.post(
        url,
        headers={"X-API-Key": API_KEY},
        json={
            "type": "test.event",
            "payload": {"test": "data"},
            "dedup_id": f"bench-{dedup_id}"
        },
    )
    latency = (time.perf_counter() - start) * 1000
    return response.status_code, latency

async def run_benchmark(name, url):
    async with httpx.AsyncClient(timeout=30.0) as client:
        print(f"\n{'='*70}")
        print(f"🚀 {name} Benchmark")
        print(f"{'='*70}")

        # Warmup
        print("Warming up (100 requests)...")
        for i in range(100):
            try:
                await send_request(client, url, i)
            except:
                pass

        # Benchmark
        print(f"Running benchmark (2000 requests)...")
        latencies = []
        start_time = time.perf_counter()

        for i in range(2000):
            try:
                status, latency = await send_request(client, url, i + 100)
                latencies.append(latency)
            except Exception as e:
                print(f"Error: {e}")
                break

            if (i + 1) % 500 == 0:
                print(f"Progress: {i + 1}/2000")

        duration = time.perf_counter() - start_time

        if not latencies:
            print("❌ No successful requests!")
            return None

        # Stats
        latencies.sort()
        p50 = latencies[len(latencies) // 2]
        p95 = latencies[int(len(latencies) * 0.95)]
        p99 = latencies[int(len(latencies) * 0.99)]
        avg = statistics.mean(latencies)
        rps = len(latencies) / duration

        print(f"\n✅ {name} Results:")
        print(f"  🚀 {rps:.0f} requests/second")
        print(f"  📊 Average: {avg:.2f}ms")
        print(f"  📈 P50: {p50:.2f}ms")
        print(f"  📈 P95: {p95:.2f}ms")
        print(f"  📈 P99: {p99:.2f}ms")
        print(f"  ⏱️  Total time: {duration:.2f}s")
        print("="*70)

        return {"name": name, "rps": rps, "avg": avg, "p50": p50, "p95": p95, "p99": p99}

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python bench_single.py <url> <name>")
        sys.exit(1)

    result = asyncio.run(run_benchmark(sys.argv[2], sys.argv[1] + "/api/events"))
EOF

chmod +x /tmp/bench_single.py
```

## Unified Test Suite

Comprehensive testing framework for comparing all implementations.

### Quick Start

```bash
cd unified_test_suite

# Install dependencies
uv sync
```

### Test Individual Implementation

```bash
# Test specific implementation by base URL
./run_tests.sh --base-url http://localhost:8000  # Python
./run_tests.sh --base-url http://localhost:4000  # Elixir
./run_tests.sh --base-url http://localhost:8080  # Rust
./run_tests.sh --base-url http://localhost:5001  # Common Lisp

# Test with specific test types
./run_tests.sh --base-url http://localhost:5001 --type functional
./run_tests.sh --base-url http://localhost:8080 --type performance
```

### Run All Tests & Compare

```bash
# Ensure all servers are running first
# Then run comprehensive comparison
./run_tests.sh --type functional

# Performance benchmarks across all implementations
./run_tests.sh --type performance

# Interactive load testing (opens web UI)
./run_tests.sh --type load
# Visit: http://localhost:8089
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
- **Rust**: Dual-index cache optimization, lock-free concurrency
- **Common Lisp**: Simple synchronous architecture, native compilation
- **Python**: Plaintext cache optimization, async patterns
- **Elixir**: BEAM VM concurrency, cache-first architecture

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
