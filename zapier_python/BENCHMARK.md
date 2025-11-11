# Performance Benchmark

Comprehensive benchmark suite for the Zapier Triggers API.

## Quick Start

### 1. Setup Test Organization (Local Development)

```bash
# Start the stack
docker-compose up -d

# Run migrations
uv run alembic upgrade head

# Create benchmark organization
uv run python setup_benchmark_org.py
```

This creates a test organization with:
- API Key: `zap_test_benchmark_key_for_load_testing_purposes_only_12345`
- Rate Limit: 100,000/min (Enterprise tier)
- Webhook: `http://httpbin.org/post`

### 2. Run Benchmarks

**Quick Test (100 requests, 10 concurrent, 10s mixed workload):**
```bash
uv run python benchmark.py --quick
```

**Standard Test (1000 requests, 50 concurrent, 30s mixed workload):**
```bash
uv run python benchmark.py
```

**Custom Load Profile:**
```bash
uv run python benchmark.py \
  --requests 5000 \
  --concurrency 100 \
  --duration 60 \
  --rps 500
```

**Against Production:**
```bash
uv run python benchmark.py \
  --url https://api.yourcompany.com \
  --api-key zap_live_your_production_key \
  --requests 10000 \
  --concurrency 200
```

## Benchmark Tests

### 1. Event Ingestion (POST /events)
Tests the core ingestion pipeline with configurable concurrency.

**Measures:**
- Throughput (requests/second)
- Latency distribution (avg, p50, p95, p99, min, max)
- Error rate
- PRD target: p95 < 100ms

### 2. Inbox Retrieval (GET /inbox)
Tests paginated event retrieval with 100 events per page.

**Measures:**
- Query performance
- Pagination efficiency
- Latency under load

### 3. Mixed Workload
Simulates realistic usage with:
- 70% event ingestion
- 20% inbox retrieval
- 10% acknowledgments

**Measures:**
- Sustained throughput over time
- Per-operation latencies
- System stability under mixed load

## Example Output

```
🚀 Zapier Triggers API Benchmark
====================================================================================================
Target:       http://localhost:8000
API Key:      zap_test_benchmark_k...
Requests:     1,000
Concurrency:  50
====================================================================================================

🔥 Benchmarking event ingestion: 1000 requests, 50 concurrent
📥 Benchmarking inbox retrieval: 1000 requests, 50 concurrent
⚡ Benchmarking mixed workload: 100 RPS for 30s

====================================================================================================
📊 BENCHMARK RESULTS
====================================================================================================

POST /events
----------------------------------------------------------------------------------------------------
Total Requests:    1,000
Successful:        1,000 (100.0%)
Failed:            0 (0.00%)
Duration:          12.34s
Throughput:        81.0 req/s

Latency (ms):
  Average:         45.23
  P50:             42.10
  P95:             78.50
  P99:             95.20
  Min:             15.30
  Max:             145.60

GET /inbox
----------------------------------------------------------------------------------------------------
Total Requests:    1,000
Successful:        1,000 (100.0%)
Failed:            0 (0.00%)
Duration:          8.45s
Throughput:        118.3 req/s

Latency (ms):
  Average:         32.15
  P50:             30.20
  P95:             52.30
  P99:             68.40
  Min:             12.10
  Max:             89.20

====================================================================================================

📈 SUMMARY
----------------------------------------------------------------------------------------------------
Total Requests:    3,000
Total Duration:    50.79s
Avg Throughput:    99.7 req/s
Avg P95 Latency:   65.40ms

🎯 PRD TARGET COMPARISON
----------------------------------------------------------------------------------------------------
Ingestion P95 < 100ms:  78.50ms ✅ PASS
====================================================================================================
```

## Performance Targets (PRD)

| Metric | Target | Test Coverage |
|--------|--------|---------------|
| Ingestion Latency (p95) | < 100ms | ✅ POST /events benchmark |
| Delivery Latency (p95) | < 5s | ⚠️ Requires webhook mock server |
| Throughput | 1K+ events/sec/instance | ✅ Mixed workload test |
| Uptime | 99.9% | ⚠️ Requires extended load test |
| Error Rate | < 0.5% | ✅ All benchmarks |

## Options

```
--url URL              API base URL (default: http://localhost:8000)
--api-key KEY          API key for authentication
--requests N           Number of requests per test (default: 1000)
--concurrency N        Concurrent requests (default: 50)
--duration SECONDS     Duration for mixed workload (default: 30)
--rps N               Target requests/sec for mixed (default: 100)
--quick               Quick test mode (100 req, 10 concurrent, 10s)
```

## Requirements

The benchmark uses:
- `httpx` for async HTTP requests
- Native Python `asyncio` for concurrency
- Built-in `statistics` for latency calculations

All dependencies are already in the project via `uv`.

## Tips

1. **Warm-up**: Run with `--quick` first to warm up connections
2. **Database**: Ensure PostgreSQL connection pool is sized appropriately
3. **Rate Limits**: Use enterprise tier for load testing (100K/min)
4. **Network**: For accurate results, minimize network latency between client and server
5. **Resources**: Monitor CPU/memory during benchmarks with `docker stats`

## CI/CD Integration

Add to your pipeline:

```yaml
- name: Performance Benchmark
  run: |
    uv run python setup_benchmark_org.py
    uv run python benchmark.py --quick
    # Fail if p95 > 100ms threshold
```
