# Performance Benchmark

Comprehensive benchmark suite for the Zapier Triggers API. **Supports both Python and Elixir implementations** with automatic API detection.

## Quick Start

### 1. Setup Test Organization

#### For Elixir (this repo):

```bash
# Start the Elixir server
mix phx.server

# In another terminal, generate a benchmark API key
curl -X POST http://localhost:4000/api/keys/generate \
  -H "Content-Type: application/json" \
  -d '{
    "organization_name": "Benchmark Org",
    "tier": "enterprise"
  }'

# Save the API key for benchmarking
export BENCHMARK_API_KEY="zap_test_..."
```

#### For Python implementation:

```bash
# Start the Python stack
docker-compose up -d

# Run migrations
uv run alembic upgrade head

# Create benchmark organization
uv run python setup_benchmark_org.py
```

### 2. Run Benchmarks

**Quick Test (100 requests, 10 concurrent):**
```bash
# Elixir (auto-detected)
python benchmark.py --url http://localhost:4000 --api-key $BENCHMARK_API_KEY --quick

# Python (auto-detected)
python benchmark.py --url http://localhost:8000 --api-key zap_test_benchmark --quick
```

**Standard Test (1000 requests, 50 concurrent):**
```bash
# Elixir
python benchmark.py --url http://localhost:4000 --api-key $BENCHMARK_API_KEY

# Python
python benchmark.py --url http://localhost:8000 --api-key zap_test_benchmark
```

**Custom Load Profile:**
```bash
python benchmark.py \
  --url http://localhost:4000 \
  --api-key $BENCHMARK_API_KEY \
  --requests 5000 \
  --concurrency 100 \
  --duration 60 \
  --rps 500
```

**Explicit API Type (skip auto-detection):**
```bash
# Elixir
python benchmark.py --url http://localhost:4000 --api-type elixir --api-key $BENCHMARK_API_KEY

# Python
python benchmark.py --url http://localhost:8000 --api-type python --api-key zap_test_benchmark
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
- 80% event ingestion
- 20% inbox retrieval

**Measures:**
- Sustained throughput over time
- Per-operation latencies
- System stability under mixed load

## API Compatibility

The benchmark tool automatically detects and adapts to different API implementations:

| Feature | Python API | Elixir API | Adapter Handles |
|---------|------------|------------|-----------------|
| Base path | `/events` | `/api/events` | ✅ Auto-routing |
| Payload field | `data` | `payload` | ✅ Auto-mapping |
| Response format | `{"id": "..."}` | `{"id": "...", "status": "...", ...}` | ✅ ID extraction |
| Ack endpoint | Batch `/inbox/ack` | Individual `/api/ack/:id` | ⚠️ Not tested in mixed workload |

## Example Output

```
🚀 Zapier Triggers API Benchmark
====================================================================================================
Target:       http://localhost:4000
API Key:      zap_test_benchmark_k...
Requests:     1,000
Concurrency:  50
====================================================================================================
🔧 Using ELIXIR API adapter

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
--url URL              API base URL (default: http://localhost:4000)
--api-key KEY          API key for authentication
--api-type TYPE        API type: auto, elixir, python (default: auto)
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

Install with:
```bash
pip install httpx
```

Or if using the project's environment:
```bash
uv pip install httpx
```

## Comparing Python vs Elixir

Run benchmarks against both implementations to compare performance:

```bash
# Benchmark Python
python benchmark.py \
  --url http://localhost:8000 \
  --api-key zap_test_benchmark \
  --requests 5000 \
  --concurrency 100 > python_results.txt

# Benchmark Elixir
python benchmark.py \
  --url http://localhost:4000 \
  --api-key $BENCHMARK_API_KEY \
  --requests 5000 \
  --concurrency 100 > elixir_results.txt

# Compare results
diff -y python_results.txt elixir_results.txt
```

## Tips

1. **Warm-up**: Run with `--quick` first to warm up connections
2. **Database**: Ensure PostgreSQL connection pool is sized appropriately
3. **Rate Limits**: Use enterprise tier for load testing (100K/min)
4. **Network**: For accurate results, minimize network latency between client and server
5. **Resources**: Monitor CPU/memory during benchmarks:
   - Elixir: `observer:start()` in iex
   - Python: `docker stats`
6. **API Detection**: Let the tool auto-detect the API type for convenience
7. **Parallel Testing**: Run both servers and benchmark sequentially for side-by-side comparison

## CI/CD Integration

Add to your pipeline:

```yaml
- name: Performance Benchmark (Elixir)
  run: |
    mix phx.server &
    sleep 5
    API_KEY=$(curl -s -X POST http://localhost:4000/api/keys/generate \
      -H "Content-Type: application/json" \
      -d '{"organization_name":"CI","tier":"enterprise"}' | jq -r '.api_key')
    python benchmark.py --url http://localhost:4000 --api-key $API_KEY --quick
    # Fail if p95 > 100ms threshold
```

## Troubleshooting

### Auto-detection fails
Use `--api-type` to explicitly specify the API type:
```bash
python benchmark.py --url http://localhost:4000 --api-type elixir
```

### Connection refused
Ensure the server is running:
```bash
# Elixir
mix phx.server

# Python
docker-compose up -d
```

### Authentication errors
Generate a valid API key first:
```bash
# Elixir
curl -X POST http://localhost:4000/api/keys/generate \
  -H "Content-Type: application/json" \
  -d '{"organization_name":"Benchmark","tier":"enterprise"}'
```

### Rate limit exceeded
Use enterprise tier (100K/min) or reduce concurrency:
```bash
python benchmark.py --concurrency 10 --rps 50
```
