# Unified Test Suite - Quick Reference

## What Was Built

A comprehensive testing framework that can:
- ✅ Test both Python and Elixir implementations
- ✅ Generate realistic test data (10 event types)
- ✅ Run functional correctness tests (15+ test cases)
- ✅ Measure performance (throughput, latency, percentiles)
- ✅ Load test with Locust (interactive web UI)
- ✅ Compare implementations side-by-side
- ✅ Generate detailed reports

## Quick Start (3 commands)

```bash
# 1. Install dependencies
cd unified_test_suite && uv sync

# 2. Start your API servers (in separate terminals)
cd zapier_python && uv run uvicorn src.zapier_triggers_api.main:app --reload
cd zapier_elixir/zapier_triggers && mix phx.server

# 3. Run tests
cd unified_test_suite && ./run_tests.sh
```

## Common Commands

```bash
# Functional tests (verify correctness)
./run_tests.sh --type functional

# Performance benchmark (compare speed)
./run_tests.sh --type performance

# Load test (interactive UI at http://localhost:8089)
./run_tests.sh --type load

# Generate test data
uv run python data/generator.py

# Using Make
make install      # Install dependencies
make test         # Run all tests
make functional   # Functional tests only
make performance  # Performance benchmark
make load         # Load testing UI
```

## What You'll See

### Functional Tests
```
tests/test_functional.py::TestAPIKeyManagement::test_generate_api_key[python] ✓
tests/test_functional.py::TestAPIKeyManagement::test_generate_api_key[elixir] ✓
tests/test_functional.py::TestEventIngestion::test_create_single_event[python] ✓
tests/test_functional.py::TestEventIngestion::test_create_single_event[elixir] ✓
...
15 passed in 3.42s
```

### Performance Benchmark
```
┏━━━━━━━━━━━━━━━━┳━━━━━━━━━━┳━━━━━━━━━━┓
┃ Metric         ┃ Python   ┃ Elixir   ┃
┡━━━━━━━━━━━━━━━━╇━━━━━━━━━━╇━━━━━━━━━━┩
│ Requests/sec   │ 245.33   │ 892.17   │
│ P95 Latency    │ 198.32ms │ 52.18ms  │
│ Error Rate     │ 0.00%    │ 0.00%    │
└────────────────┴──────────┴──────────┘

✓ Throughput winner: ELIXIR (264% higher)
✓ Latency winner: ELIXIR (74% lower)
```

### Load Test
- Open http://localhost:8089
- Configure users and spawn rate
- Watch real-time charts
- See request statistics

## Test Data Generated

The generator creates realistic events for:
- **Users**: created, updated, deleted (with addresses, preferences)
- **Orders**: placed, fulfilled, cancelled (with items, shipping)
- **Payments**: succeeded, failed (with error codes)
- **Subscriptions**: started, cancelled (with billing info)

Output files:
- `test_data_sample.json` (10 events - for inspection)
- `test_data_small.json` (100 events)
- `test_data_medium.json` (1,000 events)
- `test_data_large.json` (10,000 events)

## Test Coverage

### Functional Tests Cover:
- ✅ API key generation & management
- ✅ Event ingestion (single & batch)
- ✅ Event deduplication (Elixir)
- ✅ Inbox listing & pagination
- ✅ Rate limiting enforcement
- ✅ Webhook configuration
- ✅ Payload size limits
- ✅ Error handling (401, 409, 413, 422, 429)
- ✅ Health checks

### Performance Tests Measure:
- ✅ Throughput (requests/sec)
- ✅ Latency (avg, p50, p95, p99, min, max)
- ✅ Success/failure rates
- ✅ Error rates
- ✅ Concurrent request handling

## Key Features

### 1. Unified API Client
Single interface works with both implementations:
```python
from tests.api_client import APIClient

# Auto-detects Python or Elixir
client = APIClient("http://localhost:4000")
client.generate_api_key()
response, latency = client.create_event(event)
```

### 2. Parametrized Testing
Single test runs against both:
```python
def test_event_creation(any_client):
    # Runs twice: once for Python, once for Elixir
    response = any_client.create_event(event)
    assert response.status_code == 201
```

### 3. Rich Reporting
- Console: Rich tables with colors
- JSON: Machine-readable results
- Locust: Interactive web UI
- Reports saved in `reports/`

## File Organization

```
unified_test_suite/
├── config/          # Configuration (Pydantic settings)
├── data/            # Test data generator (Faker-based)
├── tests/           # All test suites
│   ├── api_client.py      # Unified client
│   ├── test_functional.py # Correctness tests
│   ├── test_performance.py # Locust scenarios
│   └── benchmark.py        # Comparison tool
├── reports/         # Generated results
└── [docs]           # README, QUICK_START, FEATURES
```

## Configuration

Create `.env.test`:
```bash
TEST_IMPLEMENTATION=both  # or python, elixir
TEST_PYTHON_BASE_URL=http://localhost:8000
TEST_ELIXIR_BASE_URL=http://localhost:4000
```

Or use command-line flags:
```bash
./run_tests.sh --impl python --python-url http://custom-url:8000
```

## Expected Results

Based on the comparison analysis:

### Python Implementation
- Throughput: ~200-400 req/sec
- P95 Latency: ~100-200ms
- Good for: Prototyping, moderate scale

### Elixir Implementation
- Throughput: ~600-1200 req/sec
- P95 Latency: ~30-70ms
- Good for: Production, high scale

**Performance Winner**: Elixir (3-4x faster)

## Troubleshooting

### APIs not accessible
```bash
# Check if running
curl http://localhost:8000/health
curl http://localhost:4000/health

# Start them
cd zapier_python && uv run uvicorn src.zapier_triggers_api.main:app --reload
cd zapier_elixir/zapier_triggers && mix phx.server
```

### Dependencies issue
```bash
cd unified_test_suite
rm -rf .venv
uv sync
```

### Rate limiting in tests
Tests use enterprise tier (100K/min) automatically.
If still hitting limits, reduce concurrency:
```bash
uv run python tests/benchmark.py --requests 1000 --concurrency 10
```

## Documentation

- **README.md** - Full documentation (9KB)
- **QUICK_START.md** - 5-minute guide (4KB)
- **FEATURES.md** - Feature overview (10KB)
- **COMPARISON_SUMMARY.md** - Implementation comparison (in parent dir)

## Next Steps

1. **Run functional tests** to verify both implementations work
2. **Run performance tests** to see the speed difference
3. **Try load testing** with Locust UI
4. **Generate test data** to inspect event formats
5. **Read the docs** for advanced usage

## Support

Check the documentation:
```bash
cat unified_test_suite/README.md
cat unified_test_suite/QUICK_START.md
cat unified_test_suite/FEATURES.md
```

Or run:
```bash
./run_tests.sh --help
```

---

**Created**: 2025-11-10
**Location**: `/Users/reuben/gauntlet/zapier/unified_test_suite/`
**Status**: Ready to use ✅
