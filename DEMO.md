# Zapier Triggers API - Performance Demo Guide

**Quick demo guide to showcase the performance of all four implementations**

---

## 🎯 What This Demo Shows

This demo runs sequential performance benchmarks against all four implementations:
- **Rust** (Axum) - Ultra-low latency champion
- **Python** (FastAPI) - Excellent async performance
- **Common Lisp** (Woo) - Simple synchronous with great compiler
- **Elixir** (Phoenix) - Fault-tolerant BEAM VM

Each benchmark sends 2,000 requests and measures P50, P95, P99 latency and throughput.

---

## 📋 Prerequisites

**Required:**
- PostgreSQL running on localhost:5432
- Redis running on localhost:6379
- All dependencies installed for each implementation

**Check services are running:**
```bash
# Check PostgreSQL
psql -h localhost -p 5432 -U postgres -c "SELECT 1"

# Check Redis
redis-cli ping
# Should return: PONG
```

---

## 🚀 Demo Instructions

### Demo 1: Rust Implementation (Champion - 1.40ms P95)

**Step 1: Start Rust server**
```bash
cd zapier_rust
cargo run --release
```

**Wait for:** `Server listening on 0.0.0.0:8090`

**Step 2: Run benchmark (in new terminal)**
```bash
cd unified_test_suite
python benchmark_single.py rust 2000 1
```

**Expected Results:**
```
P50:     ~0.74ms
P95:     ~1.40ms  ⭐ BEST
P99:     ~2.06ms
Throughput: ~1,100 req/s
```

**Step 3: Stop server**
```bash
# Press Ctrl+C in the Rust server terminal
```

---

### Demo 2: Python Implementation (Excellent - 3.88ms P95)

**Step 1: Start Python server**
```bash
cd zapier_python
uv run uvicorn zapier_triggers_api.main:app --host 127.0.0.1 --port 8001
```

**Wait for:** `Uvicorn running on http://127.0.0.1:8001`

**Step 2: Verify server is responding**
```bash
curl http://127.0.0.1:8001/health
# Should return: {"status":"healthy"}
```

**Step 3: Run benchmark (use simple script)**
```bash
cd unified_test_suite
python /tmp/bench_single.py http://127.0.0.1:8001 Python
```

**Note:** The unified benchmark requires API key generation which may fail. Use the simple benchmark script instead.

**Expected Results:**
```
P50:     ~2.79ms
P95:     ~3.88ms  ⭐ EXCELLENT
P99:     ~5.45ms
Throughput: ~340 req/s
```

**Step 4: Stop server**
```bash
# Press Ctrl+C in the Python server terminal
```

---

### Demo 3: Common Lisp Implementation (Very Good - 6.90ms P95)

**Step 1: Start Common Lisp server**
```bash
cd zapier_common_lisp
timeout 10 sbcl --load "simple-server.lisp" &
```

**Wait for:** `Server started on port 5001`

**Step 2: Verify server is responding**
```bash
curl http://127.0.0.1:5001/health
# Should return: {"status":"healthy"}
```

**Step 3: Run benchmark (use simple script)**
```bash
cd unified_test_suite
python /tmp/bench_single.py http://127.0.0.1:5001 "Common-Lisp"
```

**Expected Results:**
```
P50:     ~4.01ms
P95:     ~6.90ms  ⭐ VERY GOOD
P99:     ~12.19ms
Throughput: ~225 req/s
```

**Step 4: Stop server**
```bash
# Kill the SBCL process
pkill -f sbcl
```

---

### Demo 4: Elixir Implementation (Meets Requirements - 52.97ms P95)

**Step 1: Start Elixir server**
```bash
cd zapier_elixir/zapier_triggers
mix phx.server
```

**Wait for:** `[info] Running ZapierTriggersWeb.Endpoint`

**Step 2: Verify server is responding**
```bash
curl http://127.0.0.1:4000/health
# Should return: {"status":"healthy"}
```

**Step 3: Run benchmark**
```bash
cd unified_test_suite
python /tmp/bench_single.py http://127.0.0.1:4000 Elixir
```

**Expected Results:**
```
P50:     ~44ms
P95:     ~52.97ms  ✅ MEETS PRD
P99:     ~69ms
Throughput: ~22 req/s
```

**Why slower?** This is expected BEAM VM overhead (see PERFORMANCE_ANALYSIS_2025-11-12.md). Elixir optimizes for fault-tolerance over raw speed.

**Step 4: Stop server**
```bash
# Press Ctrl+C in the Elixir server terminal
```

---

## 📊 Quick Comparison

| Implementation | P95 Latency | vs Rust | vs PRD (<100ms) |
|---------------|-------------|---------|-----------------|
| **Rust** | 1.40ms | 1x | 71x better |
| **Python** | 3.88ms | 2.77x | 25.7x better |
| **Common Lisp** | 6.90ms | 4.93x | 14.5x better |
| **Elixir** | 52.97ms | 37.8x | 1.9x better |

**All implementations exceed PRD requirements!** ✅

---

## 🎬 Full Demo Script (Run All Sequentially)

If you want to run all benchmarks back-to-back:

```bash
#!/bin/bash
# Full performance demo

echo "=== Rust Benchmark ==="
cd zapier_rust && cargo run --release &
RUST_PID=$!
sleep 10
cd ../unified_test_suite && python benchmark_single.py rust 2000 1
kill $RUST_PID
sleep 2

echo "=== Python Benchmark ==="
cd ../zapier_python && uv run uvicorn zapier_triggers_api.main:app --port 8001 &
PYTHON_PID=$!
sleep 5
cd ../unified_test_suite && python /tmp/bench_single.py http://127.0.0.1:8001 Python
kill $PYTHON_PID
sleep 2

echo "=== Common Lisp Benchmark ==="
cd ../zapier_common_lisp && timeout 10 sbcl --load "simple-server.lisp" &
LISP_PID=$!
sleep 5
cd ../unified_test_suite && python /tmp/bench_single.py http://127.0.0.1:5001 "Common-Lisp"
pkill -f sbcl
sleep 2

echo "=== Elixir Benchmark ==="
cd ../zapier_elixir/zapier_triggers && mix phx.server &
ELIXIR_PID=$!
sleep 10
cd ../../unified_test_suite && python /tmp/bench_single.py http://127.0.0.1:4000 Elixir
kill $ELIXIR_PID

echo "=== Demo Complete ==="
```

---

## 🔍 What to Look For

**During Rust benchmark:**
- ✅ Very fast responses (~1ms)
- ✅ Consistent performance
- ✅ No GC pauses
- ✅ 1,100+ req/s throughput

**During Python benchmark:**
- ✅ Fast async I/O (~3-4ms)
- ✅ Redis Streams working
- ✅ Immediate 202 responses
- ✅ 340 req/s throughput

**During Common Lisp benchmark:**
- ✅ Simple synchronous design (~7ms)
- ✅ SBCL compiler efficiency
- ✅ Fast DB round-trips (4-7ms)
- ✅ 225 req/s throughput

**During Elixir benchmark:**
- ✅ Slower but consistent (~53ms)
- ✅ Cache-first working correctly
- ✅ BEAM VM overhead visible
- ✅ 22 req/s throughput

---

## 📖 Detailed Analysis

For comprehensive performance analysis and architectural trade-offs, see:
- **`log_docs/PERFORMANCE_ANALYSIS_2025-11-12.md`** - 14,000+ word deep-dive
- **`log_docs/current_progress.md`** - Project status and findings

---

## 🐛 Troubleshooting

**Issue: Benchmark script not found**
```bash
# Create the simple benchmark script if missing
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

**Issue: Server won't start**
- Check PostgreSQL is running: `psql -h localhost -U postgres -c "SELECT 1"`
- Check Redis is running: `redis-cli ping`
- Check port is available: `lsof -ti:PORT` (should return nothing)

**Issue: Benchmark fails with 401 Unauthorized**
- The API key `sk_0C9A265F-1A52-4AD9-A377-7E1903750D45` should exist in the database
- Check: `psql zapier_triggers -c "SELECT * FROM organizations;"`

**Issue: Elixir server won't respond on port 4000**
- Check logs for compilation errors
- Verify Phoenix dependencies: `cd zapier_elixir/zapier_triggers && mix deps.get`
- Try restarting: `mix phx.server`

---

## ✅ Success Criteria

You should see:
- ✅ All servers start successfully
- ✅ Health checks return `{"status":"healthy"}`
- ✅ Benchmarks complete with 100% success rate
- ✅ Performance matches expected results (within 20%)
- ✅ Clear performance differences between implementations

---

**Demo Duration:** ~10 minutes (2 minutes per implementation + setup)
**Difficulty:** Easy (just copy-paste commands)
**Prerequisites:** PostgreSQL, Redis, dependencies installed

Enjoy the demo! 🚀
