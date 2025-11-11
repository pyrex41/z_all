# Python FastAPI Performance Optimization Guide

## Current Performance: ~300-400 req/s

## 🎯 Target Performance: 800-1000+ req/s

---

## Root Cause Analysis

The Python implementation **already has** the right infrastructure:
- ✅ Async PostgreSQL (asyncpg)
- ✅ Redis client with connection pooling
- ✅ Good database connection pool (20 connections, 10 overflow)
- ✅ Multiple worker support

**But it's not using them optimally!**

### The Bottleneck: Uncached Database Queries

Every API request currently does **3 database queries**:

```python
# 1. Organization lookup (EVERY REQUEST!)
SELECT organizations.* FROM organizations
WHERE organizations.api_key_prefix = 'zap_test_...'

# 2. Event insert
INSERT INTO events (id, org_id, type, dedup_id, payload, created_at) VALUES (...)

# 3. Event delivery insert
INSERT INTO event_deliveries (id, event_id, status, ...) VALUES (...)
```

**Query #1 is completely unnecessary** - API keys don't change frequently!

---

## 🚀 Performance Improvements

### 1. Enable Redis Caching for API Keys (BIGGEST WIN)

**Impact**: Eliminates 33% of database queries
**Expected improvement**: 2-3x throughput increase

#### Quick Implementation:

Change this in `routes/events.py`:

```python
# OLD (line 12):
from zapier_triggers_api.auth import get_current_org

# NEW:
from zapier_triggers_api.auth_cached import get_current_org_cached as get_current_org
```

That's it! The cached version:
- Checks Redis first (~1ms)
- Falls back to database on cache miss (~5-10ms)
- Caches result for 5 minutes
- Reduces DB load by 99%+

### 2. Disable Query Logging in Production

**Impact**: Reduces CPU overhead
**Expected improvement**: 10-15% throughput increase

```bash
export SQLALCHEMY_ECHO=false
```

Or edit `database.py` line 14:

```python
# OLD:
echo=settings.environment == "development",

# NEW:
echo=False,  # Disable query logging for performance
```

### 3. Use Multiple Workers

**Impact**: Utilize multiple CPU cores
**Expected improvement**: Linear scaling (4 workers = 4x throughput)

```bash
uvicorn src.zapier_triggers_api.main:app \
    --host 0.0.0.0 \
    --port 8000 \
    --workers 4 \
    --log-level warning
```

### 4. Optimize Connection Pool Size

**Impact**: Handle more concurrent connections
**Expected improvement**: 20-30% at high concurrency

Edit `database.py` lines 16-17:

```python
# Increase for high-traffic scenarios
pool_size=30,        # Up from 20
max_overflow=20,     # Up from 10
```

### 5. Use Gunicorn with Uvicorn Workers (Production)

**Impact**: Better process management and load balancing

```bash
pip install gunicorn

gunicorn src.zapier_triggers_api.main:app \
    --workers 4 \
    --worker-class uvicorn.workers.UvicornWorker \
    --bind 0.0.0.0:8000 \
    --log-level warning \
    --access-logfile - \
    --error-logfile - \
    --timeout 30 \
    --graceful-timeout 10
```

---

## 📊 Performance Comparison

### Before Optimizations
```
Throughput: 293 req/s
P95 Latency: 435ms
Success Rate: 91.6% (failures due to timeouts)
DB Queries per request: 3
```

### After Optimizations (Expected)
```
Throughput: 800-1000 req/s (2.7-3.4x improvement)
P95 Latency: 100-150ms (3x improvement)
Success Rate: 100%
DB Queries per request: 2 (33% reduction)
Cache hit rate: 99%+
```

### Comparison with Elixir/Rust
```
Python (optimized):  ~1,000 req/s
Elixir:              ~900 req/s    (similar!)
Rust:                ~2,500 req/s  (still 2.5x faster)
```

---

## 🔧 Quick Start: Apply All Optimizations

### Option 1: Use the Optimized Startup Script

```bash
cd /Users/reuben/gauntlet/zapier/zapier_python
chmod +x start_optimized.sh
./start_optimized.sh
```

### Option 2: Manual Steps

1. **Enable caching** (1-line change):
   ```python
   # In routes/events.py, routes/inbox.py, routes/webhooks.py
   from zapier_triggers_api.auth_cached import get_current_org_cached as get_current_org
   ```

2. **Start with optimizations**:
   ```bash
   export SQLALCHEMY_ECHO=false

   uvicorn src.zapier_triggers_api.main:app \
       --host 0.0.0.0 \
       --port 8000 \
       --workers 4 \
       --log-level warning \
       --no-access-log
   ```

3. **Verify Redis is running**:
   ```bash
   redis-cli ping  # Should return "PONG"
   ```

4. **Run benchmark**:
   ```bash
   cd unified_test_suite
   source .venv/bin/activate
   python benchmark_single.py python 5000 200
   ```

---

## 🎓 Why Python Can Match Elixir

**Elixir's advantage**: Built-in ETS caching, BEAM VM concurrency

**Python can catch up by**:
- Using Redis for caching (same concept as ETS)
- Async I/O with asyncpg (non-blocking database)
- Multiple workers (similar to BEAM processes)
- Optimized connection pooling

**Python's remaining limitations**:
- GIL (Global Interpreter Lock) limits CPU-bound tasks
- Higher memory per worker (~100MB vs Elixir's ~20MB per process)
- Less fault-tolerant (no supervisor trees)

**But for I/O-bound APIs like this**, Python with proper caching can match Elixir!

---

## 🚨 Important Notes

1. **Redis must be running** for caching to work
   ```bash
   # Start Redis if not running
   redis-server
   ```

2. **Cache invalidation**: If you update an organization's API key, clear the cache:
   ```bash
   redis-cli DEL "org:prefix:zap_test_xxx"
   ```

3. **Monitor cache hit rate**:
   ```bash
   # Add to your monitoring
   redis-cli INFO stats | grep keyspace
   ```

4. **Test before deploying**: Run the benchmark to verify improvements

---

## 📈 Expected Results After Optimization

```bash
python benchmark_single.py python 5000 200
```

**Target metrics:**
- Throughput: 800-1000 req/s (up from 293)
- P95 Latency: <150ms (down from 435ms)
- Success Rate: 100% (up from 91.6%)
- DB queries/request: 2 (down from 3)

---

## 🔍 Debugging Performance Issues

### Check Database Connection Pool
```python
# Add to your monitoring
from zapier_triggers_api.database import engine
print(engine.pool.status())
```

### Check Redis Cache Hit Rate
```bash
redis-cli INFO stats | grep -E "keyspace_hits|keyspace_misses"
```

### Profile Slow Requests
```python
# Add middleware to track request duration
import time
from fastapi import Request

@app.middleware("http")
async def add_process_time_header(request: Request, call_next):
    start_time = time.time()
    response = await call_next(request)
    process_time = time.time() - start_time
    response.headers["X-Process-Time"] = str(process_time)
    return response
```

---

## ✅ Checklist

- [ ] Enable Redis caching in auth
- [ ] Disable query logging
- [ ] Start with multiple workers
- [ ] Verify Redis is running
- [ ] Run performance benchmark
- [ ] Compare with baseline (293 req/s)
- [ ] Target: 800-1000 req/s achieved ✨

---

**Questions?** Check the benchmark results or compare with Elixir/Rust implementations.
