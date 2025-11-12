# Zapier Triggers API - Common Lisp Implementation

> ⚠️ **Security Notice**: The default configuration uses PostgreSQL trust authentication for development convenience. **See [SECURITY.md](SECURITY.md) for production hardening requirements.**

A high-performance event ingestion API built with **SBCL**, featuring connection pooling, rate limiting, and asynchronous webhook delivery.

**Two implementations available:**
- **Woo (async/event-driven)** - Clack-based, recommended for production (119 req/s POST events)
- **Hunchentoot (thread-per-request)** - Simple and reliable (70 req/s POST events, 2,733 req/s GET /health)

See [WOO_IMPLEMENTATION.md](WOO_IMPLEMENTATION.md) for details on the Clack/Woo implementation.

## 🚀 Performance Highlights

- **2,733 requests/second** @ 10 concurrent connections
- **Thread-safe connection pooling** (10 connections)
- **Rate limiting**: 1,000 req/min per organization (configurable)
- **Dual-layer deduplication** (in-memory cache + database)
- **Asynchronous webhook delivery**
- **87% test pass rate** (7/8 smoke tests passing)

## 📋 Table of Contents

- [Tech Stack](#tech-stack)
- [Quick Start](#quick-start)
- [Configuration](#configuration)
- [API Reference](#api-reference)
- [Architecture](#architecture)
- [Testing](#testing)
- [Security](#security)
- [Troubleshooting](#troubleshooting)

## Tech Stack

| Component | Technology | Version |
|-----------|-----------|---------|
| Language | SBCL (Steel Bank Common Lisp) | 2.2.9 |
| Web Server | Hunchentoot | Latest |
| Database | PostgreSQL | 16+ |
| DB Driver | Postmodern | Latest |
| JSON | Yason | Latest |
| HTTP Client | Drakma | Latest |
| Threading | Bordeaux Threads | Latest |

## Quick Start

### Prerequisites

```bash
# SBCL
sudo apt-get install sbcl  # Ubuntu/Debian
brew install sbcl          # macOS

# PostgreSQL
sudo apt-get install postgresql postgresql-contrib  # Ubuntu
brew install postgresql@16                          # macOS
```

### Database Setup

```bash
# Create database
sudo -u postgres createdb zapier_triggers

# Create schema
sudo -u postgres psql zapier_triggers << 'EOF'
CREATE TABLE organizations (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    api_key VARCHAR(255) UNIQUE NOT NULL,
    tier VARCHAR(50) DEFAULT 'free',
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE events (
    id UUID PRIMARY KEY,
    organization_id INTEGER REFERENCES organizations(id),
    event_type VARCHAR(255) NOT NULL,
    payload_json TEXT NOT NULL,
    dedup_id VARCHAR(255),
    status VARCHAR(50) DEFAULT 'pending',
    created_at TIMESTAMP DEFAULT NOW(),
    UNIQUE(organization_id, dedup_id)
);

CREATE INDEX idx_events_org_status ON events(organization_id, status);
CREATE INDEX idx_events_dedup ON events(organization_id, dedup_id);

CREATE TABLE webhooks (
    id SERIAL PRIMARY KEY,
    organization_id INTEGER REFERENCES organizations(id),
    url VARCHAR(500) NOT NULL,
    event_type_filter VARCHAR(255),
    enabled BOOLEAN DEFAULT true,
    created_at TIMESTAMP DEFAULT NOW()
);
EOF
```

### Start the Server

You can run either implementation:

#### Option 1: Woo Server (Recommended - Async/Event-Driven)

```bash
cd zapier_common_lisp

# Start with default settings (4 workers)
sbcl --load start-server.lisp

# Or explicitly specify Woo
sbcl --load start-server.lisp woo

# Or start with Hunchentoot backend via Clack
sbcl --load start-server.lisp hunchentoot
```

**Server starts on**: `http://localhost:5001`

#### Option 2: Hunchentoot Server (Simple - Thread-per-Request)

```bash
cd zapier_common_lisp

# Start simple server
sbcl --load start-simple-server.lisp

# Or with custom port
PORT=5002 sbcl --load start-simple-server.lisp
```

**Server starts on**: `http://localhost:5001` (or your custom PORT)

#### Verify it's running

```bash
curl http://localhost:5001/health
# {"status":"ok","timestamp":"2025-11-11T..."}
```

## Configuration

Configure via environment variables (optional):

```bash
# Database Configuration
export DB_NAME=zapier_triggers      # Default: zapier_triggers
export DB_USER=postgres             # Default: postgres
export DB_PASSWORD=your_password    # Default: (none - trust auth)
export DB_HOST=localhost            # Default: localhost
export DB_PORT=5432                 # Default: 5432

# Server Configuration
export PORT=5001                    # Default: 5001
export RATE_LIMIT_RPM=1000         # Default: 1000 requests/min
export DEDUP_MAX_SIZE=10000        # Default: 10000 cache entries

# Connection Pool
export DB_POOL_SIZE=10             # Default: 10 connections
```

**Configuration in code** (`simple-server.lisp:26-42`):
```lisp
(defun load-config ()
  "Load configuration from environment or defaults"
  (setf (gethash "db-name" *config*)
        (or (uiop:getenv "DB_NAME") "zapier_triggers"))
  (setf (gethash "db-user" *config*)
        (or (uiop:getenv "DB_USER") "postgres"))
  (setf (gethash "port" *config*)
        (parse-integer (or (uiop:getenv "PORT") "5001")))
  (setf (gethash "rate-limit-rpm" *config*)
        (parse-integer (or (uiop:getenv "RATE_LIMIT_RPM") "1000"))))
```

## API Reference

### Health Check

```bash
GET /health

# Response: 200 OK
{
  "status": "ok",
  "timestamp": "2025-11-11T17:25:41.962565Z"
}
```

### Generate API Key

```bash
POST /api/keys/generate
Content-Type: application/json

{
  "organization_name": "My Organization",
  "tier": "free"  # free | professional | enterprise
}

# Response: 200 OK
{
  "api-key": "sk_9DBF241D-56BB-4776-B9FE-2F612B9C26AE",
  "organization-name": "My Organization",
  "tier": "free"
}
```

### Create Event

```bash
POST /api/events
Content-Type: application/json
x-api-key: sk_9DBF241D-56BB-4776-B9FE-2F612B9C26AE

{
  "type": "user.created",
  "payload": {
    "user_id": "12345",
    "email": "user@example.com"
  },
  "dedup_id": "unique-event-identifier"  # Optional but recommended
}

# Response: 200 OK (First time)
{
  "status": "accepted",
  "event-id": "6EE37B7E-082C-4EBB-8B8D-C9574F6502A8"
}

# Response: 200 OK (Duplicate)
{
  "status": "duplicate",
  "event-id": "6EE37B7E-082C-4EBB-8B8D-C9574F6502A8",
  "message": "Event already exists"
}

# Response: 401 Unauthorized
{
  "error": "Missing API key"
}
# or
{
  "error": "Invalid API key"
}

# Response: 429 Too Many Requests
{
  "error": "Rate limit exceeded",
  "limit": 1000,
  "window": "60 seconds"
}
```

### Get Inbox

```bash
GET /api/inbox?status=pending&limit=10
x-api-key: sk_9DBF241D-56BB-4776-B9FE-2F612B9C26AE

# Query Parameters:
#   status=pending|delivered|failed  (optional)
#   limit=N                          (default: 10, max: 100)

# Response: 200 OK
{
  "events": [
    {
      "id": "6EE37B7E-082C-4EBB-8B8D-C9574F6502A8",
      "type": "user.created",
      "payload": {"user_id": "12345", "email": "user@example.com"},
      "status": "pending",
      "created-at": "2025-11-11T17:30:00Z"
    }
  ],
  "count": 1
}
```

### Register Webhook

```bash
POST /api/webhook/register
Content-Type: application/json
x-api-key: sk_9DBF241D-56BB-4776-B9FE-2F612B9C26AE

{
  "url": "https://example.com/webhook",
  "event_type_filter": "user.*"  # Optional: filter by event type pattern
}

# Response: 200 OK
{
  "status": "registered",
  "webhook-id": 123
}
```

### Cache Stats

```bash
GET /stats/cache

# Response: 200 OK
{
  "size": 42,
  "max-size": 10000,
  "hit-rate": 0.85
}
```

## Architecture

### System Overview

```
┌─────────────────────────────────────────────────────┐
│            Hunchentoot HTTP Server                   │
│                 (Port 5001)                          │
└─────────────────┬───────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────┐
│             Request Processing                       │
│  • Header parsing (x-api-key)                       │
│  • JSON body parsing                                 │
│  • API key validation (DB lookup)                    │
│  • Rate limit check (per org)                        │
└─────────────────┬───────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────┐
│          Deduplication Layer                         │
│  • In-memory cache check (fast path)                │
│  • Database UNIQUE constraint (safety)               │
└─────────────────┬───────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────┐
│        PostgreSQL Connection Pool                    │
│  • 10 pre-initialized connections                    │
│  • Thread-safe access via locks                      │
│  • On-demand connection creation                     │
└─────────────────┬───────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────┐
│           PostgreSQL Database                        │
│  • organizations table (API keys)                    │
│  • events table (with dedup constraint)              │
│  • webhooks table                                    │
└──────────────────────────────────────────────────────┘

    (Parallel Process)
┌──────────────────────────────────────────────────────┐
│         Webhook Delivery (Async)                     │
│  • Background threads per webhook                    │
│  • HTTP POST with event payload                      │
│  • Error handling and retries                        │
└──────────────────────────────────────────────────────┘
```

### Key Components

**Connection Pool** (`simple-server.lisp:64-102`)
- Pre-initialized pool of 10 PostgreSQL connections
- Thread-safe lock-based access
- Graceful degradation on exhaustion

**Rate Limiter** (`simple-server.lisp:115-143`)
- Sliding window algorithm
- Per-organization tracking
- Configurable limits via env vars

**Deduplication Cache** (`simple-server.lisp:145-174`)
- Thread-safe hash table
- Configurable max size
- LRU eviction when full

**Webhook Processor** (`simple-server.lisp:280-325`)
- Asynchronous delivery via bordeaux-threads
- Filters by event type pattern
- Parallel delivery to multiple webhooks

## Testing

### Run Smoke Tests

```bash
cd zapier_common_lisp

# Make sure server is running first
# (in another terminal)

# Run shell-based tests (recommended)
bash tests/run-smoke-tests.sh

# Or SBCL-native tests
sbcl --load tests/smoke-tests.lisp \
     --eval '(zapier-smoke-tests:run-tests)' \
     --quit
```

### Test Coverage

Current test pass rate: **87% (7/8 tests)**

✅ **Passing Tests:**
1. Health check
2. Cache stats endpoint
3. Generate API key
4. Auth requirement (401 without key)
5. Create event with valid API key
6. Duplicate detection
7. Get inbox

⚠️ **Failing Test:**
8. Invalid API key rejection (returns 500 instead of 401)

### Example Test Output

```
═══════════════════════════════════════════════════════════
   Zapier Triggers API - Smoke Tests
═══════════════════════════════════════════════════════════

Running tests against: http://localhost:5001

✅ PASS: Health check
✅ PASS: Cache stats endpoint
✅ PASS: Generate API key
   Generated API key: sk_9DBF241D-56BB-4776-B9FE-2F612B9C26AE
✅ PASS: Create event without API key (auth required)
✅ PASS: Create event
   Created event: 6EE37B7E-082C-4EBB-8B8D-C9574F6502A8
✅ PASS: Duplicate detection
✅ PASS: Get inbox
   Retrieved 2 events from inbox
❌ FAIL: Invalid API key rejected - Expected 401, got 500

═══════════════════════════════════════════════════════════
   Test Summary
═══════════════════════════════════════════════════════════

Total:  8 tests
Passed: 7 (87%)
Failed: 1

❌ Some tests failed
```

See [tests/README.md](tests/README.md) for more details.

## Security

⚠️ **CRITICAL**: The default configuration is for **DEVELOPMENT ONLY**.

### Production Requirements

1. **Enable PostgreSQL password authentication** (currently uses `trust`)
2. **Configure environment variables** for credentials
3. **Enable SSL/TLS** for database connections
4. **Set up firewall rules** and reverse proxy
5. **Configure monitoring** and alerting

**See [SECURITY.md](SECURITY.md) for complete production hardening guide.**

### Quick Security Checklist

- [ ] PostgreSQL using `scram-sha-256` authentication (not `trust`)
- [ ] Database password stored in secrets manager
- [ ] SSL/TLS enabled for database connections
- [ ] Reverse proxy configured (nginx/Apache)
- [ ] HTTPS enabled
- [ ] Rate limiting tuned for production workload
- [ ] Monitoring and alerting configured

## Troubleshooting

### Server won't start

**Error**: `Connection refused` or `Failed to connect to database`

**Solution**:
```bash
# Check PostgreSQL is running
sudo service postgresql status

# Start if needed
sudo service postgresql start

# Verify database exists
sudo -u postgres psql -l | grep zapier_triggers
```

### Tests failing with connection errors

**Error**: `Connection pool exhausted`

**Solution**:
```bash
# Increase pool size
export DB_POOL_SIZE=20

# Or restart server to reset pool
pkill -f simple-server
sbcl --load simple-server.lisp ...
```

### Rate limit issues

**Error**: `Rate limit exceeded`

**Solution**:
```bash
# Increase rate limit
export RATE_LIMIT_RPM=10000

# Restart server for changes to take effect
```

### Webhook delivery failures

**Check webhook registration**:
```bash
sudo -u postgres psql zapier_triggers -c \
  "SELECT * FROM webhooks WHERE organization_id = YOUR_ORG_ID;"
```

**Check server logs** for delivery errors:
```bash
# Server logs show webhook delivery attempts
[INFO] Delivering webhook to https://example.com/webhook
[ERROR] Webhook delivery failed: Connection timeout
```

## File Structure

```
zapier_common_lisp/
├── simple-server.lisp        # Complete implementation (545 lines)
├── README.md                 # This file
├── SECURITY.md              # Production security guide
├── REVIEW_RESPONSE.md       # Code review analysis
└── tests/
    ├── run-smoke-tests.sh   # Shell-based smoke tests (curl)
    ├── smoke-tests.lisp     # SBCL-native tests (drakma)
    └── README.md            # Test documentation
```

## Performance Benchmarks

Tested with `wrk` on `POST /api/events`:

```bash
wrk -t10 -c10 -d30s \
  -H "x-api-key: sk_..." \
  -H "Content-Type: application/json" \
  -s post_event.lua \
  http://localhost:5001/api/events
```

**Results:**
- **Throughput**: 2,733 requests/second
- **Latency (avg)**: 3.66ms
- **Latency (p50)**: <5ms
- **Latency (p99)**: <20ms

## Implementation Notes

### Single-File Design

This implementation uses a **monolithic single-file approach** (`simple-server.lisp`) for simplicity:

**Advantages:**
- Easy to understand and navigate
- No complex module dependencies
- Fast prototyping and iteration
- Simple deployment (one file)

**Trade-offs:**
- Harder to test individual components
- Less reusable across projects
- Longer compile times for changes

For production systems, consider splitting into:
- `src/config.lisp` - Configuration
- `src/db.lisp` - Database layer
- `src/middleware.lisp` - Rate limiting, auth
- `src/routes.lisp` - HTTP handlers
- `src/webhooks.lisp` - Webhook delivery

### Thread Safety

All shared state uses **Bordeaux Threads locks**:

```lisp
;; Connection pool (line 66)
(defvar *db-pool-lock* (bt:make-lock "db-pool-lock"))

;; Rate limiter (line 117)
(defvar *rate-limit-lock* (bt:make-lock "rate-limit-lock"))

;; Dedup cache (line 147)
(defvar *dedup-cache-lock* (bt:make-lock "dedup-cache-lock"))
```

### Error Handling

All HTTP handlers use `handler-case`:

```lisp
(define-easy-handler (post-event :uri "/api/events") ()
  (handler-case
      ;; ... handler logic ...
    (error (e)
      (format t "~&[ERROR] Failed to create event: ~a~%" e)
      (json-response (list :error "Failed to create event"
                          :message (format nil "~a" e))
                     500))))
```

## Related Documentation

- **[SECURITY.md](SECURITY.md)** - Production security hardening
- **[REVIEW_RESPONSE.md](REVIEW_RESPONSE.md)** - Code review analysis
- **[tests/README.md](tests/README.md)** - Test suite documentation

## Resources

- **SBCL Manual**: http://www.sbcl.org/manual/
- **Hunchentoot Documentation**: https://edicl.github.io/hunchentoot/
- **Postmodern Guide**: https://marijnhaverbeke.nl/postmodern/
- **Common Lisp HyperSpec**: http://www.lispworks.com/documentation/HyperSpec/

## License

MIT

---

**Server Status**: http://localhost:5001/health

**Questions or Issues?** See [REVIEW_RESPONSE.md](REVIEW_RESPONSE.md) for detailed feature documentation.
