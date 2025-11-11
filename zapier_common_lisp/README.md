# Zapier Triggers API - Common Lisp (Woo) Implementation

> **Note**: This is part of a [monorepo](../README.md) with multiple implementations. See [comparison](../COMPARISON_SUMMARY.md) for performance analysis.

A high-performance RESTful webhook ingestion API built with **Common Lisp** and the **Woo** web server, showcasing Lisp's metaprogramming capabilities and REPL-driven development workflow.

## Overview

This implementation demonstrates:
- 🚀 **High-performance HTTP** via Woo's libev-based architecture
- 🔄 **Interactive development** with hot code reloading
- 🧵 **Thread-safe concurrency** using bordeaux-threads
- 🎯 **Functional programming** patterns in production
- 📊 **Multi-worker clustering** for scalability

## Tech Stack

- **Language**: Common Lisp (SBCL)
- **Web Server**: Woo (non-blocking HTTP on libev)
- **Framework**: Clack + Lack (middleware)
- **Routing**: Ningle
- **Database**: PostgreSQL 16 (via Postmodern)
- **JSON**: Jonathan (fast JSON encoding/decoding)
- **Package Manager**: Quicklisp
- **Build Tool**: ASDF

## Quick Start

### Prerequisites

- **SBCL** 2.x+ ([download](http://www.sbcl.org/))
- **Quicklisp** ([installation](https://www.quicklisp.org/))
- **PostgreSQL** 16+
- **macOS or Linux** (Windows via WSL)

### Installation

**From monorepo root:**
```bash
cd zapier_common_lisp
./scripts/setup.sh
```

**Or manually:**
```bash
# Install SBCL
brew install sbcl  # macOS
# apt-get install sbcl  # Linux

# Install Quicklisp
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
> (quicklisp-quickstart:install)
> (ql:add-to-init-file)
> (quit)

# Setup database
createdb zapier_triggers
psql zapier_triggers < sql/schema.sql

# Load dependencies
sbcl --eval '(ql:quickload :zapier-triggers)' --quit
```

### Start Server

```bash
# Using script (recommended)
./scripts/start.sh

# Or manually
sbcl --eval '(ql:quickload :zapier-triggers)' \
     --eval '(zapier-triggers:start-server :port 5000 :worker-num 4)'

# Visit: http://localhost:5000/health
```

## Development

### REPL Workflow

Common Lisp's REPL-driven development allows hot code reloading:

```lisp
;; Start REPL
sbcl

;; Load system
(ql:quickload :zapier-triggers)

;; Switch to package
(in-package :zapier-triggers)

;; Start server
(start-server :port 5000 :worker-num 4 :debug t)

;; Make code changes in your editor...

;; Reload modified file
(load "src/routes/events.lisp")

;; Test changes immediately (no restart needed!)

;; Stop server when done
(stop-server)
```

### Configuration

Environment variables:

```bash
export PORT=5000
export WORKER_COUNT=4
export ENVIRONMENT=development
export DATABASE_URL=postgresql://user:pass@localhost/zapier_triggers
```

### Run Tests

```bash
./scripts/test.sh

# Or manually
sbcl --eval '(ql:quickload :zapier-triggers/tests)' \
     --eval '(asdf:test-system :zapier-triggers)' \
     --quit
```

## API Endpoints

All endpoints match the unified API specification:

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check |
| POST | `/api/keys/generate` | Generate API key |
| GET | `/api/keys` | View API key info |
| POST | `/api/events` | Ingest event |
| GET | `/api/inbox` | List events |
| POST | `/api/ack/:id` | Acknowledge event |
| POST | `/api/webhook/config` | Configure webhook |

## Performance

### Targets
- **Throughput**: 500-800 req/s (1000 requests, 50 concurrent)
- **P50 Latency**: <80ms
- **P95 Latency**: <150ms
- **P99 Latency**: <200ms

### Multi-Worker Clustering

Woo supports multi-worker clustering for better concurrency:

```lisp
;; Production: 4 workers
(start-server :port 5000 :worker-num 4 :debug nil)

;; Development: 1 worker for debugging
(start-server :port 5000 :worker-num 1 :debug t)
```

### Thread-Safety

Rate limiting and shared state use **bordeaux-threads** for thread-safe operations:

```lisp
;; From middleware/rate-limit.lisp
(defvar *rate-limit-lock* (bt:make-lock "rate-limit-lock"))

(defun within-limit-p (org-id tier)
  (bt:with-lock-held (*rate-limit-lock*)
    (consume-token (get-or-create-bucket org-id tier))))
```

## Architecture

```
┌─────────────────────────────────────────┐
│         Woo HTTP Server (libev)          │
├─────────────────────────────────────────┤
│          Clack Middleware Stack          │
│  • Error Handler                         │
│  • Access Log                            │
│  • Authentication (API Keys)             │
│  • Rate Limiting (Token Bucket)          │
├─────────────────────────────────────────┤
│         Ningle Routing Layer             │
│  • /health                               │
│  • /api/keys/*                           │
│  • /api/events                           │
│  • /api/inbox                            │
│  • /api/ack/:id                          │
│  • /api/webhook/config                   │
├─────────────────────────────────────────┤
│         Business Logic (Models)          │
│  • Organizations                         │
│  • Events                                │
│  • Webhooks                              │
├─────────────────────────────────────────┤
│      PostgreSQL (via Postmodern)         │
│  • Connection pooling                    │
│  • Prepared statements                   │
└─────────────────────────────────────────┘
```

## Project Structure

```
zapier_common_lisp/
├── zapier-triggers.asd      # System definition
├── src/
│   ├── package.lisp          # Package definitions
│   ├── config.lisp           # Configuration
│   ├── server.lisp           # Woo server + routing
│   ├── middleware/
│   │   ├── auth.lisp         # API key auth
│   │   ├── rate-limit.lisp   # Rate limiting (thread-safe)
│   │   └── error-handler.lisp# Error handling
│   ├── routes/
│   │   ├── health.lisp       # Health check
│   │   ├── keys.lisp         # API keys
│   │   ├── events.lisp       # Event ingestion
│   │   ├── inbox.lisp        # Event retrieval
│   │   └── webhook.lisp      # Webhook config
│   ├── models/
│   │   ├── organization.lisp # Organization model
│   │   ├── event.lisp        # Event model
│   │   └── webhook.lisp      # Webhook model
│   ├── db/
│   │   ├── connection.lisp   # DB connection pooling
│   │   └── queries.lisp      # SQL queries
│   └── utils/
│       ├── json.lisp         # JSON utilities
│       ├── validation.lisp   # Input validation
│       └── crypto.lisp       # UUID generation
├── tests/                    # Test suite
├── sql/
│   └── schema.sql            # Database schema
├── scripts/
│   ├── setup.sh              # Setup script
│   ├── start.sh              # Start server
│   └── test.sh               # Run tests
└── README.md                 # This file
```

## Example Usage

### 1. Generate API Key

```bash
curl -X POST http://localhost:5000/api/keys/generate \
  -H "Content-Type: application/json" \
  -d '{
    "organization_name": "My Org",
    "tier": "free"
  }'
```

### 2. Send Event

```bash
curl -X POST http://localhost:5000/api/events \
  -H "X-API-Key: your-api-key" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "user.created",
    "dedup_id": "unique-123",
    "payload": {
      "user_id": "12345",
      "email": "user@example.com"
    }
  }'
```

### 3. Check Inbox

```bash
curl "http://localhost:5000/api/inbox?status=pending&limit=10" \
  -H "X-API-Key: your-api-key"
```

## Monorepo Context

This implementation is part of a comparative study:

- **[Python (FastAPI)](../zapier_python/README.md)** - 245 req/s, simple MVP
- **[Elixir (Phoenix)](../zapier_elixir/zapier_triggers/README.md)** - 892 req/s, production-ready
- **[Rust](../zapier_rust/)** - High-performance (WIP)
- **[Unified Test Suite](../unified_test_suite/README.md)** - Cross-implementation testing

### Integration with Test Suite

```bash
cd ../unified_test_suite

# Run functional tests
./run_tests.sh --type functional --impl commonlisp

# Run performance benchmarks
./run_tests.sh --type performance --impl commonlisp
```

## Why Common Lisp?

### Advantages
✅ **Interactive Development**: REPL-driven workflow with instant feedback
✅ **Hot Code Reloading**: Update code without restarting server
✅ **Powerful Macros**: Create DSLs for routing and validation
✅ **Mature Ecosystem**: Decades of stable libraries
✅ **Native Compilation**: SBCL compiles to native machine code
✅ **Advanced Debugging**: Inspect and modify running system

### Trade-offs
⚠️ **Learning Curve**: Lisp syntax unfamiliar to many
⚠️ **Smaller Community**: Fewer resources than mainstream languages
⚠️ **Deployment**: Less common in production environments
⚠️ **Tooling**: Fewer modern dev tools

## Dependencies

Core libraries used:

- **woo**: Fast HTTP server
- **clack**: Web application environment
- **lack**: Middleware library
- **ningle**: Routing framework
- **postmodern**: PostgreSQL client
- **jonathan**: Fast JSON library
- **bordeaux-threads**: Cross-implementation threading
- **local-time**: Timestamp handling
- **uuid**: UUID generation
- **cl-ppcre**: Regular expressions

## Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) for:
- Development workflow
- Code style guidelines
- Testing requirements
- PR process

## Resources

- **Woo Documentation**: https://github.com/fukamachi/woo
- **Common Lisp Resources**: https://lisp-lang.org/
- **SBCL Manual**: http://www.sbcl.org/manual/
- **Quicklisp**: https://www.quicklisp.org/

## License

MIT

---

**Quick Links:**
- API: http://localhost:5000/health
- Test Suite: [../unified_test_suite/](../unified_test_suite/)
- Comparison: [../COMPARISON_SUMMARY.md](../COMPARISON_SUMMARY.md)
