# Product Requirements Document (PRD)
## Zapier Triggers API - Common Lisp (Woo) Implementation

**Project ID:** zapier_common_lisp
**Status:** Planning
**Created:** 2025-11-10
**Implementation Language:** Common Lisp
**Web Server:** Woo (Fast non-blocking HTTP server on libev)

---

## 1. Executive Summary

This PRD defines the implementation of the Zapier Triggers API using **Common Lisp** and the **Woo** web server. This implementation will join the existing Python (FastAPI), Elixir (Phoenix), and Rust implementations in the monorepo, providing a unique perspective on functional programming with decades of language stability and powerful metaprogramming capabilities.

The Common Lisp implementation aims to showcase:
- High-performance HTTP handling via Woo's libev-based architecture
- Lisp's legendary REPL-driven development workflow
- Powerful macro system for DSL creation
- Interactive debugging and hot-code reloading in production
- Integration with the unified test suite for cross-language comparison

---

## 2. Goals & Success Metrics

### Primary Goals
1. **Functional Parity**: Implement all P0 API endpoints compatible with unified test suite
2. **Performance Target**: Achieve >500 req/s throughput (between Python and Elixir)
3. **Developer Experience**: Demonstrate Lisp's interactive development advantages
4. **Integration**: Pass all unified test suite functional tests

### Success Metrics
- ✅ All 15+ unified functional tests passing
- ✅ Benchmark results: 500-800 req/s throughput
- ✅ P95 latency: <150ms under load
- ✅ Clean integration with existing monorepo tooling
- ✅ Documentation quality matches other implementations

---

## 3. Technical Architecture

### Core Technology Stack

**Language & Runtime:**
- Common Lisp (SBCL - Steel Bank Common Lisp)
- ASDF (Another System Definition Facility) for build
- Quicklisp for dependency management

**Web Server:**
- **Woo**: Fast non-blocking HTTP server built on libev
  - Event-driven architecture
  - Multi-worker clustering support
  - SSL/TLS support (via CL+SSL)
  - Clack-compatible (middleware ecosystem)

**Database:**
- PostgreSQL 16 (via cl-dbi or postmodern)
- Connection pooling
- Prepared statements for performance

**Routing & Middleware:**
- **Clack**: Web application environment
- **Lack**: Middleware library (inspired by Rack/WSGI)
- **Ningle** or **Caveman2**: Lightweight routing framework

**JSON Handling:**
- **jonathan** or **jzon**: Fast JSON encoding/decoding
- Schema validation for API requests

**Additional Libraries:**
- **local-time**: Timestamp handling
- **uuid**: Unique identifier generation
- **cl-ppcre**: Regular expressions
- **bordeaux-threads**: Thread management
- **trivial-backtrace**: Error handling

### System Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Woo HTTP Server                       │
│                  (libev event loop)                      │
├─────────────────────────────────────────────────────────┤
│                   Clack Middleware                        │
│  ┌──────────┬──────────┬──────────┬──────────────┐      │
│  │   Auth   │   CORS   │  Logger  │ Error Handler│      │
│  └──────────┴──────────┴──────────┴──────────────┘      │
├─────────────────────────────────────────────────────────┤
│                   Application Layer                       │
│  ┌─────────────────────────────────────────────┐        │
│  │         API Routes (Ningle/Caveman)          │        │
│  │  /api/keys/generate  |  /api/events          │        │
│  │  /api/inbox         |  /api/ack/:id         │        │
│  │  /api/webhook/config |  /health              │        │
│  └─────────────────────────────────────────────┘        │
├─────────────────────────────────────────────────────────┤
│                    Business Logic                         │
│  ┌──────────┬──────────┬──────────┬──────────────┐      │
│  │   Keys   │  Events  │  Inbox   │  Webhooks    │      │
│  └──────────┴──────────┴──────────┴──────────────┘      │
├─────────────────────────────────────────────────────────┤
│                   Data Access Layer                       │
│  ┌─────────────────────────────────────────────┐        │
│  │         Database (PostgreSQL via DBI)        │        │
│  │  - Organizations  - Events  - Webhooks       │        │
│  └─────────────────────────────────────────────┘        │
└─────────────────────────────────────────────────────────┘
```

---

## 4. Functional Requirements

### P0: Must-Have (MVP)

#### 4.1 API Key Management
**Endpoints:**
- `POST /api/keys/generate` - Generate new API key for organization
- `GET /api/keys` - Retrieve API key information

**Requirements:**
- Generate secure random API keys (UUID v4)
- Store organization metadata
- Support tier-based rate limits (free, starter, professional, enterprise)
- Thread-safe key storage and retrieval

#### 4.2 Event Ingestion
**Endpoint:**
- `POST /api/events` - Accept JSON event payloads

**Requirements:**
- Authenticate via `X-API-Key` header
- Validate JSON payload structure
- Generate unique event IDs
- Store with timestamp metadata
- Return 201 with event confirmation
- Enforce rate limits per tier
- Handle concurrent requests safely

#### 4.3 Event Retrieval (Inbox)
**Endpoint:**
- `GET /api/inbox` - List undelivered events

**Query Parameters:**
- `status`: Filter by status (pending, delivered, failed)
- `limit`: Max results (default: 100, max: 1000)
- `offset`: Pagination offset

**Requirements:**
- Filter by organization (via API key)
- Support pagination
- Return structured JSON array
- Include metadata (count, total, etc.)

#### 4.4 Event Acknowledgment
**Endpoint:**
- `POST /api/ack/:id` - Acknowledge event delivery

**Requirements:**
- Mark event as delivered
- Validate ownership (event belongs to org)
- Return 204 on success
- Handle missing/invalid IDs gracefully

#### 4.5 Health Check
**Endpoint:**
- `GET /health` - Service health status

**Requirements:**
- Check database connectivity
- Return JSON status with timestamp
- Fast response (<10ms)

### P1: Should-Have

#### 4.6 Webhook Configuration
**Endpoint:**
- `POST /api/webhook/config` - Configure webhook URL

**Requirements:**
- Store webhook URL per organization
- Validate URL format
- Optional authentication headers
- Support retry configuration

#### 4.7 Rate Limiting
- Implement per-tier rate limits:
  - Free: 10 req/min
  - Starter: 60 req/min
  - Professional: 600 req/min
  - Enterprise: 6000 req/min
- Use in-memory token bucket or sliding window
- Return 429 with proper headers

#### 4.8 Request Validation
- JSON schema validation
- Payload size limits (256KB)
- Content-Type enforcement
- Comprehensive error messages

### P2: Nice-to-Have

- API documentation endpoint (OpenAPI/Swagger)
- Prometheus metrics endpoint
- Request/response logging
- REPL debugging interface
- Hot code reloading in production

---

## 5. Integration with Unified Test Suite

### Test Suite Compatibility

The Common Lisp implementation **MUST** be compatible with the existing unified test suite:

**Location:** `/unified_test_suite/`

**Required Support:**
1. All functional tests in `tests/test_functional.py`
2. Performance benchmarks in `tests/benchmark.py`
3. Load testing via Locust (`tests/test_performance.py`)

### Configuration

Add Common Lisp support to test suite:

**File:** `unified_test_suite/config/test_config.py`
```python
COMMON_LISP_BASE_URL = os.getenv(
    "TEST_COMMON_LISP_BASE_URL",
    "http://localhost:5000"
)
```

**File:** `unified_test_suite/run_tests.sh`
```bash
--impl commonlisp|cl    Test Common Lisp implementation
--cl-url URL           Custom Common Lisp API URL
```

### Expected Test Results

**Functional Tests:** 100% pass rate (15/15 tests)

**Performance Targets:**
- Throughput: 500-800 req/s (1000 requests, 50 concurrent)
- P50 Latency: <80ms
- P95 Latency: <150ms
- P99 Latency: <200ms
- Success Rate: >99.9%

**Comparison Position:**
- Faster than Python (245 req/s)
- Comparable to or slower than Elixir (892 req/s)
- Focus on developer experience over raw speed

---

## 6. Development Workflow

### Project Structure

```
zapier_common_lisp/
├── zapier-triggers.asd          # System definition
├── src/
│   ├── package.lisp             # Package definitions
│   ├── config.lisp              # Configuration
│   ├── server.lisp              # Woo server setup
│   ├── middleware/
│   │   ├── auth.lisp            # API key authentication
│   │   ├── rate-limit.lisp      # Rate limiting
│   │   └── error-handler.lisp   # Error handling
│   ├── routes/
│   │   ├── keys.lisp            # API key routes
│   │   ├── events.lisp          # Event ingestion
│   │   ├── inbox.lisp           # Event retrieval
│   │   └── health.lisp          # Health checks
│   ├── models/
│   │   ├── organization.lisp    # Organization model
│   │   ├── event.lisp           # Event model
│   │   └── webhook.lisp         # Webhook model
│   ├── db/
│   │   ├── connection.lisp      # DB connection pool
│   │   └── queries.lisp         # SQL queries
│   └── utils/
│       ├── json.lisp            # JSON utilities
│       ├── validation.lisp      # Input validation
│       └── crypto.lisp          # Key generation
├── tests/
│   ├── test-suite.lisp          # Test runner
│   ├── unit/
│   │   ├── test-events.lisp     # Event tests
│   │   └── test-keys.lisp       # Key tests
│   └── integration/
│       └── test-api.lisp        # API integration tests
├── sql/
│   ├── schema.sql               # Database schema
│   └── migrations/              # Schema migrations
├── scripts/
│   ├── setup.sh                 # Setup script
│   ├── start.sh                 # Start server
│   └── test.sh                  # Run tests
├── .taskmaster/                 # Task Master integration
│   ├── docs/
│   │   └── prd-woo-implementation.md  # This file
│   └── tasks/
├── README.md                    # Documentation
└── Dockerfile                   # Containerization

```

### Development Commands

```bash
# Install dependencies
sbcl --eval "(ql:quickload :zapier-triggers)" --quit

# Start server (development)
sbcl --eval "(ql:quickload :zapier-triggers)" \
     --eval "(zapier-triggers:start-server :port 5000)"

# Start server (production - 4 workers)
sbcl --eval "(ql:quickload :zapier-triggers)" \
     --eval "(zapier-triggers:start-server :port 5000 :worker-num 4)"

# Run tests
sbcl --eval "(ql:quickload :zapier-triggers/tests)" \
     --eval "(zapier-triggers/tests:run-tests)"

# REPL development
sbcl --eval "(ql:quickload :zapier-triggers)" \
     --eval "(in-package :zapier-triggers)"
```

---

## 7. Woo Server Configuration

### Basic Setup

```common-lisp
(defun start-server (&key (port 5000) (worker-num 4))
  "Start Woo server with specified configuration"
  (woo:run
    (lambda (env)
      (handle-request env))
    :port port
    :worker-num worker-num
    :use-default-middlewares nil
    :debug nil))
```

### Clustering (Multi-Worker)

```common-lisp
;; Production: 4 workers for concurrency
(woo:run app
  :port 5000
  :worker-num 4)

;; Development: Single worker for debugging
(woo:run app
  :port 5000
  :worker-num 1
  :debug t)
```

### SSL/TLS Support (Optional)

```common-lisp
(woo:run app
  :port 5443
  :ssl-cert-file #P"/path/to/cert.pem"
  :ssl-key-file #P"/path/to/key.pem")
```

### Middleware Stack

```common-lisp
(lack:builder
  :accesslog              ; Request logging
  (:static :path "/static/")
  :cors                   ; CORS headers
  *auth-middleware*       ; API key auth
  *rate-limit-middleware* ; Rate limiting
  *error-handler*         ; Error handling
  *routes*)               ; Application routes
```

---

## 8. Database Schema

### PostgreSQL Tables

```sql
-- Organizations table
CREATE TABLE organizations (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    api_key VARCHAR(255) UNIQUE NOT NULL,
    tier VARCHAR(50) DEFAULT 'free',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_organizations_api_key ON organizations(api_key);

-- Events table
CREATE TABLE events (
    id VARCHAR(255) PRIMARY KEY,
    organization_id INTEGER REFERENCES organizations(id),
    event_type VARCHAR(100) NOT NULL,
    payload JSONB NOT NULL,
    status VARCHAR(50) DEFAULT 'pending',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    delivered_at TIMESTAMP
);

CREATE INDEX idx_events_org_status ON events(organization_id, status);
CREATE INDEX idx_events_created_at ON events(created_at);

-- Webhooks table
CREATE TABLE webhooks (
    id SERIAL PRIMARY KEY,
    organization_id INTEGER REFERENCES organizations(id),
    url VARCHAR(500) NOT NULL,
    auth_headers JSONB,
    retry_count INTEGER DEFAULT 3,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

---

## 9. Non-Functional Requirements

### Performance
- **Throughput:** 500-800 req/s (target: match or exceed Python)
- **Latency:** P95 <150ms under normal load
- **Concurrency:** Handle 100+ concurrent connections
- **Startup Time:** <5 seconds
- **Memory:** <500MB under normal load

### Reliability
- **Uptime:** 99.9% availability target
- **Error Handling:** Graceful degradation on DB failures
- **Recovery:** Automatic restart on crashes
- **Connection Pooling:** Reuse DB connections efficiently

### Scalability
- **Horizontal:** Multi-worker support (Woo clustering)
- **Vertical:** Efficient memory and CPU usage
- **Database:** Connection pooling with configurable limits

### Security
- **Authentication:** Secure API key validation
- **SQL Injection:** Use parameterized queries exclusively
- **Input Validation:** Sanitize all user inputs
- **Rate Limiting:** Prevent abuse

### Developer Experience
- **REPL Workflow:** Hot code reloading during development
- **Error Messages:** Clear, actionable error responses
- **Documentation:** Comprehensive API docs
- **Testing:** Unit and integration test coverage

---

## 10. Deployment

### Docker Support

**Dockerfile:**
```dockerfile
FROM clfoundation/sbcl:latest

WORKDIR /app

# Install Quicklisp
RUN sbcl --eval "(load \"https://beta.quicklisp.org/quicklisp.lisp\")" \
         --eval "(quicklisp-quickstart:install)" \
         --eval "(quit)"

# Copy application
COPY . /app/

# Load dependencies
RUN sbcl --eval "(ql:quickload :zapier-triggers)" --quit

# Build binary
RUN sbcl --eval "(ql:quickload :zapier-triggers)" \
         --eval "(sb-ext:save-lisp-and-die \"zapier-triggers\" \
                   :toplevel 'zapier-triggers:main \
                   :executable t)"

EXPOSE 5000

CMD ["./zapier-triggers"]
```

### Environment Variables

```bash
DATABASE_URL=postgresql://user:pass@localhost/zapier_triggers
PORT=5000
WORKER_COUNT=4
LOG_LEVEL=info
ENVIRONMENT=production
```

---

## 11. Testing Strategy

### Unit Tests (FiveAM or Prove)

```common-lisp
(deftest test-generate-api-key
  "Test API key generation"
  (let ((key (generate-api-key)))
    (is (stringp key))
    (is (= 36 (length key)))))

(deftest test-event-validation
  "Test event payload validation"
  (let ((valid-event '(("type" . "user.created")
                       ("payload" . (("user_id" . "123"))))))
    (is (validate-event valid-event))))
```

### Integration Tests

```common-lisp
(deftest test-post-event-endpoint
  "Test POST /api/events endpoint"
  (let* ((client (make-test-client))
         (api-key (create-test-api-key))
         (response (post-event client api-key test-event)))
    (is (= 201 (response-status response)))
    (is (assoc "id" (response-body response)))))
```

### Unified Test Suite Integration

The implementation must pass all tests in:
- `unified_test_suite/tests/test_functional.py`

Run tests:
```bash
cd unified_test_suite
./run_tests.sh --type functional --impl commonlisp
./run_tests.sh --type performance --impl commonlisp
```

---

## 12. Comparison with Other Implementations

### Common Lisp (Woo) Advantages
✅ **Interactive Development**: REPL-driven workflow with hot reloading
✅ **Powerful Macros**: DSL creation for routing and validation
✅ **Mature Ecosystem**: Decades of stable libraries
✅ **Performance**: Compiled native code (SBCL)
✅ **Debugging**: Live inspection and modification
✅ **Memory Management**: Advanced GC options

### Trade-offs
⚠️ **Learning Curve**: Lisp syntax unfamiliar to many developers
⚠️ **Community Size**: Smaller than Python/Elixir ecosystems
⚠️ **Deployment**: Less common in production environments
⚠️ **Tooling**: Fewer modern dev tools compared to mainstream languages

### Expected Position in Benchmarks
- **Faster than:** Python (FastAPI)
- **Comparable to:** Rust (depending on optimization)
- **Slower than:** Elixir (BEAM's concurrency advantages)

---

## 13. Success Criteria

### MVP Completion Checklist

- [ ] Woo server running with multi-worker support
- [ ] All P0 API endpoints implemented
- [ ] Database integration (PostgreSQL)
- [ ] Authentication middleware (API keys)
- [ ] Rate limiting implementation
- [ ] All unified functional tests passing (15/15)
- [ ] Performance benchmarks documented
- [ ] README with setup instructions
- [ ] Docker support
- [ ] Integration with monorepo test suite

### Performance Targets
- [ ] >500 req/s throughput
- [ ] P95 latency <150ms
- [ ] 100% functional test pass rate
- [ ] Memory usage <500MB

### Documentation
- [ ] API endpoint documentation
- [ ] Setup and installation guide
- [ ] Developer workflow guide
- [ ] Comparison with other implementations
- [ ] REPL usage examples

---

## 14. Timeline & Milestones

### Phase 1: Foundation (Days 1-3)
- [ ] Project structure setup
- [ ] Woo server basic configuration
- [ ] Database connection and schema
- [ ] Basic routing (health check)

### Phase 2: Core Features (Days 4-7)
- [ ] API key generation endpoint
- [ ] Event ingestion endpoint
- [ ] Inbox retrieval endpoint
- [ ] Event acknowledgment endpoint
- [ ] Authentication middleware

### Phase 3: Enhancement (Days 8-10)
- [ ] Rate limiting implementation
- [ ] Input validation and error handling
- [ ] Webhook configuration endpoint
- [ ] Unit test coverage

### Phase 4: Integration (Days 11-12)
- [ ] Unified test suite integration
- [ ] Performance benchmarking
- [ ] Documentation completion
- [ ] Docker support

### Phase 5: Optimization (Days 13-14)
- [ ] Performance tuning
- [ ] Code cleanup and refactoring
- [ ] Final testing and validation
- [ ] README and comparison updates

---

## 15. Out of Scope (Future Work)

- Advanced deduplication (24-hour window)
- API key rotation
- Webhook delivery workers
- Prometheus metrics
- OpenAPI/Swagger documentation
- WebSocket support
- GraphQL interface
- Multi-tenancy features

---

## 16. References

### Woo Documentation
- GitHub: https://github.com/fukamachi/woo
- Benchmark: https://github.com/fukamachi/woo/blob/master/benchmark.md

### Common Lisp Resources
- SBCL: http://www.sbcl.org/
- Quicklisp: https://www.quicklisp.org/
- Clack: https://github.com/fukamachi/clack
- Lack: https://github.com/fukamachi/lack

### Monorepo Context
- Project Spec: `/zapier/project_spec.md`
- Unified Tests: `/zapier/unified_test_suite/README.md`
- Python Implementation: `/zapier/zapier_python/README.md`
- Elixir Implementation: `/zapier/zapier_elixir/zapier_triggers/README.md`

---

## 17. Approval & Sign-off

**PRD Author:** AI Development Team
**Date:** 2025-11-10
**Status:** Ready for Implementation

**Stakeholders:**
- [ ] Technical Lead
- [ ] Backend Team
- [ ] DevOps Team

---

**Next Steps:**
1. Review and approve PRD
2. Setup project structure
3. Begin Phase 1 implementation
4. Schedule weekly progress reviews
