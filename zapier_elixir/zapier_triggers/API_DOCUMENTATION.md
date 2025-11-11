# Zapier Triggers API Documentation

A production-ready event ingestion and webhook delivery system built with Elixir/Phoenix.

## Features

- ✅ RESTful API for event ingestion
- ✅ Webhook delivery with automatic retries (exponential backoff)
- ✅ Per-organization rate limiting with tier-based limits
- ✅ 24-hour event deduplication
- ✅ Dead Letter Queue for failed deliveries
- ✅ API key authentication with rotation
- ✅ HTTPS-only with HSTS
- ✅ Structured JSON logging
- ✅ Prometheus metrics
- ✅ PostgreSQL-backed job queue (Oban)

## Quick Start

### Prerequisites

- Elixir 1.14+
- PostgreSQL 14+
- Mix (comes with Elixir)

### Installation

```bash
# Install dependencies
mix deps.get

# Create and migrate database
mix ecto.create
mix ecto.migrate

# Start the server
mix phx.server
```

The API will be available at `http://localhost:4000/api`

## API Endpoints

### Authentication

All authenticated endpoints require an `X-API-Key` header with a valid API key.

### 1. Generate API Key (Public)

Create a new organization and generate an API key.

```http
POST /api/keys/generate
Content-Type: application/json

{
  "organization_name": "My Organization",
  "tier": "free"
}
```

**Tiers and Rate Limits:**
- `free`: 100 requests/minute
- `pro`: 1,000 requests/minute
- `business`: 10,000 requests/minute
- `enterprise`: 100,000 requests/minute

**Response (201):**
```json
{
  "organization_id": "550e8400-e29b-41d4-a716-446655440000",
  "organization_name": "My Organization",
  "api_key": "zap_test_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789",
  "tier": "free",
  "rate_limit_per_minute": 100,
  "created_at": "2025-11-10T12:00:00Z",
  "warning": "Store this API key securely. It will not be shown again."
}
```

### 2. View API Key Info (Authenticated)

Get information about your organization and API key.

```http
GET /api/keys
X-API-Key: zap_test_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789
```

**Response (200):**
```json
{
  "organization_id": "550e8400-e29b-41d4-a716-446655440000",
  "organization_name": "My Organization",
  "tier": "free",
  "rate_limit_per_minute": 100,
  "webhook_url": "https://example.com/webhook",
  "created_at": "2025-11-10T12:00:00Z",
  "updated_at": "2025-11-10T12:00:00Z",
  "note": "API key is not displayed for security reasons. Use /api/keys/rotate to generate a new one."
}
```

### 3. Rotate API Key (Authenticated)

Generate a new API key, invalidating the old one.

```http
POST /api/keys/rotate
X-API-Key: zap_test_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789
```

**Response (200):**
```json
{
  "organization_id": "550e8400-e29b-41d4-a716-446655440000",
  "organization_name": "My Organization",
  "api_key": "zap_test_NewKeyAbCdEfGhIjKlMnOpQrStUvWxYz012",
  "rotated_at": "2025-11-10T13:00:00Z",
  "warning": "Your old API key has been invalidated. Update your applications with this new key."
}
```

### 4. Configure Webhook URL (Authenticated)

Set or update the webhook URL where events will be delivered.

```http
POST /api/webhook/config
X-API-Key: zap_test_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789
Content-Type: application/json

{
  "webhook_url": "https://example.com/webhook"
}
```

**Response (200):**
```json
{
  "organization_id": "550e8400-e29b-41d4-a716-446655440000",
  "webhook_url": "https://example.com/webhook",
  "updated_at": "2025-11-10T13:00:00Z",
  "message": "Webhook URL configured successfully"
}
```

### 5. Create Event (Authenticated)

Ingest a new event for webhook delivery.

```http
POST /api/events
X-API-Key: zap_test_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789
Content-Type: application/json

{
  "type": "user.created",
  "dedup_id": "unique-event-id-123",
  "payload": {
    "user_id": "12345",
    "email": "user@example.com",
    "name": "John Doe"
  }
}
```

**Fields:**
- `type` (required): Event type identifier
- `dedup_id` (optional): Unique ID for 24-hour deduplication
- `payload` (required): Event data (max 256KB)

**Response (201):**
```json
{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "type": "user.created",
  "organization_id": "550e8400-e29b-41d4-a716-446655440000",
  "created_at": "2025-11-10T14:00:00Z",
  "status": "queued"
}
```

**Error Responses:**

**409 Conflict** (Duplicate):
```json
{
  "error": "Duplicate event detected within 24-hour window",
  "dedup_id": "unique-event-id-123"
}
```

**413 Payload Too Large**:
```json
{
  "error": "Payload exceeds maximum size of 256KB"
}
```

**422 Unprocessable Entity** (No webhook URL):
```json
{
  "error": "Organization has no webhook URL configured. Use POST /api/webhook/config to set one."
}
```

**429 Too Many Requests** (Rate limit exceeded):
```json
{
  "error": "Rate limit exceeded",
  "limit": 100,
  "window": "1 minute",
  "retry_after": "60 seconds"
}
```

### 6. List Events (Inbox) (Authenticated)

List events with their delivery status.

```http
GET /api/inbox?status=pending&limit=100&offset=0
X-API-Key: zap_test_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789
```

**Query Parameters:**
- `status` (optional): Filter by status (`pending`, `delivered`, `failed`, `dead_letter`)
- `limit` (optional): Max results (default: 100, max: 1000)
- `offset` (optional): Pagination offset (default: 0)

**Response (200):**
```json
{
  "events": [
    {
      "id": "660e8400-e29b-41d4-a716-446655440001",
      "type": "user.created",
      "dedup_id": "unique-event-id-123",
      "payload": {
        "user_id": "12345",
        "email": "user@example.com"
      },
      "status": "delivered",
      "attempts": 1,
      "response_status": 200,
      "last_error": null,
      "created_at": "2025-11-10T14:00:00Z",
      "updated_at": "2025-11-10T14:00:05Z"
    }
  ],
  "pagination": {
    "total": 150,
    "limit": 100,
    "offset": 0,
    "has_more": true
  }
}
```

### 7. Acknowledge Event (Authenticated)

Manually mark an event as delivered (useful for manual recovery).

```http
POST /api/ack/:event_id
X-API-Key: zap_test_AbCdEfGhIjKlMnOpQrStUvWxYz0123456789
```

**Response (200):**
```json
{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "status": "delivered",
  "acknowledged_at": "2025-11-10T15:00:00Z",
  "message": "Event delivery acknowledged successfully"
}
```

## Webhook Delivery

### Delivery Behavior

- Events are delivered via HTTP POST to the configured webhook URL
- Automatic retries with exponential backoff (5 max attempts)
- Failed deliveries after max attempts move to Dead Letter Queue
- 30-second timeout per request

### Webhook Request Format

```http
POST <your-webhook-url>
Content-Type: application/json
User-Agent: ZapierTriggers/1.0
X-Event-ID: 660e8400-e29b-41d4-a716-446655440001
X-Event-Type: user.created

{
  "id": "660e8400-e29b-41d4-a716-446655440001",
  "type": "user.created",
  "timestamp": "2025-11-10T14:00:00Z",
  "data": {
    "user_id": "12345",
    "email": "user@example.com",
    "name": "John Doe"
  }
}
```

### Expected Response

Your webhook should return HTTP 2xx (200-299) to indicate successful delivery.

## Monitoring

### Prometheus Metrics

Metrics are exposed on port 9568 at `/metrics`

**Key Metrics:**
- `zapier_triggers_events_created_count` - Total events created
- `zapier_triggers_events_duplicate_count` - Duplicate events blocked
- `zapier_triggers_deliveries_success_count` - Successful deliveries
- `zapier_triggers_deliveries_failed_count` - Failed delivery attempts
- `zapier_triggers_deliveries_dead_letter_count` - Events in DLQ
- `zapier_triggers_deliveries_duration` - Webhook delivery duration
- `zapier_triggers_rate_limit_exceeded_count` - Rate limit hits

### Structured Logging

All logs are output in JSON format with relevant metadata:
- Request ID
- Organization ID
- Event ID
- Sensitive parameters (API keys, passwords) are automatically filtered

## Security

### API Key Security

- API keys are hashed with bcrypt before storage
- Keys are prefixed: `zap_live_` (production) or `zap_test_` (development)
- 32 bytes of cryptographically secure random data (base64 encoded)

### HTTPS/TLS

- Production enforces HTTPS with HSTS
- TLS 1.2+ with strong cipher suites
- HTTP automatically redirects to HTTPS

### Rate Limiting

- Per-organization, per-minute limits
- Based on organization tier
- Returns `X-RateLimit-Limit` and `X-RateLimit-Remaining` headers

### CORS

- Configured to allow cross-origin requests
- Can be customized in `router.ex`

## Architecture

### Stack

- **Framework**: Phoenix 1.7+ (API-only)
- **Language**: Elixir 1.14+
- **Database**: PostgreSQL 14+
- **Job Queue**: Oban (Postgres-backed)
- **Caching**: Cachex (in-memory)
- **Rate Limiting**: Hammer (ETS-backed)
- **HTTP Client**: HTTPoison
- **Logging**: LoggerJSON
- **Metrics**: Telemetry + Prometheus

### Database Schema

**Organizations:**
- Binary ID (UUID)
- Name
- API key hash (bcrypt)
- Webhook URL
- Rate limit per minute
- Tier (free/pro/business/enterprise)

**Events:**
- Binary ID (UUID)
- Type
- Dedup ID (24-hour unique constraint)
- Payload (JSONB)
- Organization reference
- Timestamps

**Event Deliveries:**
- Binary ID (UUID)
- Status (pending/delivered/failed/dead_letter)
- Attempts count
- Response status code
- Last error message
- Event reference
- Timestamps

### Job Processing

- Oban queue: `delivery` (10 workers)
- Max 5 retry attempts with exponential backoff
- Dead Letter Queue for permanent failures
- 7-day job retention
- Daily deduplication cleanup (2 AM cron)

## Development

### Run Tests

```bash
mix test
```

### Database Operations

```bash
# Create database
mix ecto.create

# Run migrations
mix ecto.migrate

# Reset database
mix ecto.reset

# Rollback last migration
mix ecto.rollback
```

### Interactive Shell

```bash
iex -S mix phx.server
```

## Production Deployment

### Environment Variables

Required:
- `DATABASE_URL` - PostgreSQL connection string
- `SECRET_KEY_BASE` - Phoenix secret (generate with `mix phx.gen.secret`)
- `PHX_HOST` - Public hostname

Optional:
- `PORT` - HTTP port (default: 4000)
- `POOL_SIZE` - Database connection pool size (default: 10)
- `PHX_SERVER` - Set to `true` to start server

### Build Release

```bash
# Build production release
MIX_ENV=prod mix release

# Run release
_build/prod/rel/zapier_triggers/bin/zapier_triggers start
```

## License

Copyright © 2025
