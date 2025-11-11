# Getting Started with Zapier Triggers API (Rust)

This guide will get you up and running in 5 minutes.

## 🚀 Quick Start

### Prerequisites
- Rust 1.76+ (`rustup update`)
- PostgreSQL 16+
- curl (for testing)

### Option 1: Automated Setup (Recommended)

```bash
# Run the setup script
./scripts/setup-local.sh

# Start the server
cargo run

# In another terminal, test the API
./scripts/test-api.sh
```

### Option 2: Manual Setup

#### 1. Install Dependencies
```bash
cargo build
cargo install sqlx-cli --no-default-features --features postgres
```

#### 2. Configure Environment
```bash
cp .env.example .env

# Generate secrets
export API_KEY_SALT=$(openssl rand -hex 32)
export WEBHOOK_SECRET=$(openssl rand -hex 32)

# Update .env file
sed -i '' "s/your_32_byte_hex_salt_here/$API_KEY_SALT/" .env
sed -i '' "s/your_32_byte_hex_secret_here/$WEBHOOK_SECRET/" .env
```

#### 3. Setup Database
```bash
# Create database
createdb zapier_triggers

# Run migrations
sqlx migrate run
```

#### 4. Start Server
```bash
cargo run
```

Server runs on `http://localhost:8080`

---

## 🧪 Testing the API

### Manual Testing

#### 1. Health Check
```bash
curl http://localhost:8080/health
# Response: {"status":"healthy"}
```

#### 2. Generate API Key
```bash
curl -X POST http://localhost:8080/api/keys/generate \
  -H "Content-Type: application/json" \
  -d '{
    "org_name": "my-org",
    "tier": "pro"
  }'
# Save the api_key from response
```

#### 3. Configure Webhook
```bash
curl -X POST http://localhost:8080/api/webhook/config \
  -H "Content-Type: application/json" \
  -H "X-API-Key: YOUR_API_KEY" \
  -d '{
    "webhook_url": "https://webhook.site/your-unique-url"
  }'
```

#### 4. Create Event
```bash
curl -X POST http://localhost:8080/api/events \
  -H "Content-Type: application/json" \
  -H "X-API-Key: YOUR_API_KEY" \
  -d '{
    "type": "user.signup",
    "payload": {
      "user_id": 123,
      "email": "user@example.com"
    },
    "dedup_id": "signup-123"
  }'
```

#### 5. Check Inbox
```bash
curl http://localhost:8080/api/inbox?limit=10 \
  -H "X-API-Key: YOUR_API_KEY"
```

#### 6. Acknowledge Event
```bash
curl -X POST http://localhost:8080/api/ack/EVENT_ID \
  -H "X-API-Key: YOUR_API_KEY"
```

### Automated Testing

Run the test script:
```bash
./scripts/test-api.sh
```

This will:
- Check health endpoint
- Generate API key
- Configure webhook
- Create event
- Test deduplication
- Check inbox
- Acknowledge event

---

## 📋 Common Operations

### Development

```bash
# Run in debug mode
cargo run

# Run with logs
RUST_LOG=debug cargo run

# Run tests
cargo test

# Check code without building
cargo check

# Lint code
cargo clippy

# Format code
cargo fmt

# Build release binary
cargo build --release
```

### Database

```bash
# Connect to database
psql zapier_triggers

# Run specific migration
sqlx migrate run --source migrations

# Revert last migration
sqlx migrate revert

# Check migration status
sqlx migrate info
```

### Monitoring

```bash
# View logs (structured JSON)
cargo run | jq

# Check metrics
curl http://localhost:8080/metrics

# Monitor database
psql zapier_triggers -c "SELECT COUNT(*) FROM events"
psql zapier_triggers -c "SELECT status, COUNT(*) FROM event_deliveries GROUP BY status"
```

---

## 🐛 Troubleshooting

### Server won't start

**Problem**: `Error: Connection refused`
**Solution**: Ensure PostgreSQL is running
```bash
# macOS
brew services start postgresql@16

# Linux
sudo systemctl start postgresql
```

**Problem**: `Error: database "zapier_triggers" does not exist`
**Solution**: Create the database
```bash
createdb zapier_triggers
sqlx migrate run
```

**Problem**: `Error: API_KEY_SALT must be set`
**Solution**: Check your .env file
```bash
cat .env | grep API_KEY_SALT
# Should show a 64-character hex string
```

### Build Issues

**Problem**: `error: linking with cc failed`
**Solution**: Install build dependencies
```bash
# macOS
xcode-select --install

# Ubuntu/Debian
sudo apt-get install build-essential pkg-config libssl-dev
```

**Problem**: SQLx compilation errors
**Solution**: Set DATABASE_URL for compile-time checking
```bash
export DATABASE_URL=postgres://localhost/zapier_triggers
cargo clean
cargo build
```

### API Issues

**Problem**: `401 Unauthorized`
**Solution**: Check API key header
```bash
# Ensure header is correct
curl -v http://localhost:8080/api/keys \
  -H "X-API-Key: zap_live_..."
```

**Problem**: `429 Too Many Requests`
**Solution**: Rate limit exceeded. Wait 60 seconds or upgrade tier.

**Problem**: `400 Webhook URL not configured`
**Solution**: Configure webhook first
```bash
curl -X POST http://localhost:8080/api/webhook/config \
  -H "Content-Type: application/json" \
  -H "X-API-Key: YOUR_API_KEY" \
  -d '{"webhook_url": "https://your-webhook-url.com"}'
```

---

## 🚢 Deployment

### Local Production Build

```bash
# Build optimized binary
cargo build --release

# Binary location
./target/release/zapier-triggers

# Run in production mode
DATABASE_URL=postgres://... \
API_KEY_SALT=... \
WEBHOOK_SECRET=... \
./target/release/zapier-triggers
```

### Fly.io Deployment

See [FLY_IO_DEPLOYMENT.md](./FLY_IO_DEPLOYMENT.md) for complete deployment guide.

Quick commands:
```bash
# First time setup
fly postgres create --name zapier-triggers-db
fly launch --no-deploy
fly postgres attach zapier-triggers-db
fly secrets set API_KEY_SALT="..." WEBHOOK_SECRET="..."
fly deploy

# Regular deployment
fly deploy
```

### Docker

```bash
# Build image
docker build -t zapier-triggers-rust .

# Run container
docker run -p 8080:8080 \
  -e DATABASE_URL=postgres://... \
  -e API_KEY_SALT=... \
  -e WEBHOOK_SECRET=... \
  zapier-triggers-rust
```

---

## 📊 Performance

### Benchmarking

```bash
# Install drill (load testing tool)
cargo install drill

# Create benchmark.yml (see examples in docs)
# Run load test
drill --benchmark benchmark.yml --stats
```

### Expected Performance
- **Throughput**: 2,500+ req/s (single node)
- **Latency**: <10ms P95
- **Memory**: <200MB @ 1K req/s
- **CPU**: <30% @ 1K req/s

---

## 📚 Next Steps

1. **Read Documentation**
   - [README.md](./README.md) - Project overview
   - [QUICK_REFERENCE.md](./QUICK_REFERENCE.md) - Command reference
   - [IMPLEMENTATION_STATUS.md](./IMPLEMENTATION_STATUS.md) - Current status
   - [PRD](./.taskmaster/docs/prd-init.md) - Product requirements

2. **Explore Code**
   - `src/main.rs` - Application entry point
   - `src/handlers/` - API endpoint implementations
   - `src/workers/delivery.rs` - Background worker
   - `migrations/` - Database schema

3. **Run Tests**
   - Implement integration tests
   - Run unified test suite
   - Benchmark performance

4. **Deploy**
   - Set up Fly.io account
   - Deploy to production
   - Monitor metrics

---

## 🤝 Contributing

Found a bug? Have a suggestion?

1. Check existing issues
2. Create new issue with details
3. Submit PR with tests
4. Follow code style (cargo fmt)

---

## 📞 Support

- **Issues**: GitHub Issues
- **Docs**: [README.md](./README.md)
- **Reference**: [QUICK_REFERENCE.md](./QUICK_REFERENCE.md)

---

**Happy coding! 🦀**
