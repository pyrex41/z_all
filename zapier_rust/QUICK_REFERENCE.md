# Zapier Triggers API (Rust) - Quick Reference

## 🚀 Deployment Commands (Fly.io)

### First Time Setup
```bash
# 1. Create database
fly postgres create --name zapier-triggers-db --region ord --vm-size shared-cpu-1x

# 2. Launch app (creates fly.toml)
fly launch --no-deploy

# 3. Attach database
fly postgres attach zapier-triggers-db --app zapier-triggers-rust

# 4. Set secrets
fly secrets set API_KEY_SALT="$(openssl rand -hex 32)" WEBHOOK_SECRET="$(openssl rand -hex 32)"

# 5. Deploy
fly deploy
```

### Regular Deployment
```bash
fly deploy                    # Deploy current code
fly logs                      # View logs
fly status                    # Check status
fly dashboard                 # Open web UI
```

---

## 📊 Performance Targets

| Metric | Target | Command to Verify |
|--------|--------|-------------------|
| Throughput | 2,500+ req/s | `drill --benchmark benchmark.yml` |
| P95 Latency | <10ms | Fly.io metrics dashboard |
| Memory | <200MB @ load | `fly ssh console -C "free -m"` |
| CPU | <30% @ 1K req/s | Fly.io metrics |
| Binary Size | <20MB | `ls -lh target/release/zapier-triggers` |
| Cold Start | <100ms | Firecracker VM (automatic) |

---

## 🔧 Common Operations

### Monitoring
```bash
fly logs                      # Live logs
fly status                    # App status
fly dashboard                 # Web dashboard
curl https://your-app.fly.dev/health  # Health check
curl https://your-app.fly.dev/metrics # Prometheus metrics
```

### Database
```bash
fly postgres connect -a zapier-triggers-db        # Connect to DB
fly postgres db list -a zapier-triggers-db        # List databases
fly ssh console -C "zapier-triggers migrate"      # Run migrations
```

### Scaling
```bash
fly scale count 2                    # Run 2 instances
fly scale vm dedicated-cpu-1x        # Upgrade VM
fly scale memory 512                 # Increase memory
fly scale show                       # Show current scale
```

### Debugging
```bash
fly ssh console                      # SSH into VM
fly secrets list                     # View secrets (hidden)
fly apps restart your-app            # Restart app
```

---

## 📁 Project Structure

```
zapier-triggers-rust/
├── Cargo.toml                # Dependencies
├── Dockerfile                # Multi-stage build
├── fly.toml                  # Fly.io config
├── .github/workflows/ci.yml  # CI/CD pipeline
├── src/
│   ├── main.rs              # App entry point
│   ├── handlers/            # API endpoints
│   │   └── events.rs
│   ├── workers/             # Background workers
│   │   └── delivery.rs
│   └── middleware/          # Auth, rate limiting
│       └── auth.rs
├── migrations/              # Database migrations
│   ├── 001_organizations.sql
│   ├── 002_events.sql
│   └── 003_deliveries.sql
└── tests/                   # Integration tests
```

---

## 🔐 Secrets Management

### Required Secrets
```bash
DATABASE_URL       # Auto-set by fly postgres attach
API_KEY_SALT       # fly secrets set API_KEY_SALT="..."
WEBHOOK_SECRET     # fly secrets set WEBHOOK_SECRET="..."
```

### View Secrets
```bash
fly secrets list              # List all secrets (values hidden)
```

---

## 💰 Cost Estimates

### Demo (~$10/mo)
- API App: shared-cpu-1x, 256MB = $5/mo
- PostgreSQL: shared-cpu-1x, 10GB = $5/mo

### Production (~$50/mo)
- API App (2x): dedicated-cpu-1x, 512MB = $30/mo
- PostgreSQL (HA): dedicated-cpu-1x, 50GB + replica = $20/mo

---

## 🧪 Testing

### Local
```bash
cargo test                    # Unit tests
cargo test --test integration # Integration tests
cargo clippy                  # Linting
cargo fmt                     # Formatting
cargo bench                   # Benchmarks
```

### Load Testing
```bash
drill --benchmark benchmark.yml --stats
```

---

## 🔄 CI/CD (GitHub Actions)

### Workflow
1. **PR:** Runs tests, clippy, fmt
2. **Main Merge:** Tests + benchmarks + auto-deploy to Fly.io
3. **Manual:** GitHub Actions UI → Run workflow

### Setup
```bash
# Get Fly.io token
fly auth token

# Add to GitHub Secrets
# Settings → Secrets → Actions → FLY_API_TOKEN
```

---

## 📚 Key Files

| File | Purpose |
|------|---------|
| `prd-init.md` | Product requirements document |
| `tasks.json` | Task breakdown |
| `FLY_IO_DEPLOYMENT.md` | Detailed deployment guide |
| `fly.toml` | Fly.io configuration |
| `Dockerfile` | Container image |
| `.github/workflows/ci.yml` | CI/CD pipeline |

---

## 🆘 Troubleshooting

### App won't start
```bash
fly logs                      # Check error logs
fly secrets list              # Verify secrets are set
curl https://your-app.fly.dev/health  # Test health endpoint
```

### Database issues
```bash
fly postgres connect -a zapier-triggers-db  # Connect to DB
fly status -a zapier-triggers-db           # Check DB status
```

### High latency
```bash
fly regions list              # Check region proximity
fly scale vm dedicated-cpu-1x # Upgrade VM
```

### Out of memory
```bash
fly scale memory 512          # Increase memory
fly scale show                # Check current settings
```

---

## 🔗 Links

- **Fly.io Docs:** https://fly.io/docs/
- **Rust on Fly.io:** https://fly.io/docs/languages-and-frameworks/rust/
- **Project PRD:** `.taskmaster/docs/prd-init.md`
- **Deployment Guide:** `FLY_IO_DEPLOYMENT.md`

---

**Last Updated:** November 10, 2025
**Version:** 1.0 (Rust Implementation)
