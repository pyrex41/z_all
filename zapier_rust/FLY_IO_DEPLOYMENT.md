# Fly.io Deployment Guide - Zapier Triggers API (Rust)

## Quick Start

### Prerequisites
```bash
# Install Fly.io CLI
curl -L https://fly.io/install.sh | sh

# Login to Fly.io
fly auth login
```

### Initial Setup (One-Time)

1. **Create PostgreSQL Database**
```bash
fly postgres create \
  --name zapier-triggers-db \
  --region ord \
  --vm-size shared-cpu-1x \
  --initial-cluster-size 1 \
  --volume-size 10
```

2. **Launch Application**
```bash
# Creates fly.toml interactively
fly launch --no-deploy

# Or use existing fly.toml
fly launch
```

3. **Attach Database**
```bash
fly postgres attach zapier-triggers-db --app zapier-triggers-rust
# This automatically sets DATABASE_URL secret
```

4. **Set Secrets**
```bash
fly secrets set \
  API_KEY_SALT="$(openssl rand -hex 32)" \
  WEBHOOK_SECRET="$(openssl rand -hex 32)"
```

5. **Deploy**
```bash
fly deploy
```

---

## Configuration Files

### fly.toml
Located at project root. Key configurations:
- **Region:** `ord` (Chicago) for demo
- **Internal Port:** `8080` for HTTP API
- **Metrics Port:** `9090` for Prometheus
- **Health Check:** `/health` endpoint with 30s interval
- **VM Size:** 256MB RAM, shared CPU
- **Auto-scaling:** Disabled for demo (set to 1 machine)

### Dockerfile
Multi-stage Alpine-based build:
- **Build Stage:** Rust 1.76 Alpine with musl-dev, cargo caching
- **Runtime Stage:** Alpine 3.19 minimal with ca-certificates, libgcc
- **Final Size:** ~50MB
- **Security:** Non-root user (appuser)

---

## Common Operations

### Deployment
```bash
# Deploy current code
fly deploy

# Deploy specific branch
git checkout feature-branch
fly deploy

# Deploy with build logs
fly deploy --build-only
```

### Monitoring
```bash
# View logs (live)
fly logs

# View status
fly status

# Open web dashboard
fly dashboard

# Check health
curl https://zapier-triggers-rust.fly.dev/health
```

### Database
```bash
# Connect to PostgreSQL
fly postgres connect -a zapier-triggers-db

# Run migrations manually
fly ssh console -C "zapier-triggers migrate"

# View database info
fly postgres db list -a zapier-triggers-db
```

### Scaling
```bash
# Scale VM count
fly scale count 2  # Run 2 instances

# Scale VM size
fly scale vm dedicated-cpu-1x
fly scale memory 512

# View current scale
fly scale show
```

### Debugging
```bash
# SSH into VM
fly ssh console

# Run command in VM
fly ssh console -C "ps aux"

# View secrets (hidden values)
fly secrets list

# Set new secret
fly secrets set NEW_SECRET=value

# Restart app
fly apps restart zapier-triggers-rust
```

---

## Database Migrations

### Automatic (Recommended)
Add to `fly.toml`:
```toml
[deploy]
  release_command = "zapier-triggers migrate"
```

### Manual
```bash
# SSH and run migration binary
fly ssh console -C "/usr/local/bin/zapier-triggers migrate"

# Or connect and run SQLx migrations
fly postgres connect -a zapier-triggers-db
\i /path/to/migration.sql
```

---

## CI/CD with GitHub Actions

### Setup
1. Get Fly.io API token:
```bash
fly auth token
```

2. Add to GitHub Secrets:
   - Go to repo Settings → Secrets → Actions
   - Add `FLY_API_TOKEN` with the token value

3. Workflow file at `.github/workflows/ci.yml` handles:
   - ✅ Tests on PR
   - ✅ Benchmarks on main
   - ✅ Auto-deploy to Fly.io on main merge

### Manual Trigger
```bash
# From GitHub UI: Actions → CI/CD → Run workflow
```

---

## Performance Targets on Fly.io

| Metric | Target | Verification |
|--------|--------|--------------|
| Throughput | 2,500+ req/s | `drill --benchmark benchmark.yml` |
| P95 Latency | <10ms | Fly.io dashboard metrics |
| Memory | <200MB @ load | `fly ssh console -C "ps aux"` |
| CPU | <30% @ 1K req/s | Fly.io metrics |
| Cold Start | <100ms | Firecracker VM startup |

---

## Cost Breakdown

### Demo Configuration (~$10/mo)
- **API App:** shared-cpu-1x, 256MB = $5/mo
- **PostgreSQL:** shared-cpu-1x, 256MB, 10GB = $5/mo

### Production Configuration (~$50/mo)
- **API App (2x):** dedicated-cpu-1x, 512MB each = $30/mo
- **PostgreSQL (HA):** dedicated-cpu-1x, 1GB, 50GB with replica = $20/mo

### Scaling Options
```bash
# Free tier (testing only)
fly scale vm shared-cpu-1x --memory 256

# Production tier
fly scale vm dedicated-cpu-2x --memory 1024
```

---

## Troubleshooting

### App Won't Start
```bash
# Check logs for errors
fly logs

# Verify secrets are set
fly secrets list

# Check health endpoint
curl https://zapier-triggers-rust.fly.dev/health
```

### Database Connection Issues
```bash
# Verify DATABASE_URL is set
fly secrets list | grep DATABASE_URL

# Test connection from VM
fly ssh console -C "psql $DATABASE_URL -c 'SELECT 1'"

# Check Postgres status
fly status -a zapier-triggers-db
```

### High Latency
```bash
# Check region proximity
fly regions list

# View metrics
fly dashboard

# Scale up VM
fly scale vm dedicated-cpu-1x
```

### Out of Memory
```bash
# View current memory
fly scale show

# Increase memory
fly scale memory 512
```

---

## Local Development with Fly.io

### Use Fly.io Staging Database Locally
```bash
# Proxy Fly.io Postgres to localhost
fly proxy 5432 -a zapier-triggers-db

# In another terminal, connect locally
export DATABASE_URL="postgres://user:pass@localhost:5432/dbname"
cargo run
```

### Test with Fly.io .env
```bash
# Download secrets to .env (DO NOT COMMIT!)
fly secrets list | sed 's/\s*=\s*/=/' > .env

# Use with cargo
cargo run
```

---

## Additional Resources

- **Fly.io Docs:** https://fly.io/docs/
- **Rust on Fly.io:** https://fly.io/docs/languages-and-frameworks/rust/
- **Postgres on Fly.io:** https://fly.io/docs/postgres/
- **Pricing:** https://fly.io/docs/about/pricing/

---

## Support

- **Fly.io Community:** https://community.fly.io/
- **GitHub Issues:** Report bugs in project repo
- **Project Docs:** See main README.md

