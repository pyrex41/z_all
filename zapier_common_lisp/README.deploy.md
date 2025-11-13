# Deployment Guide - Common Lisp Zapier Triggers

## Prerequisites

1. **Install Fly.io CLI**
   ```bash
   curl -L https://fly.io/install.sh | sh
   ```

2. **Login to Fly.io**
   ```bash
   flyctl auth login
   ```

3. **Set up PostgreSQL database**
   ```bash
   # Create a Postgres database on Fly.io
   flyctl postgres create --name zapier-triggers-db --region sjc

   # Attach it to your app
   flyctl postgres attach zapier-triggers-db -a zapier-triggers-lisp
   ```

## Local Testing

### Build Docker image locally
```bash
./scripts/build.sh
```

### Run locally
```bash
docker run -p 5000:5000 \
  -e DATABASE_URL="postgresql://user:pass@host:5432/dbname" \
  zapier-triggers-lisp:latest
```

### Test the endpoints
```bash
# Health check
curl http://localhost:5000/health

# Queue stats
curl http://localhost:5000/api/queue/stats
```

## Deploy to Fly.io

### Initial Setup

1. **Initialize Fly app** (if not done yet)
   ```bash
   flyctl launch --no-deploy
   ```
   - Choose a unique app name (e.g., `zapier-triggers-lisp`)
   - Select region (e.g., `sjc` for San Jose)
   - Don't deploy yet

2. **Set secrets**
   ```bash
   # Database URL (automatically set if you attached Postgres above)
   # If not, set manually:
   flyctl secrets set DATABASE_URL="postgresql://user:pass@host:5432/dbname"

   # Other optional secrets
   flyctl secrets set LOG_LEVEL="info"
   ```

3. **Configure resources** (edit `fly.toml` if needed)
   - Default: 512MB RAM, 1 shared CPU
   - For higher load: upgrade to 1GB RAM, 2 CPUs

### Deploy

```bash
# Deploy using the convenience script
./scripts/deploy.sh

# Or deploy directly
flyctl deploy
```

### Monitor

```bash
# View logs
flyctl logs

# Check status
flyctl status

# Scale instances
flyctl scale count 2

# SSH into the machine
flyctl ssh console
```

## Continuous Deployment

### GitHub Actions (Optional)

Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy to Fly.io

on:
  push:
    branches: [main, master]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: superfly/flyctl-actions/setup-flyctl@master
      - run: flyctl deploy --remote-only
        env:
          FLY_API_TOKEN: ${{ secrets.FLY_API_TOKEN }}
```

Then set the `FLY_API_TOKEN` secret in GitHub:
```bash
# Get your token
flyctl auth token

# Add to GitHub repo secrets as FLY_API_TOKEN
```

## Environment Variables

Set via `fly.toml` or `flyctl secrets`:

- `PORT` - Server port (default: 5000)
- `DATABASE_URL` - PostgreSQL connection string (required)
- `ENVIRONMENT` - "production" or "development"
- `WORKER_COUNT` - Number of background workers (default: 4)
- `LOG_LEVEL` - "debug", "info", "warn", "error" (default: info)

## Troubleshooting

### Check logs
```bash
flyctl logs --app zapier-triggers-lisp
```

### Restart application
```bash
flyctl apps restart zapier-triggers-lisp
```

### SSH into machine
```bash
flyctl ssh console --app zapier-triggers-lisp
```

### Check health
```bash
curl https://zapier-triggers-lisp.fly.dev/health
```

## Performance Tuning

### Scale vertically (more resources per machine)
```bash
flyctl scale vm shared-cpu-2x --memory 1024
```

### Scale horizontally (more machines)
```bash
flyctl scale count 3  # Run 3 instances
```

### Regions
```bash
# Add more regions for global distribution
flyctl regions add lhr syd  # London, Sydney
```

## Cost Estimates

- **Free tier**: 3 shared-cpu-1x VMs with 256MB RAM each
- **Hobby plan** ($5/mo):
  - 512MB RAM, 1 shared CPU: ~$2-3/month
  - 1GB RAM, 2 shared CPUs: ~$5-7/month
- **PostgreSQL**: Free tier available, or $5-15/month for production

## Support

- Fly.io Docs: https://fly.io/docs
- Community: https://community.fly.io
