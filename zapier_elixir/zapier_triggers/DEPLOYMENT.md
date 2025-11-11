## Deployment Guide

Comprehensive deployment guide for Zapier Triggers API.

## Quick Start with Docker

### Local Development

```bash
# Build and start with docker-compose
docker-compose up -d

# Run migrations
docker-compose exec api /app/bin/zapier_triggers eval "ZapierTriggers.Release.migrate()"

# View logs
docker-compose logs -f api

# Stop services
docker-compose down
```

Access:
- **API**: http://localhost:4000
- **Swagger UI**: http://localhost:4000/api/docs
- **OpenAPI Spec**: http://localhost:4000/api/openapi
- **Health Check**: http://localhost:4000/health/ready
- **Prometheus Metrics**: http://localhost:9568/metrics

### Build Production Image

```bash
# Build the image
docker build -t zapier-triggers:latest .

# Run with environment variables
docker run -d \
  -p 4000:4000 \
  -p 9568:9568 \
  -e DATABASE_URL="ecto://user:pass@host/db" \
  -e SECRET_KEY_BASE="$(mix phx.gen.secret)" \
  -e PHX_HOST="api.example.com" \
  -e PHX_SERVER="true" \
  --name zapier-triggers \
  zapier-triggers:latest
```

## Deployment Platforms

### Fly.io

```bash
# Install flyctl
curl -L https://fly.io/install.sh | sh

# Login
fly auth login

# Launch app (follow prompts)
fly launch

# Deploy
fly deploy

# View logs
fly logs

# Scale
fly scale count 2
```

### Render

1. Connect your Git repository
2. Select "Docker" as environment
3. Add environment variables:
   - `DATABASE_URL` (from Render PostgreSQL)
   - `SECRET_KEY_BASE` (generate with `mix phx.gen.secret`)
   - `PHX_HOST` (your-app.onrender.com)
   - `PHX_SERVER=true`
4. Deploy

### Railway

```bash
# Install Railway CLI
npm install -g @railway/cli

# Login
railway login

# Initialize project
railway init

# Add PostgreSQL
railway add postgres

# Deploy
railway up

# View logs
railway logs
```

### Kubernetes

See `k8s/` directory for Kubernetes manifests (not included in basic setup).

## Environment Variables

### Required

```bash
# Database connection
DATABASE_URL=ecto://user:password@host:5432/database

# Phoenix secret (generate with: mix phx.gen.secret)
SECRET_KEY_BASE=your_64_char_secret

# Hostname for URL generation
PHX_HOST=api.example.com

# Start server
PHX_SERVER=true
```

### Optional

```bash
# HTTP port (default: 4000)
PORT=4000

# Database pool size (default: 10)
POOL_SIZE=10

# Enable IPv6
ECTO_IPV6=true

# Environment (prod/dev/test)
MIX_ENV=prod
```

## Health Checks

### Liveness Probe
```bash
curl http://localhost:4000/health/live
```

Response:
```json
{
  "status": "ok",
  "service": "zapier_triggers",
  "timestamp": "2025-11-10T12:00:00Z"
}
```

### Readiness Probe
```bash
curl http://localhost:4000/health/ready
```

Response:
```json
{
  "status": "ready",
  "checks": {
    "database": "ok",
    "oban": "ok",
    "cache": "ok"
  },
  "timestamp": "2025-11-10T12:00:00Z"
}
```

## Database Migrations

Migrations are run automatically on container startup via `rel/overlays/bin/migrate`:

```bash
# Manual migration
docker-compose exec api /app/bin/zapier_triggers eval "ZapierTriggers.Release.migrate()"

# Or with the release
/app/bin/zapier_triggers eval "ZapierTriggers.Release.migrate()"
```

## Monitoring

### Prometheus Metrics

Metrics are exposed on port 9568:

```bash
curl http://localhost:9568/metrics
```

Configure Prometheus to scrape:

```yaml
scrape_configs:
  - job_name: 'zapier_triggers'
    static_configs:
      - targets: ['api:9568']
```

### Logs

Structured JSON logs are written to stdout:

```bash
# Docker
docker logs -f zapier-triggers

# Docker Compose
docker-compose logs -f api

# Fly.io
fly logs

# Kubernetes
kubectl logs -f deployment/zapier-triggers
```

## Scaling

### Horizontal Scaling

The application is stateless and can be scaled horizontally:

```bash
# Docker Compose
docker-compose up -d --scale api=3

# Fly.io
fly scale count 3

# Kubernetes
kubectl scale deployment/zapier-triggers --replicas=3
```

### Vertical Scaling

Adjust resources based on load:

```yaml
# Docker Compose
services:
  api:
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G
```

## Security Checklist

- [ ] Use HTTPS in production (handled by reverse proxy/load balancer)
- [ ] Set strong `SECRET_KEY_BASE` (at least 64 characters)
- [ ] Use managed PostgreSQL with SSL (`ssl: true` in runtime.exs)
- [ ] Rotate API keys regularly
- [ ] Enable database backups
- [ ] Configure rate limits appropriately
- [ ] Set up monitoring and alerting
- [ ] Use environment-specific CORS origins (not `*`)
- [ ] Enable audit logging for sensitive operations
- [ ] Keep dependencies updated

## Troubleshooting

### Container won't start

```bash
# Check logs
docker logs zapier-triggers

# Check database connectivity
docker-compose exec api /app/bin/zapier_triggers remote
# Then run: Repo.query!("SELECT 1")
```

### Health check failing

```bash
# Check database connection
curl http://localhost:4000/health/ready

# Verify environment variables
docker inspect zapier-triggers | jq '.[0].Config.Env'
```

### Migration errors

```bash
# Reset database (DEV ONLY!)
docker-compose exec db psql -U postgres -c "DROP DATABASE zapier_triggers_dev;"
docker-compose exec db psql -U postgres -c "CREATE DATABASE zapier_triggers_dev;"
docker-compose restart api
```

### Performance issues

1. Check database connection pool size
2. Monitor Prometheus metrics
3. Review slow query logs
4. Scale horizontally if CPU-bound
5. Increase resources if memory-bound

## Backup and Recovery

### Database Backup

```bash
# Backup
docker-compose exec db pg_dump -U postgres zapier_triggers_dev > backup.sql

# Restore
docker-compose exec -T db psql -U postgres zapier_triggers_dev < backup.sql
```

### Disaster Recovery

1. **Database**: Use managed PostgreSQL with automated backups
2. **Code**: Deploy from Git tags/releases
3. **Secrets**: Store in secrets manager (AWS Secrets Manager, HashiCorp Vault)
4. **Metrics**: Export to long-term storage (Prometheus remote write)

## Rolling Updates

For zero-downtime deployments:

1. Health checks must pass before routing traffic
2. Use readiness probe with appropriate delay
3. Scale up new version alongside old
4. Wait for readiness
5. Scale down old version

```bash
# Fly.io (automatic)
fly deploy

# Kubernetes
kubectl rollout status deployment/zapier-triggers
kubectl rollout undo deployment/zapier-triggers  # if needed
```

## Support

For issues or questions:
- Check logs first
- Review health check endpoints
- Monitor Prometheus metrics
- Check GitHub issues
