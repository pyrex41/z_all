#!/bin/bash
# Production-optimized startup with proper connection pooling

cd "$(dirname "$0")"
source .venv/bin/activate

# PostgreSQL default max_connections = 100
# Reserve 10 for maintenance = 90 available
# Formula: workers × (pool_size + max_overflow) < 90

# Set performance environment variables
export WORKERS=4
export POOL_SIZE=15          # 15 connections per worker
export MAX_OVERFLOW=5        # Up to 5 extra per worker
# Total: 4 × (15 + 5) = 80 connections (safe!)

export SQLALCHEMY_ECHO=false
export LOG_LEVEL=warning

# Kill existing processes
pkill -f "gunicorn.*zapier_triggers"
pkill -f "uvicorn.*zapier_triggers"
sleep 2

# Verify Redis is running
if ! redis-cli ping &>/dev/null; then
    echo "⚠️  WARNING: Redis is not running!"
    echo "Start Redis: redis-server"
    echo ""
fi

# Start with optimized settings
echo "Starting Python API with Gunicorn (Production Config)"
echo "  - Workers: $WORKERS"
echo "  - DB pool per worker: $POOL_SIZE (overflow: $MAX_OVERFLOW)"
echo "  - Total max DB connections: $((WORKERS * (POOL_SIZE + MAX_OVERFLOW)))"
echo "  - PostgreSQL max_connections: 100 (default)"
echo "  - Safety margin: $((100 - WORKERS * (POOL_SIZE + MAX_OVERFLOW))) connections"
echo "  - Redis caching: ENABLED"
echo "  - Query logging: disabled"
echo ""

# Start Gunicorn
DATABASE_POOL_SIZE=$POOL_SIZE \
DATABASE_MAX_OVERFLOW=$MAX_OVERFLOW \
gunicorn src.zapier_triggers_api.main:app \
    --workers $WORKERS \
    --worker-class uvicorn.workers.UvicornWorker \
    --bind 0.0.0.0:8000 \
    --timeout 30 \
    --graceful-timeout 10 \
    --max-requests 10000 \
    --max-requests-jitter 1000 \
    --log-level warning \
    --access-logfile - \
    --error-logfile -
