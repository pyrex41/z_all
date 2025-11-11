#!/bin/bash
# Production-ready FastAPI startup with Gunicorn

cd "$(dirname "$0")"
source .venv/bin/activate

# Set performance environment variables
export DATABASE_POOL_SIZE=30
export DATABASE_MAX_OVERFLOW=20
export SQLALCHEMY_ECHO=false  # Disable query logging
export LOG_LEVEL=warning

# Detect number of CPU cores
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    WORKERS=$(sysctl -n hw.ncpu)
else
    # Linux
    WORKERS=$(nproc)
fi

# Use 2x cores for I/O-bound workloads
WORKERS=$((WORKERS * 2))

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
echo "Starting Python API with Gunicorn + Uvicorn workers"
echo "  - Workers: $WORKERS (CPU cores × 2)"
echo "  - Database pool: 30 (overflow: 20)"
echo "  - Query logging: disabled"
echo "  - Worker class: UvicornWorker (async)"
echo "  - Max requests per worker: 10,000"
echo ""

gunicorn src.zapier_triggers_api.main:app \
    --config gunicorn.conf.py \
    --workers $WORKERS \
    --bind 0.0.0.0:8000
