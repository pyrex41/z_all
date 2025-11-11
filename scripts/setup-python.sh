#!/bin/bash
# Setup script for Python (FastAPI) implementation

set -e

echo "🐍 Setting up Python implementation..."

cd "$(dirname "$0")/../zapier_python"

# Check for UV
if ! command -v uv &> /dev/null; then
    echo "❌ UV not found. Install from: https://github.com/astral-sh/uv"
    exit 1
fi

echo "📦 Installing dependencies..."
uv sync --all-extras

echo "🗄️  Checking PostgreSQL..."
if ! command -v psql &> /dev/null; then
    echo "⚠️  PostgreSQL not found. You'll need it to run the API."
    echo "   Install: brew install postgresql@16"
fi

echo "🔴 Checking Redis..."
if ! command -v redis-cli &> /dev/null; then
    echo "⚠️  Redis not found. You'll need it to run the API."
    echo "   Install: brew install redis"
    echo "   Or run: docker run -d -p 6379:6379 redis:7"
fi

echo ""
echo "✅ Python setup complete!"
echo ""
echo "Next steps:"
echo "  1. Start PostgreSQL and Redis"
echo "  2. Create database: createdb zapier_triggers"
echo "  3. Run migrations: cd zapier_python && uv run alembic upgrade head"
echo "  4. Start server: cd zapier_python && uv run uvicorn src.zapier_triggers_api.main:app --reload"
echo "  5. Visit: http://localhost:8000/docs"
