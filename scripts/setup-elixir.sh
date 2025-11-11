#!/bin/bash
# Setup script for Elixir (Phoenix) implementation

set -e

echo "💧 Setting up Elixir implementation..."

cd "$(dirname "$0")/../zapier_elixir/zapier_triggers"

# Check for Elixir
if ! command -v elixir &> /dev/null; then
    echo "❌ Elixir not found. Install from: https://elixir-lang.org/install.html"
    exit 1
fi

# Check for Mix
if ! command -v mix &> /dev/null; then
    echo "❌ Mix not found. Should be included with Elixir."
    exit 1
fi

echo "📦 Installing dependencies..."
mix deps.get

echo "🗄️  Checking PostgreSQL..."
if ! command -v psql &> /dev/null; then
    echo "⚠️  PostgreSQL not found. You'll need it to run the API."
    echo "   Install: brew install postgresql@16"
fi

echo "🔧 Compiling project..."
mix compile

echo ""
echo "✅ Elixir setup complete!"
echo ""
echo "Next steps:"
echo "  1. Start PostgreSQL"
echo "  2. Create database: cd zapier_elixir/zapier_triggers && mix ecto.create"
echo "  3. Run migrations: mix ecto.migrate"
echo "  4. Start server: mix phx.server"
echo "  5. Visit: http://localhost:4000/api/docs"
