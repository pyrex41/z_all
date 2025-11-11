#!/bin/bash
set -e

echo "🦀 Setting up Zapier Triggers API (Rust) for local development"
echo "================================================================"

# Check prerequisites
echo ""
echo "📋 Checking prerequisites..."

if ! command -v cargo &> /dev/null; then
    echo "❌ Rust not found. Install from https://rustup.rs/"
    exit 1
fi
echo "✅ Rust found: $(rustc --version)"

if ! command -v psql &> /dev/null; then
    echo "❌ PostgreSQL not found. Install PostgreSQL 16+"
    exit 1
fi
echo "✅ PostgreSQL found: $(psql --version)"

# Install sqlx-cli if not present
if ! command -v sqlx &> /dev/null; then
    echo ""
    echo "📦 Installing sqlx-cli..."
    cargo install sqlx-cli --no-default-features --features postgres
fi
echo "✅ sqlx-cli found"

# Setup environment
echo ""
echo "🔧 Setting up environment..."
if [ ! -f .env ]; then
    cp .env.example .env

    # Generate random secrets
    API_KEY_SALT=$(openssl rand -hex 32)
    WEBHOOK_SECRET=$(openssl rand -hex 32)

    # Update .env file
    sed -i.bak "s/your_32_byte_hex_salt_here/$API_KEY_SALT/" .env
    sed -i.bak "s/your_32_byte_hex_secret_here/$WEBHOOK_SECRET/" .env
    rm .env.bak

    echo "✅ .env file created with generated secrets"
else
    echo "✅ .env file already exists"
fi

# Check database
echo ""
echo "🗄️  Setting up database..."
DB_NAME="zapier_triggers"
if psql -lqt | cut -d \| -f 1 | grep -qw "$DB_NAME"; then
    echo "✅ Database '$DB_NAME' already exists"
else
    echo "📝 Creating database '$DB_NAME'..."
    createdb "$DB_NAME"
    echo "✅ Database created"
fi

# Run migrations
echo ""
echo "🔄 Running migrations..."
sqlx migrate run
echo "✅ Migrations complete"

# Build project
echo ""
echo "🔨 Building project..."
cargo build
echo "✅ Build complete"

# Success message
echo ""
echo "🎉 Setup complete!"
echo ""
echo "To start the server:"
echo "  cargo run"
echo ""
echo "To run tests:"
echo "  cargo test"
echo ""
echo "To check code:"
echo "  cargo clippy"
echo ""
echo "Server will be available at: http://localhost:8080"
echo "Health check: http://localhost:8080/health"
echo ""
