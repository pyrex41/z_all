#!/bin/bash
# Setup script for Rust implementation

set -e

echo "🦀 Setting up Rust implementation..."

cd "$(dirname "$0")/../zapier_rust"

# Check for Rust
if ! command -v cargo &> /dev/null; then
    echo "❌ Rust not found. Install from: https://rustup.rs/"
    exit 1
fi

# Check if there's a Cargo.toml
if [ ! -f "Cargo.toml" ]; then
    echo "⚠️  Rust implementation not yet initialized."
    echo "   The zapier_rust directory exists but has no Cargo project."
    exit 1
fi

echo "📦 Building project..."
cargo build

echo "🧪 Running tests..."
cargo test

echo ""
echo "✅ Rust setup complete!"
echo ""
echo "Next steps:"
echo "  1. Configure database connection"
echo "  2. Run: cargo run"
echo "  3. Visit: http://localhost:8080 (or configured port)"
