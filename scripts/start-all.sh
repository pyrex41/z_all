#!/bin/bash
# Start all API implementations for testing

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

echo "🚀 Starting all Zapier Triggers API implementations..."
echo ""

# Store PIDs for cleanup
PIDS=()

# Cleanup function
cleanup() {
    echo ""
    echo "🛑 Stopping all services..."
    for pid in "${PIDS[@]}"; do
        if kill -0 "$pid" 2>/dev/null; then
            kill "$pid"
        fi
    done
    exit 0
}

trap cleanup SIGINT SIGTERM

# Start Python
echo "🐍 Starting Python implementation (port 8000)..."
if [ -d "$ROOT_DIR/zapier_python" ]; then
    cd "$ROOT_DIR/zapier_python"
    uv run uvicorn src.zapier_triggers_api.main:app --port 8000 > /tmp/zapier_python.log 2>&1 &
    PIDS+=($!)
    echo "   PID: ${PIDS[-1]}"
else
    echo "   ⚠️  Not found"
fi

# Start Elixir
echo "💧 Starting Elixir implementation (port 4000)..."
if [ -d "$ROOT_DIR/zapier_elixir/zapier_triggers" ]; then
    cd "$ROOT_DIR/zapier_elixir/zapier_triggers"
    mix phx.server > /tmp/zapier_elixir.log 2>&1 &
    PIDS+=($!)
    echo "   PID: ${PIDS[-1]}"
else
    echo "   ⚠️  Not found"
fi

# Start Rust (if implemented)
if [ -d "$ROOT_DIR/zapier_rust" ] && [ -f "$ROOT_DIR/zapier_rust/Cargo.toml" ]; then
    echo "🦀 Starting Rust implementation (port 8080)..."
    cd "$ROOT_DIR/zapier_rust"
    cargo run > /tmp/zapier_rust.log 2>&1 &
    PIDS+=($!)
    echo "   PID: ${PIDS[-1]}"
fi

echo ""
echo "⏳ Waiting for services to start (5 seconds)..."
sleep 5

echo ""
echo "✅ Services started!"
echo ""
echo "Available APIs:"
echo "  - Python:  http://localhost:8000       (docs: /docs)"
echo "  - Elixir:  http://localhost:4000       (docs: /api/docs)"
if [ -f "$ROOT_DIR/zapier_rust/Cargo.toml" ]; then
    echo "  - Rust:    http://localhost:8080"
fi
echo ""
echo "Logs:"
echo "  - Python:  tail -f /tmp/zapier_python.log"
echo "  - Elixir:  tail -f /tmp/zapier_elixir.log"
if [ -f "$ROOT_DIR/zapier_rust/Cargo.toml" ]; then
    echo "  - Rust:    tail -f /tmp/zapier_rust.log"
fi
echo ""
echo "Press Ctrl+C to stop all services"
echo ""

# Wait for interrupt
wait
