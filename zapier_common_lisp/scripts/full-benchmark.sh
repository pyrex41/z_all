#!/bin/bash
# Full benchmark using bench_single.py for consistent comparison

set -e

echo "========================================="
echo "  Full Benchmark - Woo vs Hunchentoot"
echo "========================================="
echo

PORT_WOO=5001
PORT_HUNCHENTOOT=5002
BENCH_SCRIPT="/tmp/bench_single.py"

# Check if bench script exists
if [ ! -f "$BENCH_SCRIPT" ]; then
    echo "Error: $BENCH_SCRIPT not found"
    echo "This script requires the bench_single.py from the gauntlet benchmarks"
    exit 1
fi

# Check if httpx is installed
if ! python3 -c "import httpx" 2>/dev/null; then
    echo "Error: httpx module not found"
    echo "Please install: pip3 install --user httpx"
    echo "Or use: python3 -m pip install --break-system-packages httpx"
    exit 1
fi

# Function to wait for server
wait_for_server() {
    local port=$1
    local max_attempts=30
    local attempt=0

    echo "Waiting for server on port $port..."
    while [ $attempt -lt $max_attempts ]; do
        if curl -s http://localhost:$port/health > /dev/null 2>&1; then
            echo "Server ready!"
            return 0
        fi
        sleep 1
        attempt=$((attempt + 1))
    done

    echo "Server failed to start"
    return 1
}

# Function to setup API key
setup_api_key() {
    local port=$1
    echo "Creating API key..."

    API_KEY=$(curl -s -X POST http://localhost:$port/api/keys/generate \
        -H "Content-Type: application/json" \
        -d '{"organization_name":"benchmark","tier":"pro"}' | \
        python3 -c "import sys, json; print(json.load(sys.stdin)['api_key'])" 2>/dev/null)

    if [ -z "$API_KEY" ]; then
        echo "Error: Failed to create API key"
        return 1
    fi

    echo "API Key: $API_KEY"

    # Update bench_single.py with the API key
    sed -i.bak "s/API_KEY = \".*\"/API_KEY = \"$API_KEY\"/" "$BENCH_SCRIPT"
    return 0
}

# Change to parent directory
cd "$(dirname "$0")/.."

echo
echo "========================================="
echo "  Testing Woo (4 workers)"
echo "========================================="

# Start Woo server
echo "Starting Woo server on port $PORT_WOO..."
sbcl --noinform --load start-server.lisp woo > /tmp/woo-bench.log 2>&1 &
WOO_PID=$!
echo "Woo PID: $WOO_PID"

if wait_for_server $PORT_WOO && setup_api_key $PORT_WOO; then
    # Run benchmark
    python3 "$BENCH_SCRIPT" "http://localhost:$PORT_WOO" "Woo (4 workers)" || true

    # Stop Woo server
    echo
    echo "Stopping Woo server..."
    kill $WOO_PID 2>/dev/null || true
    sleep 3
else
    echo "Failed to start Woo server or setup API key"
    kill $WOO_PID 2>/dev/null || true
    exit 1
fi

echo
echo "========================================="
echo "  Testing Hunchentoot"
echo "========================================="

# Start Hunchentoot server
echo "Starting Hunchentoot server on port $PORT_HUNCHENTOOT..."
PORT=$PORT_HUNCHENTOOT sbcl --noinform --load start-simple-server.lisp > /tmp/hunchentoot-bench.log 2>&1 &
HUNCHENTOOT_PID=$!
echo "Hunchentoot PID: $HUNCHENTOOT_PID"

if wait_for_server $PORT_HUNCHENTOOT && setup_api_key $PORT_HUNCHENTOOT; then
    # Run benchmark
    python3 "$BENCH_SCRIPT" "http://localhost:$PORT_HUNCHENTOOT" "Hunchentoot" || true

    # Stop Hunchentoot server
    echo
    echo "Stopping Hunchentoot server..."
    kill $HUNCHENTOOT_PID 2>/dev/null || true
else
    echo "Failed to start Hunchentoot server or setup API key"
    kill $HUNCHENTOOT_PID 2>/dev/null || true
    exit 1
fi

# Restore bench_single.py
if [ -f "${BENCH_SCRIPT}.bak" ]; then
    mv "${BENCH_SCRIPT}.bak" "$BENCH_SCRIPT"
fi

echo
echo "========================================="
echo "  Benchmark Complete!"
echo "========================================="
echo
echo "Server logs:"
echo "  Woo:         /tmp/woo-bench.log"
echo "  Hunchentoot: /tmp/hunchentoot-bench.log"
