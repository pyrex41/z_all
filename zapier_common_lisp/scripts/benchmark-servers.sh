#!/bin/bash
# Benchmark script to compare Woo vs Hunchentoot performance

set -e

echo "========================================="
echo "  Zapier Triggers - Server Benchmarks"
echo "========================================="
echo

# Configuration
DURATION=30
CONNECTIONS=100
THREADS=4
PORT_WOO=5001
PORT_HUNCHENTOOT=5002

# Check if wrk is installed
if ! command -v wrk &> /dev/null; then
    echo "Error: wrk is not installed"
    echo "Install with: brew install wrk"
    exit 1
fi

# Function to wait for server to be ready
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

    echo "Server failed to start on port $port"
    return 1
}

# Function to benchmark a server
benchmark_server() {
    local server_name=$1
    local port=$2

    echo
    echo "========================================="
    echo "  Benchmarking $server_name"
    echo "========================================="
    echo

    # Create API key for testing
    echo "Creating test API key..."
    API_KEY=$(curl -s -X POST http://localhost:$port/api/keys/generate \
        -H "Content-Type: application/json" \
        -d '{"organization_name":"benchmark","tier":"pro"}' | jq -r '.api_key')

    if [ -z "$API_KEY" ] || [ "$API_KEY" = "null" ]; then
        echo "Error: Failed to create API key"
        return 1
    fi

    echo "API Key: $API_KEY"
    echo

    # Warm-up
    echo "Warming up..."
    wrk -t2 -c10 -d5s http://localhost:$port/health > /dev/null 2>&1
    sleep 2

    # Benchmark health endpoint
    echo "Benchmarking /health endpoint..."
    wrk -t$THREADS -c$CONNECTIONS -d${DURATION}s http://localhost:$port/health

    echo
    echo "Benchmarking /api/events endpoint (POST with dedup)..."

    # Create Lua script for POST benchmark
    cat > /tmp/post-event.lua << 'EOF'
wrk.method = "POST"
wrk.headers["Content-Type"] = "application/json"
wrk.headers["X-API-Key"] = os.getenv("API_KEY")

counter = 0
function request()
    counter = counter + 1
    local body = string.format('{"type":"user.signup","payload":{"user_id":%d},"dedup_id":"bench-%d"}', counter, counter)
    return wrk.format(nil, nil, nil, body)
end
EOF

    API_KEY=$API_KEY wrk -t$THREADS -c$CONNECTIONS -d${DURATION}s -s /tmp/post-event.lua http://localhost:$port/api/events

    echo
    echo "Benchmarking /api/inbox endpoint (GET)..."
    wrk -t$THREADS -c$CONNECTIONS -d${DURATION}s \
        -H "X-API-Key: $API_KEY" \
        http://localhost:$port/api/inbox

    rm -f /tmp/post-event.lua
}

# Start Woo server
echo "Starting Woo server on port $PORT_WOO..."
sbcl --noinform --load start-server.lisp woo > /tmp/woo-server.log 2>&1 &
WOO_PID=$!
echo "Woo PID: $WOO_PID"

if wait_for_server $PORT_WOO; then
    benchmark_server "Woo (4 workers)" $PORT_WOO

    # Stop Woo server
    echo
    echo "Stopping Woo server..."
    kill $WOO_PID 2>/dev/null || true
    sleep 3
else
    echo "Failed to start Woo server"
    kill $WOO_PID 2>/dev/null || true
    exit 1
fi

# Start Hunchentoot server
echo
echo "Starting Hunchentoot server on port $PORT_HUNCHENTOOT..."
PORT=$PORT_HUNCHENTOOT sbcl --noinform --load start-simple-server.lisp > /tmp/hunchentoot-server.log 2>&1 &
HUNCHENTOOT_PID=$!
echo "Hunchentoot PID: $HUNCHENTOOT_PID"

if wait_for_server $PORT_HUNCHENTOOT; then
    benchmark_server "Hunchentoot" $PORT_HUNCHENTOOT

    # Stop Hunchentoot server
    echo
    echo "Stopping Hunchentoot server..."
    kill $HUNCHENTOOT_PID 2>/dev/null || true
    sleep 3
else
    echo "Failed to start Hunchentoot server"
    kill $HUNCHENTOOT_PID 2>/dev/null || true
    exit 1
fi

echo
echo "========================================="
echo "  Benchmark Complete!"
echo "========================================="
echo
echo "Server logs:"
echo "  Woo:         /tmp/woo-server.log"
echo "  Hunchentoot: /tmp/hunchentoot-server.log"
