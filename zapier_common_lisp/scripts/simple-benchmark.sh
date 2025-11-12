#!/bin/bash
# Simple benchmark using curl and time

set -e

echo "========================================="
echo "  Simple Server Comparison"
echo "========================================="
echo

PORT_WOO=5001
PORT_HUNCHENTOOT=5002
REQUESTS=1000

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

# Function to benchmark
simple_benchmark() {
    local server_name=$1
    local port=$2

    echo
    echo "========================================="
    echo "  Testing $server_name"
    echo "========================================="

    # Create API key
    echo "Creating API key..."
    API_KEY=$(curl -s -X POST http://localhost:$port/api/keys/generate \
        -H "Content-Type: application/json" \
        -d '{"organization_name":"benchmark","tier":"pro"}' | jq -r '.api_key')

    echo "Running $REQUESTS requests..."

    # Health endpoint test
    echo "Health endpoint:"
    START=$(date +%s%N)
    for i in $(seq 1 $REQUESTS); do
        curl -s http://localhost:$port/health > /dev/null
    done
    END=$(date +%s%N)
    DURATION_MS=$(( (END - START) / 1000000 ))
    echo "  Total: ${DURATION_MS}ms"
    echo "  Average: $((DURATION_MS / REQUESTS))ms per request"
    echo "  Throughput: $((REQUESTS * 1000 / DURATION_MS)) req/s"

    # POST event test
    echo
    echo "POST /api/events:"
    START=$(date +%s%N)
    for i in $(seq 1 100); do
        curl -s -X POST http://localhost:$port/api/events \
            -H "Content-Type: application/json" \
            -H "X-API-Key: $API_KEY" \
            -d "{\"type\":\"test\",\"payload\":{\"id\":$i},\"dedup_id\":\"bench-$i\"}" > /dev/null
    done
    END=$(date +%s%N)
    DURATION_MS=$(( (END - START) / 1000000 ))
    echo "  Total: ${DURATION_MS}ms (100 requests)"
    echo "  Average: $((DURATION_MS / 100))ms per request"
    echo "  Throughput: $((100 * 1000 / DURATION_MS)) req/s"
}

# Change to parent directory
cd "$(dirname "$0")/.."

# Test Woo
echo "Starting Woo server..."
sbcl --noinform --load start-server.lisp woo > /tmp/woo-server.log 2>&1 &
WOO_PID=$!

if wait_for_server $PORT_WOO; then
    simple_benchmark "Woo (4 workers)" $PORT_WOO
    kill $WOO_PID 2>/dev/null || true
    sleep 3
fi

# Test Hunchentoot
echo
echo "Starting Hunchentoot server..."
PORT=$PORT_HUNCHENTOOT sbcl --noinform --load start-simple-server.lisp > /tmp/hunchentoot-server.log 2>&1 &
HUNCHENTOOT_PID=$!

if wait_for_server $PORT_HUNCHENTOOT; then
    simple_benchmark "Hunchentoot" $PORT_HUNCHENTOOT
    kill $HUNCHENTOOT_PID 2>/dev/null || true
fi

echo
echo "========================================="
echo "  Benchmark Complete!"
echo "========================================="
