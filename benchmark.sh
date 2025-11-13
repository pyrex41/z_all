#!/usr/bin/env bash
set -e

# Zapier Triggers API - Deployment Benchmark Script
# Usage: ./benchmark.sh <impl> <num_requests> <concurrency>
# Example: ./benchmark.sh elixir 1000 50

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to get URL for implementation
get_url() {
    case "$1" in
        python) echo "https://zapier-python.fly.dev" ;;
        elixir) echo "https://zapier-elixir.fly.dev" ;;
        rust) echo "https://zapier-triggers-rust.fly.dev" ;;
        lisp) echo "https://zapier-triggers-lisp.fly.dev" ;;
        *) echo "" ;;
    esac
}

# Function to print usage
print_usage() {
    echo -e "${BLUE}Usage:${NC} $0 <implementation> <num_requests> <concurrency>"
    echo ""
    echo -e "${BLUE}Implementations:${NC}"
    echo "  python  - Python (FastAPI) deployment"
    echo "  elixir  - Elixir (Phoenix) deployment"
    echo "  rust    - Rust (Axum) deployment"
    echo "  lisp    - Common Lisp (SBCL/Woo) deployment"
    echo ""
    echo -e "${BLUE}Examples:${NC}"
    echo "  $0 elixir 1000 50    # 1000 requests, 50 concurrent"
    echo "  $0 rust 5000 100     # 5000 requests, 100 concurrent"
    echo "  $0 python 500 25     # 500 requests, 25 concurrent"
}

# Check arguments
if [ $# -ne 3 ]; then
    print_usage
    exit 1
fi

IMPL=$1
NUM_REQUESTS=$2
CONCURRENCY=$3

# Validate implementation
BASE_URL=$(get_url "$IMPL")
if [ -z "$BASE_URL" ]; then
    echo -e "${RED}Error:${NC} Invalid implementation '$IMPL'"
    echo ""
    print_usage
    exit 1
fi

# Validate numeric arguments
if ! [[ "$NUM_REQUESTS" =~ ^[0-9]+$ ]] || ! [[ "$CONCURRENCY" =~ ^[0-9]+$ ]]; then
    echo -e "${RED}Error:${NC} num_requests and concurrency must be positive integers"
    exit 1
fi

if [ "$CONCURRENCY" -gt "$NUM_REQUESTS" ]; then
    echo -e "${YELLOW}Warning:${NC} Concurrency ($CONCURRENCY) is greater than num_requests ($NUM_REQUESTS)"
    echo "Setting concurrency to $NUM_REQUESTS"
    CONCURRENCY=$NUM_REQUESTS
fi

# Check if wrk is installed
if ! command -v wrk &> /dev/null; then
    echo -e "${RED}Error:${NC} 'wrk' is not installed"
    echo ""
    echo "Install wrk:"
    echo "  macOS:   brew install wrk"
    echo "  Ubuntu:  sudo apt-get install wrk"
    echo "  GitHub:  https://github.com/wg/wrk"
    exit 1
fi

echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  Zapier Triggers API - Benchmark${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${GREEN}Implementation:${NC} $IMPL"
echo -e "${GREEN}Target URL:${NC}     $BASE_URL"
echo -e "${GREEN}Total Requests:${NC} $NUM_REQUESTS"
echo -e "${GREEN}Concurrency:${NC}    $CONCURRENCY"
echo ""

# Step 1: Wake up the server by hitting health endpoint
echo -e "${YELLOW}[1/3]${NC} Waking up server (hitting health endpoint)..."
HEALTH_RESPONSE=$(curl -s -w "\n%{http_code}" "$BASE_URL/health")
HEALTH_CODE=$(echo "$HEALTH_RESPONSE" | tail -n 1)
HEALTH_BODY=$(echo "$HEALTH_RESPONSE" | sed '$d')

if [ "$HEALTH_CODE" -eq 200 ]; then
    echo -e "${GREEN}✓${NC} Server is awake (HTTP $HEALTH_CODE)"
    echo "  Response: $HEALTH_BODY"
else
    echo -e "${RED}✗${NC} Health check failed (HTTP $HEALTH_CODE)"
    echo "  Response: $HEALTH_BODY"
    echo ""
    echo -e "${YELLOW}Note:${NC} Server may be cold-starting. Waiting 10 seconds..."
    sleep 10

    # Retry health check
    HEALTH_RESPONSE=$(curl -s -w "\n%{http_code}" "$BASE_URL/health")
    HEALTH_CODE=$(echo "$HEALTH_RESPONSE" | tail -n 1)

    if [ "$HEALTH_CODE" -ne 200 ]; then
        echo -e "${RED}Error:${NC} Server is not responding. Aborting benchmark."
        exit 1
    fi
    echo -e "${GREEN}✓${NC} Server is now awake (HTTP $HEALTH_CODE)"
fi
echo ""

# Step 2: Generate API key
echo -e "${YELLOW}[2/3]${NC} Generating API key..."
API_KEY_RESPONSE=$(curl -s -X POST "$BASE_URL/api/keys/generate" \
    -H "Content-Type: application/json" \
    -d '{"organization_name":"Benchmark Test","tier":"enterprise"}')

# Extract API key (works for both JSON formats)
API_KEY=$(echo "$API_KEY_RESPONSE" | grep -oE '"api[_-]key":\s*"[^"]+"' | cut -d'"' -f4)

if [ -z "$API_KEY" ]; then
    echo -e "${RED}✗${NC} Failed to generate API key"
    echo "  Response: $API_KEY_RESPONSE"
    exit 1
fi

echo -e "${GREEN}✓${NC} API key generated: ${API_KEY:0:20}..."
echo ""

# Step 3: Run benchmark
echo -e "${YELLOW}[3/3]${NC} Running benchmark with wrk..."
echo ""

# Calculate duration based on requests and concurrency (estimate ~100 req/s baseline)
DURATION=$((NUM_REQUESTS / 100 + 10))
if [ $DURATION -lt 10 ]; then
    DURATION=10
fi

# Create Lua script for POST requests
TIMESTAMP=$(date +%s)
LUA_SCRIPT=$(cat <<'EOF'
wrk.method = "POST"
wrk.headers["Content-Type"] = "application/json"

request = function()
    local id = math.random(1, 999999999)
    local body = string.format('{"type":"benchmark.test","payload":{"benchmark_id":"%d","test":"wrk"},"dedup_id":"dedup-%d"}', id, id)
    return wrk.format(nil, nil, nil, body)
end
EOF
)

# Add API key to script
LUA_SCRIPT=$(cat <<EOF
$LUA_SCRIPT
wrk.headers["X-API-Key"] = "$API_KEY"
EOF
)

# Write Lua script to temp file
SCRIPT_FILE=$(mktemp)
echo "$LUA_SCRIPT" > "$SCRIPT_FILE"

# Run wrk
wrk -t$CONCURRENCY -c$CONCURRENCY -d${DURATION}s \
    -s "$SCRIPT_FILE" \
    "$BASE_URL/api/events"

# Cleanup
rm -f "$SCRIPT_FILE"

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}Benchmark complete!${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
