#!/bin/bash
# Smoke tests for Zapier Triggers API using curl
# Simple and portable - works anywhere curl is available

BASE_URL="http://localhost:5001"
PASSED=0
FAILED=0
API_KEY=""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "   Zapier Triggers API - Smoke Tests"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Running tests against: $BASE_URL"
echo ""

# Helper functions
pass() {
    echo -e "${GREEN}✅ PASS:${NC} $1"
    ((PASSED++))
}

fail() {
    echo -e "${RED}❌ FAIL:${NC} $1 - $2"
    ((FAILED++))
}

# Test 1: Health check
echo -n "Testing health check... "
RESPONSE=$(curl -s -w "\n%{http_code}" $BASE_URL/health)
STATUS=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | head -1)
if [ "$STATUS" = "200" ] && echo "$BODY" | grep -q '"status":"ok"'; then
    pass "Health check"
else
    fail "Health check" "Expected 200 with status=ok, got $STATUS"
fi

# Test 2: Cache stats
echo -n "Testing cache stats... "
RESPONSE=$(curl -s -w "\n%{http_code}" $BASE_URL/stats/cache)
STATUS=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | head -1)
if [ "$STATUS" = "200" ] && echo "$BODY" | grep -q '"size"'; then
    pass "Cache stats endpoint"
else
    fail "Cache stats endpoint" "Expected 200 with size, got $STATUS"
fi

# Test 3: Generate API key
echo -n "Testing API key generation... "
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST $BASE_URL/api/keys/generate \
    -H "Content-Type: application/json" \
    -d '{"organization_name":"SmokeTestOrg","tier":"free"}')
STATUS=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | head -1)
if [ "$STATUS" = "200" ] && echo "$BODY" | grep -q '"api-key":"sk_'; then
    API_KEY=$(echo "$BODY" | sed 's/.*"api-key":"\([^"]*\)".*/\1/')
    echo "   Generated API key: $API_KEY"
    pass "Generate API key"
else
    fail "Generate API key" "Expected 200 with api-key, got $STATUS"
fi

# Test 4: Create event without API key (should fail)
echo -n "Testing auth requirement... "
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST $BASE_URL/api/events \
    -H "Content-Type: application/json" \
    -d '{"type":"test.event","payload":{"test":true}}')
STATUS=$(echo "$RESPONSE" | tail -1)
if [ "$STATUS" = "401" ]; then
    pass "Create event without API key (auth required)"
else
    fail "Create event without API key" "Expected 401, got $STATUS"
fi

# Test 5: Create event with API key
if [ -n "$API_KEY" ]; then
    echo -n "Testing event creation... "
    TIMESTAMP=$(date +%s)
    RESPONSE=$(curl -s -w "\n%{http_code}" -X POST $BASE_URL/api/events \
        -H "Content-Type: application/json" \
        -H "x-api-key: $API_KEY" \
        -d "{\"type\":\"smoke.test\",\"payload\":{\"test\":true},\"dedup_id\":\"smoke-$TIMESTAMP\"}")
    STATUS=$(echo "$RESPONSE" | tail -1)
    BODY=$(echo "$RESPONSE" | head -1)
    if [ "$STATUS" = "200" ] && echo "$BODY" | grep -q '"status":"accepted"'; then
        EVENT_ID=$(echo "$BODY" | sed 's/.*"event-id":"\([^"]*\)".*/\1/')
        echo "   Created event: $EVENT_ID"
        pass "Create event"
    else
        fail "Create event" "Expected 200 with status=accepted, got $STATUS"
    fi
else
    fail "Create event" "No API key available"
fi

# Test 6: Duplicate detection
if [ -n "$API_KEY" ]; then
    echo -n "Testing duplicate detection... "
    DEDUP_ID="dedup-test-$(date +%s)"
    DATA="{\"type\":\"dedup.test\",\"payload\":{\"num\":1},\"dedup_id\":\"$DEDUP_ID\"}"

    # Create first event
    RESPONSE1=$(curl -s -w "\n%{http_code}" -X POST $BASE_URL/api/events \
        -H "Content-Type: application/json" \
        -H "x-api-key: $API_KEY" \
        -d "$DATA")
    STATUS1=$(echo "$RESPONSE1" | tail -1)
    BODY1=$(echo "$RESPONSE1" | head -1)

    if [ "$STATUS1" = "200" ] && echo "$BODY1" | grep -q '"status":"accepted"'; then
        # Try to create duplicate
        sleep 0.2
        RESPONSE2=$(curl -s -w "\n%{http_code}" -X POST $BASE_URL/api/events \
            -H "Content-Type: application/json" \
            -H "x-api-key: $API_KEY" \
            -d "$DATA")
        STATUS2=$(echo "$RESPONSE2" | tail -1)
        BODY2=$(echo "$RESPONSE2" | head -1)

        if [ "$STATUS2" = "200" ] && echo "$BODY2" | grep -q '"status":"duplicate"'; then
            pass "Duplicate detection"
        else
            fail "Duplicate detection" "Expected duplicate status, got $STATUS2"
        fi
    else
        fail "Duplicate detection" "First event not accepted"
    fi
else
    fail "Duplicate detection" "No API key available"
fi

# Test 7: Get inbox
if [ -n "$API_KEY" ]; then
    echo -n "Testing inbox retrieval... "
    RESPONSE=$(curl -s -w "\n%{http_code}" "$BASE_URL/api/inbox?limit=10" \
        -H "x-api-key: $API_KEY")
    STATUS=$(echo "$RESPONSE" | tail -1)
    BODY=$(echo "$RESPONSE" | head -1)
    if [ "$STATUS" = "200" ] && echo "$BODY" | grep -q '"events"'; then
        COUNT=$(echo "$BODY" | sed 's/.*"count":\([0-9]*\).*/\1/')
        echo "   Retrieved $COUNT events from inbox"
        pass "Get inbox"
    else
        fail "Get inbox" "Expected 200 with events, got $STATUS"
    fi
else
    fail "Get inbox" "No API key available"
fi

# Test 8: Invalid API key
echo -n "Testing invalid API key... "
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST $BASE_URL/api/events \
    -H "Content-Type: application/json" \
    -H "x-api-key: sk_invalid_key_12345" \
    -d '{"type":"test.event","payload":{"test":true}}')
STATUS=$(echo "$RESPONSE" | tail -1)
if [ "$STATUS" = "401" ]; then
    pass "Invalid API key rejected"
else
    fail "Invalid API key rejected" "Expected 401, got $STATUS"
fi

# Print summary
echo ""
echo "═══════════════════════════════════════════════════════════"
echo "   Test Summary"
echo "═══════════════════════════════════════════════════════════"
echo ""
TOTAL=$((PASSED + FAILED))
if [ $TOTAL -gt 0 ]; then
    PERCENT=$((PASSED * 100 / TOTAL))
else
    PERCENT=0
fi
echo "Total:  $TOTAL tests"
echo "Passed: $PASSED ($PERCENT%)"
echo "Failed: $FAILED"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✅ All tests passed!${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ Some tests failed${NC}"
    echo ""
    exit 1
fi
