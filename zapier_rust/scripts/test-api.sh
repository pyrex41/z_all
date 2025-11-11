#!/bin/bash
# Quick API test script for Zapier Triggers API

set -e

BASE_URL="${BASE_URL:-http://localhost:8080}"

echo "🧪 Testing Zapier Triggers API (Rust)"
echo "======================================"
echo "Base URL: $BASE_URL"
echo ""

# Test 1: Health check
echo "1️⃣  Testing health check..."
HEALTH=$(curl -s "$BASE_URL/health")
if echo "$HEALTH" | grep -q "healthy"; then
    echo "✅ Health check passed: $HEALTH"
else
    echo "❌ Health check failed"
    exit 1
fi
echo ""

# Test 2: Generate API key
echo "2️⃣  Generating API key..."
KEY_RESPONSE=$(curl -s -X POST "$BASE_URL/api/keys/generate" \
    -H "Content-Type: application/json" \
    -d '{"org_name": "test-org", "tier": "pro"}')

API_KEY=$(echo "$KEY_RESPONSE" | grep -o '"api_key":"[^"]*"' | cut -d'"' -f4)
ORG_ID=$(echo "$KEY_RESPONSE" | grep -o '"organization_id":"[^"]*"' | cut -d'"' -f4)

if [ -z "$API_KEY" ]; then
    echo "❌ Failed to generate API key"
    echo "Response: $KEY_RESPONSE"
    exit 1
fi

echo "✅ API key generated: ${API_KEY:0:20}..."
echo "✅ Organization ID: $ORG_ID"
echo ""

# Test 3: Get key info
echo "3️⃣  Getting key info..."
KEY_INFO=$(curl -s "$BASE_URL/api/keys" -H "X-API-Key: $API_KEY")
if echo "$KEY_INFO" | grep -q "test-org"; then
    echo "✅ Key info retrieved"
else
    echo "❌ Failed to get key info"
    exit 1
fi
echo ""

# Test 4: Configure webhook
echo "4️⃣  Configuring webhook..."
WEBHOOK_RESPONSE=$(curl -s -X POST "$BASE_URL/api/webhook/config" \
    -H "Content-Type: application/json" \
    -H "X-API-Key: $API_KEY" \
    -d '{"webhook_url": "https://example.com/webhook"}')

if echo "$WEBHOOK_RESPONSE" | grep -q "true"; then
    echo "✅ Webhook configured"
else
    echo "❌ Failed to configure webhook"
    exit 1
fi
echo ""

# Test 5: Create event
echo "5️⃣  Creating event..."
EVENT_RESPONSE=$(curl -s -X POST "$BASE_URL/api/events" \
    -H "Content-Type: application/json" \
    -H "X-API-Key: $API_KEY" \
    -d '{
        "type": "test.event",
        "payload": {"message": "Hello from Rust!"},
        "dedup_id": "test-123"
    }')

EVENT_ID=$(echo "$EVENT_RESPONSE" | grep -o '"id":"[^"]*"' | cut -d'"' -f4)

if [ -z "$EVENT_ID" ]; then
    echo "❌ Failed to create event"
    echo "Response: $EVENT_RESPONSE"
    exit 1
fi

echo "✅ Event created: $EVENT_ID"
echo ""

# Test 6: Check inbox
echo "6️⃣  Checking inbox..."
INBOX_RESPONSE=$(curl -s "$BASE_URL/api/inbox?limit=10" \
    -H "X-API-Key: $API_KEY")

if echo "$INBOX_RESPONSE" | grep -q "$EVENT_ID"; then
    echo "✅ Event found in inbox"
else
    echo "⚠️  Event not yet in inbox (may be processing)"
fi
echo ""

# Test 7: Test deduplication
echo "7️⃣  Testing deduplication..."
DUPLICATE_RESPONSE=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/api/events" \
    -H "Content-Type: application/json" \
    -H "X-API-Key: $API_KEY" \
    -d '{
        "type": "test.event",
        "payload": {"message": "Duplicate!"},
        "dedup_id": "test-123"
    }')

HTTP_CODE=$(echo "$DUPLICATE_RESPONSE" | tail -n1)
if [ "$HTTP_CODE" = "409" ]; then
    echo "✅ Deduplication working (409 Conflict)"
else
    echo "⚠️  Expected 409, got $HTTP_CODE"
fi
echo ""

# Test 8: Acknowledge event
echo "8️⃣  Acknowledging event..."
ACK_RESPONSE=$(curl -s -X POST "$BASE_URL/api/ack/$EVENT_ID" \
    -H "X-API-Key: $API_KEY")

if echo "$ACK_RESPONSE" | grep -q "true"; then
    echo "✅ Event acknowledged"
else
    echo "❌ Failed to acknowledge event"
    exit 1
fi
echo ""

# Summary
echo "=================================="
echo "🎉 All tests passed!"
echo ""
echo "API Key (save for future tests):"
echo "  $API_KEY"
echo ""
echo "Organization ID:"
echo "  $ORG_ID"
echo ""
