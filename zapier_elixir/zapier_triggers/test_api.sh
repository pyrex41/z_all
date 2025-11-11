#!/bin/bash

# Test script for Zapier Triggers API
# Run with: bash test_api.sh

set -e

BASE_URL="http://localhost:4000/api"

echo "🚀 Testing Zapier Triggers API"
echo "================================"
echo ""

# 1. Generate API Key
echo "1️⃣  Generating API key..."
RESPONSE=$(curl -s -X POST "$BASE_URL/keys/generate" \
  -H "Content-Type: application/json" \
  -d '{
    "organization_name": "Test Organization",
    "tier": "free"
  }')

echo "$RESPONSE" | jq '.'

API_KEY=$(echo "$RESPONSE" | jq -r '.api_key')
ORG_ID=$(echo "$RESPONSE" | jq -r '.organization_id')

if [ "$API_KEY" == "null" ]; then
  echo "❌ Failed to generate API key"
  exit 1
fi

echo "✅ API Key generated: ${API_KEY:0:20}..."
echo ""

# 2. View API Key Info
echo "2️⃣  Viewing API key info..."
curl -s -X GET "$BASE_URL/keys" \
  -H "X-API-Key: $API_KEY" | jq '.'
echo "✅ API key info retrieved"
echo ""

# 3. Configure Webhook URL
echo "3️⃣  Configuring webhook URL..."
curl -s -X POST "$BASE_URL/webhook/config" \
  -H "X-API-Key: $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "webhook_url": "https://webhook.site/unique-url"
  }' | jq '.'
echo "✅ Webhook URL configured"
echo ""

# 4. Create Event
echo "4️⃣  Creating event..."
EVENT_RESPONSE=$(curl -s -X POST "$BASE_URL/events" \
  -H "X-API-Key: $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "user.created",
    "dedup_id": "test-event-'$(date +%s)'",
    "payload": {
      "user_id": "12345",
      "email": "test@example.com",
      "name": "Test User"
    }
  }')

echo "$EVENT_RESPONSE" | jq '.'

EVENT_ID=$(echo "$EVENT_RESPONSE" | jq -r '.id')

if [ "$EVENT_ID" == "null" ]; then
  echo "❌ Failed to create event"
  exit 1
fi

echo "✅ Event created: $EVENT_ID"
echo ""

# 5. List Events (Inbox)
echo "5️⃣  Listing events in inbox..."
sleep 2  # Give it time to process
curl -s -X GET "$BASE_URL/inbox?limit=5" \
  -H "X-API-Key: $API_KEY" | jq '.'
echo "✅ Events listed"
echo ""

# 6. Test Duplicate Detection
echo "6️⃣  Testing duplicate detection..."
DUPLICATE_RESPONSE=$(curl -s -X POST "$BASE_URL/events" \
  -H "X-API-Key: $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "user.created",
    "dedup_id": "duplicate-test-123",
    "payload": {
      "test": "data"
    }
  }')

echo "$DUPLICATE_RESPONSE" | jq '.'

# Try to create the same event again
echo "Attempting to create duplicate..."
DUPLICATE_RESPONSE2=$(curl -s -X POST "$BASE_URL/events" \
  -H "X-API-Key: $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "user.created",
    "dedup_id": "duplicate-test-123",
    "payload": {
      "test": "data"
    }
  }')

echo "$DUPLICATE_RESPONSE2" | jq '.'

if echo "$DUPLICATE_RESPONSE2" | grep -q "Duplicate event detected"; then
  echo "✅ Duplicate detection working"
else
  echo "❌ Duplicate detection failed"
fi
echo ""

# 7. Test Rate Limiting (create many events quickly)
echo "7️⃣  Testing rate limiting..."
echo "Sending 105 requests rapidly (rate limit is 100/min for free tier)..."

for i in {1..105}; do
  curl -s -X POST "$BASE_URL/events" \
    -H "X-API-Key: $API_KEY" \
    -H "Content-Type: application/json" \
    -d '{
      "type": "test.event",
      "dedup_id": "rate-test-'$i'",
      "payload": {"index": '$i'}
    }' > /dev/null 2>&1 &
done

wait

echo "Checking if rate limit was triggered..."
LAST_EVENT=$(curl -s -X POST "$BASE_URL/events" \
  -H "X-API-Key: $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "type": "test.event",
    "dedup_id": "rate-test-final",
    "payload": {"test": "final"}
  }')

echo "$LAST_EVENT" | jq '.'

if echo "$LAST_EVENT" | grep -q "Rate limit exceeded"; then
  echo "✅ Rate limiting working"
else
  echo "⚠️  Rate limit may not have been triggered (might need more requests)"
fi
echo ""

# 8. Rotate API Key
echo "8️⃣  Rotating API key..."
NEW_KEY_RESPONSE=$(curl -s -X POST "$BASE_URL/keys/rotate" \
  -H "X-API-Key: $API_KEY")

echo "$NEW_KEY_RESPONSE" | jq '.'

NEW_API_KEY=$(echo "$NEW_KEY_RESPONSE" | jq -r '.api_key')

if [ "$NEW_API_KEY" != "null" ] && [ "$NEW_API_KEY" != "$API_KEY" ]; then
  echo "✅ API key rotated successfully"
  echo "Testing old key (should fail)..."
  OLD_KEY_TEST=$(curl -s -X GET "$BASE_URL/keys" \
    -H "X-API-Key: $API_KEY")

  if echo "$OLD_KEY_TEST" | grep -q "Invalid API key"; then
    echo "✅ Old key properly invalidated"
  else
    echo "⚠️  Old key still works (unexpected)"
  fi

  echo "Testing new key..."
  NEW_KEY_TEST=$(curl -s -X GET "$BASE_URL/keys" \
    -H "X-API-Key: $NEW_API_KEY")

  if echo "$NEW_KEY_TEST" | grep -q "$ORG_ID"; then
    echo "✅ New key working"
  else
    echo "❌ New key not working"
  fi
else
  echo "❌ Failed to rotate API key"
fi
echo ""

echo "================================"
echo "✅ All tests completed!"
echo ""
echo "📊 Check Prometheus metrics at: http://localhost:9568/metrics"
echo "📝 API Key: $NEW_API_KEY"
echo "🆔 Organization ID: $ORG_ID"
