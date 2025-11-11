-- Create event_deliveries table
CREATE TABLE IF NOT EXISTS event_deliveries (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id UUID NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  status VARCHAR(50) NOT NULL DEFAULT 'pending', -- pending, delivered, failed
  attempts INTEGER NOT NULL DEFAULT 0,
  last_attempt_at TIMESTAMPTZ,
  response_status INTEGER,
  error_message TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Indexes for delivery worker queries
CREATE INDEX idx_deliveries_status ON event_deliveries(status, created_at) WHERE status = 'pending';
CREATE INDEX idx_deliveries_event ON event_deliveries(event_id);
