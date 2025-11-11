-- Create events table
CREATE TABLE IF NOT EXISTS events (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
  event_type VARCHAR(255) NOT NULL,
  dedup_id VARCHAR(255),
  payload JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE(organization_id, dedup_id)
);

-- Indexes for performance
CREATE INDEX idx_events_org_created ON events(organization_id, created_at DESC);
CREATE INDEX idx_events_dedup ON events(organization_id, dedup_id) WHERE dedup_id IS NOT NULL;
