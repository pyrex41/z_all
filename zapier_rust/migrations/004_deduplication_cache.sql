-- Create deduplication_cache table
CREATE TABLE IF NOT EXISTS deduplication_cache (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
  dedup_id VARCHAR(255) NOT NULL,
  event_id UUID NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  expires_at TIMESTAMPTZ NOT NULL,
  UNIQUE(organization_id, dedup_id)
);

-- Index for cleanup worker
CREATE INDEX idx_dedup_expires ON deduplication_cache(expires_at);
