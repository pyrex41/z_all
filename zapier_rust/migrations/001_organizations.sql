-- Create organizations table
CREATE TABLE IF NOT EXISTS organizations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  api_key_hash VARCHAR(255) NOT NULL UNIQUE,
  webhook_url VARCHAR(2048),
  tier VARCHAR(50) NOT NULL DEFAULT 'free',
  rate_limit_per_minute INTEGER NOT NULL DEFAULT 100,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index for API key lookups (hot path)
CREATE INDEX idx_organizations_api_key ON organizations(api_key_hash);
