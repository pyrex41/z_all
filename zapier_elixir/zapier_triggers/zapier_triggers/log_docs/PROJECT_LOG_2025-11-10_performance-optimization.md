# Performance Optimization - Authentication Caching & Fast Hashing

**Date:** November 10, 2025  
**Session Focus:** Critical performance optimization to meet PRD requirements (<100ms response time)

## Summary

Identified and fixed severe performance bottleneck in authentication system. Original implementation used Bcrypt for API key verification on every request, causing 296ms latency (3x over PRD target). Implemented fast SHA256 hashing with authentication caching, achieving 60ms average latency (40% under target) - a 5x performance improvement.

## Changes Made

### 1. Fast API Key Hashing (lib/zapier_triggers/organizations/organization.ex:71-82)

**Replaced Bcrypt with SHA256:**
```elixir
def hash_api_key_fast(api_key) when is_binary(api_key) do
  :crypto.hash(:sha256, api_key)
  |> Base.encode16(case: :lower)
end

def verify_api_key(%__MODULE__{} = organization, api_key) do
  computed_hash = hash_api_key_fast(api_key)
  Plug.Crypto.secure_compare(computed_hash, organization.api_key_hash)
end
```

**Rationale:**
- API keys are 256-bit system-generated random strings (not user passwords)
- No vulnerability to dictionary/brute-force attacks (2^256 combinations)
- SHA256 provides sufficient security with ~1000x better performance
- Constant-time comparison prevents timing attacks

### 2. Authentication Caching (lib/zapier_triggers_web/plugs/authenticate.ex:28-67)

**Implemented 5-minute cache for auth lookups:**
```elixir
@cache_ttl :timer.minutes(5)

defp authenticate_api_key(conn, api_key) do
  api_key_hash = Organization.hash_api_key_fast(api_key)
  cache_key = "auth:#{api_key_hash}"

  case Cachex.get(:auth_cache, cache_key) do
    {:ok, org_id} when not is_nil(org_id) ->
      # Cache hit: Load by primary key (fast)
      case Repo.get(Organization, org_id) do
        %Organization{} = org -> authorize(conn, org)
        nil -> 
          Cachex.del(:auth_cache, cache_key)
          unauthorized(conn)
      end
    _ ->
      # Cache miss: Lookup and cache
      case Repo.get_by(Organization, api_key_hash: api_key_hash) do
        %Organization{} = org ->
          Cachex.put(:auth_cache, cache_key, org.id, ttl: @cache_ttl)
          authorize(conn, org)
        nil -> unauthorized(conn)
      end
  end
end
```

**Benefits:**
- Cache hit: ~10ms (primary key lookup only)
- Cache miss: ~70ms (index lookup + cache set)
- 5-minute TTL balances security and performance
- Automatic cache invalidation on org deletion

### 3. Dual Cachex Instances (lib/zapier_triggers/application.ex:18-20)

**Fixed supervisor child spec conflict:**
```elixir
Supervisor.child_spec({Cachex, name: :dedup_cache}, id: :dedup_cache),
Supervisor.child_spec({Cachex, name: :auth_cache}, id: :auth_cache),
```

## Performance Results

### Benchmarks (Sequential, Single-threaded)

| Stage | Latency | Throughput | Status |
|-------|---------|------------|--------|
| Original (Bcrypt) | 296ms | 3 req/s | ❌ FAIL (3x over) |
| After SHA256 | 84ms | 13 req/s | ✅ PASS (16% under) |
| After Caching | 60ms | 15 req/s | ✅ PASS (40% under) |

### Endpoint Performance (Final)

| Endpoint | Avg Latency | Target | Status |
|----------|-------------|--------|--------|
| GET /api/keys | 66ms | <100ms | ✅ PASS |
| POST /api/events | 75ms | <100ms | ✅ PASS |
| GET /api/inbox | 75ms | <100ms | ✅ PASS |

**Overall Improvement:** 5x faster than original implementation

## Task-Master Status

No active tasks in task-master. Work was driven by PRD performance requirements discovery.

## Todo List Status

All performance optimization todos completed:
- ✅ Investigate slow API performance
- ✅ Replace bcrypt with SHA256 for API keys
- ✅ Update authentication plug
- ✅ Test performance improvements
- ✅ Re-run benchmarks
- ✅ Add authentication cache with Cachex
- ✅ Verify <100ms target met

## Security Analysis

### Why SHA256 is Secure for API Keys

✅ **Appropriate because:**
- API keys are 43+ character random strings (256 bits entropy)
- System-generated, not user-chosen (no dictionary attacks)
- Mathematically impossible to brute force (2^256 attempts)
- Constant-time comparison prevents timing attacks
- Always stored as hashes, never plaintext

❌ **Bcrypt was inappropriate because:**
- Designed for slow password hashing to prevent cracking
- Adds 100-300ms overhead per verification
- Security benefit only applies to weak user passwords
- API keys already have maximum entropy

### Cache Security

- 5-minute TTL limits exposure window
- Cache invalidation on organization deletion
- Cache keys use SHA256 hash (not raw API key)
- Still requires database verification on cache miss

## Next Steps

1. **Concurrent Load Testing:** Current benchmarks are sequential; test with 10-50 concurrent connections
2. **Database Query Optimization:** Profile and optimize event ingestion queries
3. **Connection Pooling Review:** Verify Ecto pool size (currently 10) is optimal
4. **Rate Limiting Performance:** Ensure rate limit checks don't add significant overhead
5. **Production Monitoring:** Add telemetry for cache hit rates and latency tracking

## Code References

- `lib/zapier_triggers/organizations/organization.ex:71-82` - Fast hashing implementation
- `lib/zapier_triggers_web/plugs/authenticate.ex:28-67` - Auth caching logic
- `lib/zapier_triggers/application.ex:18-20` - Dual Cachex setup

## Lessons Learned

1. **Always profile before optimizing** - Initial assumption was database queries, actual bottleneck was Bcrypt
2. **Tool misuse hurts performance** - Bcrypt is perfect for passwords, terrible for API tokens
3. **Context matters for crypto** - Security decisions must account for threat model and data characteristics
4. **Sequential benchmarks underestimate capacity** - Need concurrent testing for realistic throughput
5. **Caching is powerful but has tradeoffs** - 5-minute TTL chosen after considering security/performance balance
