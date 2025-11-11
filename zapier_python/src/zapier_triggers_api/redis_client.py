"""Redis client for caching, rate limiting, and queuing."""

from redis.asyncio import Redis

from zapier_triggers_api.config import settings

# Global Redis client with connection pooling
redis: Redis = Redis.from_url(
    settings.redis_url,
    decode_responses=True,
    max_connections=50,  # Maximum connections in pool
    socket_connect_timeout=5,  # Connection timeout
    socket_keepalive=True,  # Keep connections alive
)


async def get_redis() -> Redis:
    """Get Redis client for dependency injection."""
    return redis
