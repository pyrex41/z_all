"""Rate limiting with Redis."""

from typing import Annotated

from fastapi import Depends, HTTPException, status
from redis.asyncio import Redis

from zapier_triggers_api.config import settings
from zapier_triggers_api.models import Organization, PlanTier
from zapier_triggers_api.redis_client import get_redis


def get_rate_limit_for_plan(plan: PlanTier) -> int:
    """Get rate limit (requests per minute) for plan tier."""
    limits = {
        PlanTier.FREE: settings.rate_limit_free,
        PlanTier.PRO: settings.rate_limit_pro,
        PlanTier.BUSINESS: settings.rate_limit_business,
        PlanTier.ENTERPRISE: settings.rate_limit_enterprise,
    }
    return limits.get(plan, settings.rate_limit_free)


async def check_rate_limit(
    org: Organization,
    redis: Annotated[Redis, Depends(get_redis)],
) -> None:
    """Check if organization is within rate limit."""
    rate_limit = get_rate_limit_for_plan(org.plan)
    key = f"rate_limit:{org.id}"

    # Increment counter
    current = await redis.incr(key)

    # Set expiry on first request in window
    if current == 1:
        await redis.expire(key, 60)  # 1 minute window

    # Check limit
    if current > rate_limit:
        ttl = await redis.ttl(key)
        raise HTTPException(
            status_code=status.HTTP_429_TOO_MANY_REQUESTS,
            detail=f"Rate limit exceeded. Try again in {ttl} seconds.",
            headers={"Retry-After": str(ttl)},
        )
