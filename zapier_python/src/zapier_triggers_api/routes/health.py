"""Health check and monitoring endpoints."""

from typing import Annotated

from fastapi import APIRouter, Depends
from redis.asyncio import Redis

from zapier_triggers_api.config import settings
from zapier_triggers_api.redis_client import get_redis

router = APIRouter(prefix="/api", tags=["health"])


@router.get("/health")
async def health_check() -> dict[str, str]:
    """Basic health check endpoint."""
    return {
        "status": "healthy",
        "service": "Zapier Triggers API",
    }


@router.get("/health/detailed")
async def detailed_health_check(
    redis: Annotated[Redis, Depends(get_redis)],
) -> dict:
    """Detailed health check with queue monitoring.

    Returns queue depths and processing statistics for monitoring.
    """
    try:
        # Get Redis connection status
        await redis.ping()
        redis_status = "healthy"
    except Exception as e:
        redis_status = f"unhealthy: {e}"

    # Get queue depths
    try:
        event_queue_length = await redis.xlen(settings.redis_stream_name)
        webhook_queue_length = await redis.xlen("zapier:webhook-deliveries")

        # Get pending counts for consumer groups
        event_pending = 0
        webhook_pending = 0

        try:
            # Get pending messages in event processor group
            event_pending_info = await redis.xpending(
                settings.redis_stream_name, "event-processors"
            )
            if event_pending_info:
                event_pending = event_pending_info.get("pending", 0)
        except Exception:
            pass  # Consumer group may not exist yet

        try:
            # Get pending messages in webhook delivery group
            webhook_pending_info = await redis.xpending(
                "zapier:webhook-deliveries", "webhook-delivery-processors"
            )
            if webhook_pending_info:
                webhook_pending = webhook_pending_info.get("pending", 0)
        except Exception:
            pass  # Consumer group may not exist yet

        queue_info = {
            "event_queue_length": event_queue_length,
            "webhook_queue_length": webhook_queue_length,
            "event_pending_processing": event_pending,
            "webhook_pending_processing": webhook_pending,
        }
    except Exception as e:
        queue_info = {"error": f"Failed to get queue info: {e}"}

    return {
        "status": "healthy" if redis_status == "healthy" else "degraded",
        "redis": redis_status,
        "queues": queue_info,
        "config": {
            "max_concurrent_events": int(settings.redis_stream_max_length / 100),
            "redis_stream_max_length": settings.redis_stream_max_length,
        },
    }
