"""Event ingestion endpoints."""

import json
import logging
from datetime import datetime
from typing import Annotated
from uuid import uuid4

from fastapi import APIRouter, Depends, HTTPException, status
from redis.asyncio import Redis
from sqlmodel.ext.asyncio.session import AsyncSession

from zapier_triggers_api.auth_cached import get_current_org_cached as get_current_org
from zapier_triggers_api.config import settings
from zapier_triggers_api.database import get_session
from zapier_triggers_api.models import DeliveryStatus, Event, EventDelivery, Organization
from zapier_triggers_api.rate_limit import check_rate_limit
from zapier_triggers_api.redis_client import get_redis
from zapier_triggers_api.schemas import EventAcceptedResponse, EventCreate, EventResponse

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/events", tags=["events"])


async def queue_event_for_processing(
    event_id: str,
    org_id: str,
    event_type: str,
    payload: dict,
    dedup_id: str | None,
    redis: Redis,
) -> None:
    """Queue event to Redis Streams for async processing (persistence + delivery).

    Uses MAXLEN to prevent unbounded stream growth.
    """
    stream_data: dict[str, str | int | float] = {
        "event_id": str(event_id),
        "org_id": str(org_id),
        "type": event_type,
        "payload": json.dumps(payload),
        "dedup_id": dedup_id or "",
        "timestamp": datetime.utcnow().isoformat(),
    }
    # Use MAXLEN ~ (approximate) for better performance
    await redis.xadd(
        settings.redis_stream_name,
        stream_data,  # type: ignore[arg-type]
        maxlen=settings.redis_stream_max_length,
        approximate=True,  # ~MAXLEN for better performance
    )


@router.post(
    "",
    response_model=EventAcceptedResponse,
    status_code=status.HTTP_202_ACCEPTED,
)
async def create_event(
    event_data: EventCreate,
    org: Annotated[Organization, Depends(get_current_org)],
    redis: Annotated[Redis, Depends(get_redis)],
) -> EventAcceptedResponse:
    """Ingest a new event (async processing - < 10ms response time).

    This endpoint accepts events and queues them for background processing.
    The event will be persisted, deduplicated, and delivered asynchronously.

    Backpressure Protection:
    - Returns 503 if queue is at 95% capacity
    - Prevents event loss by rejecting instead of dropping
    - Client should retry after 5 seconds

    Returns:
    - 202 Accepted with event ID (normal operation)
    - 503 Service Unavailable (queue near capacity, retry required)
    """
    # Fast validation only - no blocking operations

    # 1. Check rate limit (Redis check - ~1ms)
    await check_rate_limit(org, redis)

    # 2. Validate payload size (in-memory check - <1ms)
    payload_size = len(json.dumps(event_data.payload))
    if payload_size > 256 * 1024:  # 256KB
        raise HTTPException(
            status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE,
            detail="Payload exceeds 256KB limit",
        )

    # 3. Check queue depth to prevent overload (backpressure protection)
    stream_len = await redis.xlen(settings.redis_stream_name)
    queue_capacity_threshold = int(settings.redis_stream_max_length * 0.95)

    if stream_len >= queue_capacity_threshold:
        logger.warning(
            f"Event queue near capacity: {stream_len}/{settings.redis_stream_max_length} "
            f"(org: {org.id})"
        )
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail={
                "error": "event_processing_backlog",
                "message": "System under high load, please retry",
                "queue_depth": stream_len,
                "queue_capacity": settings.redis_stream_max_length,
                "retry_after_seconds": 5,
            },
            headers={"Retry-After": "5"},
        )

    # 4. Check for deduplication if dedup_id provided
    if event_data.dedup_id:
        # Check Redis cache first for fast dedup check
        dedup_key = f"dedup:{event_data.dedup_id}"
        if await redis.exists(dedup_key):
            raise HTTPException(
                status_code=status.HTTP_409_CONFLICT,
                detail="Event with this dedup_id already exists",
            )
        # Set dedup key with 24 hour expiry (events older than this can be duplicated)
        await redis.setex(dedup_key, 86400, "1")

    # 5. Generate event ID immediately
    event_id = uuid4()

    # 6. Queue to Redis Streams for async processing (~1-2ms)
    # Worker will handle: persistence, delivery
    await queue_event_for_processing(
        event_id=str(event_id),
        org_id=str(org.id),
        event_type=event_data.type,
        payload=event_data.payload,
        dedup_id=event_data.dedup_id,
        redis=redis,
    )

    # 7. Return immediately (total time: ~5-10ms)
    return EventAcceptedResponse(
        id=event_id,
        status="accepted",
        message="Event queued for processing",
    )
