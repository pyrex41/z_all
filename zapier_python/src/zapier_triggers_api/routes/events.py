"""Event ingestion endpoints."""

import json
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

router = APIRouter(prefix="/api/events", tags=["events"])


async def queue_event_for_processing(
    event_id: str,
    org_id: str,
    event_type: str,
    payload: dict,
    dedup_id: str | None,
    redis: Redis,
) -> None:
    """Queue event to Redis Streams for async processing (persistence + delivery)."""
    stream_data: dict[str, str | int | float] = {
        "event_id": str(event_id),
        "org_id": str(org_id),
        "type": event_type,
        "payload": json.dumps(payload),
        "dedup_id": dedup_id or "",
        "timestamp": datetime.utcnow().isoformat(),
    }
    await redis.xadd(settings.redis_stream_name, stream_data)  # type: ignore[arg-type]


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

    Returns 202 Accepted with event ID immediately.
    """
    # Fast validation only - no blocking operations

    # 1. Check rate limit (Redis check - ~1ms)
    await check_rate_limit(org, redis)

    # 2. Validate payload size (in-memory check - <1ms)
    payload_size = len(json.dumps(event_data.data))
    if payload_size > 256 * 1024:  # 256KB
        raise HTTPException(
            status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE,
            detail="Payload exceeds 256KB limit",
        )

    # 3. Generate event ID immediately
    event_id = uuid4()

    # 4. Queue to Redis Streams for async processing (~1-2ms)
    # Worker will handle: deduplication, persistence, delivery
    await queue_event_for_processing(
        event_id=str(event_id),
        org_id=str(org.id),
        event_type=event_data.type,
        payload=event_data.data,
        dedup_id=event_data.dedup_id,
        redis=redis,
    )

    # 5. Return immediately (total time: ~5-10ms)
    return EventAcceptedResponse(
        id=event_id,
        status="accepted",
        message="Event queued for processing",
    )
