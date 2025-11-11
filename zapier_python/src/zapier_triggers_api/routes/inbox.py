"""Inbox retrieval endpoints."""

import base64
from datetime import datetime
from typing import Annotated
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, Query, status
from pydantic import BaseModel
from sqlalchemy import func, update
from sqlmodel import select
from sqlmodel.ext.asyncio.session import AsyncSession

from zapier_triggers_api.auth_cached import get_current_org_cached as get_current_org
from zapier_triggers_api.database import get_session
from zapier_triggers_api.models import DeliveryStatus, Event, EventDelivery, Organization
from zapier_triggers_api.schemas import AckRequest, AckResponse, InboxEvent, InboxResponse

router = APIRouter(prefix="/api/inbox", tags=["inbox"])


def encode_cursor(event_id: str, created_at: datetime) -> str:
    """Encode cursor from event ID and timestamp."""
    cursor_str = f"{created_at.isoformat()}:{event_id}"
    return base64.urlsafe_b64encode(cursor_str.encode()).decode()


def decode_cursor(cursor: str) -> tuple[datetime, str]:
    """Decode cursor to timestamp and event ID."""
    cursor_str = base64.urlsafe_b64decode(cursor.encode()).decode()
    timestamp_str, event_id = cursor_str.rsplit(":", 1)
    return datetime.fromisoformat(timestamp_str), event_id


@router.get("", response_model=InboxResponse)
async def get_inbox(
    org: Annotated[Organization, Depends(get_current_org)],
    session: Annotated[AsyncSession, Depends(get_session)],
    limit: int = Query(default=100, ge=1, le=1000),
    cursor: str | None = Query(default=None),
    status_filter: DeliveryStatus | None = Query(default=None, alias="status"),
    offset: int = Query(default=0, ge=0),
) -> InboxResponse:
    """Get paginated inbox events for organization.

    Supports both cursor-based (cursor param) and offset-based (offset/limit params) pagination.
    Can filter by delivery status.
    """
    from sqlmodel import col

    query = (
        select(Event, EventDelivery)
        .join(EventDelivery, Event.id == EventDelivery.event_id)  # type: ignore[arg-type]
        .where(Event.org_id == org.id)  # type: ignore[arg-type]
        .order_by(col(Event.created_at).desc(), col(Event.id).desc())
    )

    # Apply status filter if provided
    if status_filter:
        query = query.where(EventDelivery.status == status_filter)  # type: ignore[arg-type]

    # Apply cursor-based pagination if cursor provided
    if cursor:
        cursor_time, cursor_id = decode_cursor(cursor)
        query = query.where(
            (col(Event.created_at) < cursor_time)
            | ((col(Event.created_at) == cursor_time) & (col(Event.id) < cursor_id))
        )
    else:
        # Apply offset-based pagination
        query = query.offset(offset)

    query = query.limit(limit + 1)
    results = await session.exec(query)
    rows = results.all()

    events = []
    for event, delivery in rows[:limit]:
        events.append(
            InboxEvent(
                id=event.id,
                type=event.type,
                payload=event.payload,
                created_at=event.created_at.isoformat(),
                delivery_status=delivery.status.value,
            )
        )

    next_cursor = None
    if len(rows) > limit:
        last_event = rows[limit - 1][0]
        next_cursor = encode_cursor(str(last_event.id), last_event.created_at)

    return InboxResponse(events=events, next_cursor=next_cursor)


@router.post("/ack", response_model=AckResponse)
async def acknowledge_events(
    ack_data: AckRequest,
    org: Annotated[Organization, Depends(get_current_org)],
    session: Annotated[AsyncSession, Depends(get_session)],
) -> AckResponse:
    """Acknowledge batch of events as delivered."""
    from sqlmodel import col

    # Verify events belong to org
    verify_query = select(Event.id).where(
        col(Event.id).in_(ack_data.event_ids), Event.org_id == org.id  # type: ignore[arg-type]
    )
    result = await session.exec(verify_query)
    valid_event_ids = set(result.all())

    acknowledged = 0
    if valid_event_ids:
        # Update deliveries to DELIVERED
        update_stmt = (
            update(EventDelivery)
            .where(
                col(EventDelivery.event_id).in_(valid_event_ids),
            )
            .where(EventDelivery.status != DeliveryStatus.DELIVERED)  # type: ignore[arg-type]
            .values(
                status=DeliveryStatus.DELIVERED,
                delivered_at=datetime.utcnow(),
                updated_at=datetime.utcnow(),
            )
        )
        cursor_result = await session.exec(update_stmt)  # type: ignore[arg-type]
        await session.commit()
        acknowledged = cursor_result.rowcount  # type: ignore[attr-defined]

    failed = len(ack_data.event_ids) - acknowledged
    return AckResponse(acknowledged=acknowledged, failed=failed)


class SingleAckResponse(BaseModel):
    """Response from acknowledging a single event."""

    id: UUID
    status: str
    acknowledged_at: datetime
    message: str = "Event delivery acknowledged successfully"


@router.post("/ack/{event_id}", response_model=SingleAckResponse)
async def acknowledge_single_event(
    event_id: UUID,
    org: Annotated[Organization, Depends(get_current_org)],
    session: Annotated[AsyncSession, Depends(get_session)],
) -> SingleAckResponse:
    """Acknowledge a single event delivery.

    This marks a failed/pending delivery as delivered if the webhook
    was actually successful but the system couldn't verify it.
    """
    from sqlmodel import col

    # Verify event belongs to organization
    event_query = select(Event).where(Event.id == event_id, Event.org_id == org.id)  # type: ignore[arg-type]
    result = await session.exec(event_query)
    event = result.first()

    if not event:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Event not found",
        )

    # Get the delivery record
    delivery_query = select(EventDelivery).where(EventDelivery.event_id == event_id)  # type: ignore[arg-type]
    delivery_result = await session.exec(delivery_query)
    delivery = delivery_result.first()

    if not delivery:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Event delivery record not found",
        )

    # Check if already delivered
    if delivery.status == DeliveryStatus.DELIVERED:
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail="Event already marked as delivered",
        )

    # Mark as delivered
    delivery.status = DeliveryStatus.DELIVERED
    delivery.delivered_at = datetime.utcnow()
    delivery.updated_at = datetime.utcnow()

    session.add(delivery)
    await session.commit()
    await session.refresh(delivery)

    return SingleAckResponse(
        id=event_id,
        status=delivery.status.value,
        acknowledged_at=delivery.updated_at,
    )
