"""Request/Response schemas."""

from uuid import UUID

from pydantic import BaseModel, Field


class EventCreate(BaseModel):
    """Event creation request."""

    type: str = Field(..., max_length=255, description="Event type identifier")
    payload: dict = Field(..., description="Event payload data")
    dedup_id: str | None = Field(None, max_length=255, description="Deduplication ID")


class EventResponse(BaseModel):
    """Event creation response."""

    id: UUID
    type: str
    created_at: str
    status: str = "pending"


class EventAcceptedResponse(BaseModel):
    """Async event acceptance response (202 Accepted)."""

    id: UUID
    status: str = "accepted"
    message: str = "Event queued for processing"


class InboxEvent(BaseModel):
    """Inbox event details."""

    id: UUID
    type: str
    payload: dict
    created_at: str
    delivery_status: str


class InboxResponse(BaseModel):
    """Inbox listing response with pagination."""

    events: list[InboxEvent]
    next_cursor: str | None = None


class AckRequest(BaseModel):
    """Batch acknowledgment request."""

    event_ids: list[UUID] = Field(..., min_length=1, max_length=100)


class AckResponse(BaseModel):
    """Acknowledgment response."""

    acknowledged: int
    failed: int
