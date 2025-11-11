"""Database models using SQLModel."""

from datetime import datetime
from enum import Enum
from uuid import UUID, uuid4

from sqlalchemy import JSON, Column, Index, func
from sqlmodel import Field, SQLModel


class PlanTier(str, Enum):
    """Organization plan tiers."""

    FREE = "free"
    PRO = "pro"
    BUSINESS = "business"
    ENTERPRISE = "enterprise"


class DeliveryStatus(str, Enum):
    """Event delivery statuses."""

    PENDING = "pending"
    DELIVERED = "delivered"
    FAILED = "failed"
    RETRYING = "retrying"


class Organization(SQLModel, table=True):
    """Organization with API access."""

    __tablename__ = "organizations"

    id: UUID = Field(default_factory=uuid4, primary_key=True)
    name: str = Field(max_length=255, index=True)
    api_key_hash: str = Field(max_length=255)
    api_key_prefix: str | None = Field(default=None, max_length=12, index=True)
    webhook_url: str = Field(max_length=2048)
    rate_limit: int = Field(default=100)
    plan: PlanTier = Field(default=PlanTier.FREE)
    created_at: datetime = Field(default_factory=lambda: datetime.utcnow())
    updated_at: datetime = Field(default_factory=lambda: datetime.utcnow())


class Event(SQLModel, table=True):
    """Event ingested via POST /events."""

    __tablename__ = "events"
    __table_args__ = (
        Index("ix_events_org_created", "org_id", "created_at"),
        Index("ix_events_dedup", "dedup_id", unique=True),
    )

    id: UUID = Field(default_factory=uuid4, primary_key=True)
    org_id: UUID = Field(foreign_key="organizations.id", index=True)
    type: str = Field(max_length=255, index=True)
    dedup_id: str | None = Field(default=None, max_length=255)
    payload: dict = Field(sa_column=Column(JSON))
    created_at: datetime = Field(
        default_factory=lambda: datetime.utcnow(),
        sa_column_kwargs={"server_default": func.now()},
    )


class EventDelivery(SQLModel, table=True):
    """Event delivery tracking."""

    __tablename__ = "event_deliveries"

    id: UUID = Field(default_factory=uuid4, primary_key=True)
    event_id: UUID = Field(foreign_key="events.id", index=True)
    status: DeliveryStatus = Field(default=DeliveryStatus.PENDING, index=True)
    attempts: int = Field(default=0)
    error_message: str | None = Field(default=None, max_length=2048)
    last_attempt_at: datetime | None = Field(default=None)
    delivered_at: datetime | None = Field(default=None)
    created_at: datetime = Field(default_factory=lambda: datetime.utcnow())
    updated_at: datetime = Field(default_factory=lambda: datetime.utcnow())


class AuditLog(SQLModel, table=True):
    """Audit log for compliance."""

    __tablename__ = "audit_log"

    id: UUID = Field(default_factory=uuid4, primary_key=True)
    org_id: UUID = Field(foreign_key="organizations.id", index=True)
    action: str = Field(max_length=255, index=True)
    details: dict = Field(sa_column=Column(JSON))
    created_at: datetime = Field(
        default_factory=lambda: datetime.utcnow(),
        sa_column_kwargs={"server_default": func.now()},
    )
