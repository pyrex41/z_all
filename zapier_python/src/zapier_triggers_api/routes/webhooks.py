"""Webhook configuration routes."""

from datetime import datetime
from typing import Annotated
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, status
from pydantic import BaseModel, Field, HttpUrl
from sqlmodel.ext.asyncio.session import AsyncSession

from ..auth import get_current_org
from ..database import get_session
from ..models import Organization

router = APIRouter(prefix="/api/webhook", tags=["webhooks"])


class ConfigureWebhookRequest(BaseModel):
    """Request to configure webhook URL."""

    webhook_url: HttpUrl


class ConfigureWebhookResponse(BaseModel):
    """Response from webhook configuration."""

    organization_id: UUID
    webhook_url: str
    updated_at: datetime
    message: str = "Webhook URL configured successfully"


@router.post(
    "/config",
    response_model=ConfigureWebhookResponse,
    summary="Configure Webhook URL",
    description="Sets or updates the webhook URL where events will be delivered for this organization.",
)
async def configure_webhook(
    request: ConfigureWebhookRequest,
    org: Annotated[Organization, Depends(get_current_org)],
    session: Annotated[AsyncSession, Depends(get_session)],
) -> ConfigureWebhookResponse:
    """Configure the webhook URL for the authenticated organization."""
    # Update organization webhook URL
    org.webhook_url = str(request.webhook_url)
    org.updated_at = datetime.utcnow()

    session.add(org)
    await session.commit()
    await session.refresh(org)

    return ConfigureWebhookResponse(
        organization_id=org.id,
        webhook_url=org.webhook_url,
        updated_at=org.updated_at,
    )
