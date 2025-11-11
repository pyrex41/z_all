"""API key management routes."""

import hashlib
import secrets
from datetime import datetime
from typing import Annotated
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, status
from pydantic import BaseModel, Field
from sqlmodel import select
from sqlmodel.ext.asyncio.session import AsyncSession

from ..auth import get_current_org
from ..database import get_session
from ..models import Organization, PlanTier

router = APIRouter(prefix="/api/keys", tags=["api-keys"])


class GenerateKeyRequest(BaseModel):
    """Request to generate a new API key."""

    organization_name: str = Field(..., min_length=1, max_length=255)
    tier: PlanTier = Field(default=PlanTier.FREE)


class GenerateKeyResponse(BaseModel):
    """Response from API key generation."""

    organization_id: UUID
    organization_name: str
    api_key: str
    tier: PlanTier
    rate_limit_per_minute: int
    created_at: datetime
    warning: str = "Store this API key securely. It will not be shown again."


class RotateKeyResponse(BaseModel):
    """Response from API key rotation."""

    organization_id: UUID
    organization_name: str
    api_key: str
    rotated_at: datetime
    warning: str = "Your old API key has been invalidated. Update your applications with this new key."


class KeyInfoResponse(BaseModel):
    """Organization API key metadata."""

    organization_id: UUID
    organization_name: str
    tier: PlanTier
    rate_limit_per_minute: int
    webhook_url: str
    created_at: datetime
    updated_at: datetime
    note: str = "API key is not displayed for security reasons. Use /api/keys/rotate to generate a new one."


def generate_api_key() -> tuple[str, str, str]:
    """Generate a secure API key.

    Returns:
        tuple: (prefix, full_api_key, hash)
    """
    # Generate secure random bytes
    random_bytes = secrets.token_bytes(32)
    encoded = secrets.token_urlsafe(32)

    # Add prefix based on environment
    # TODO: Make this configurable via environment variable
    prefix = "zap_test_"
    api_key = f"{prefix}{encoded}"

    # Hash for storage
    api_key_hash = hashlib.sha256(api_key.encode()).hexdigest()

    return prefix, api_key, api_key_hash


def rate_limit_for_tier(tier: PlanTier) -> int:
    """Get rate limit based on tier."""
    limits = {
        PlanTier.FREE: 100,
        PlanTier.PRO: 1_000,
        PlanTier.BUSINESS: 10_000,
        PlanTier.ENTERPRISE: 100_000,
    }
    return limits.get(tier, 100)


@router.post(
    "/generate",
    response_model=GenerateKeyResponse,
    status_code=status.HTTP_201_CREATED,
    summary="Generate API Key",
    description="Creates a new organization and generates an API key. No authentication required - this is the bootstrap endpoint.",
)
async def generate_key(
    request: GenerateKeyRequest,
    session: Annotated[AsyncSession, Depends(get_session)],
) -> GenerateKeyResponse:
    """Generate a new API key for an organization."""
    prefix, api_key, api_key_hash = generate_api_key()

    # Create organization with default webhook URL
    organization = Organization(
        name=request.organization_name,
        api_key_hash=api_key_hash,
        api_key_prefix=api_key[:12],  # Store first 12 chars for fast lookup
        webhook_url="",  # Must be configured later
        rate_limit=rate_limit_for_tier(request.tier),
        plan=request.tier,
        created_at=datetime.utcnow(),
        updated_at=datetime.utcnow(),
    )

    session.add(organization)
    await session.commit()
    await session.refresh(organization)

    return GenerateKeyResponse(
        organization_id=organization.id,
        organization_name=organization.name,
        api_key=api_key,
        tier=organization.plan,
        rate_limit_per_minute=organization.rate_limit,
        created_at=organization.created_at,
    )


@router.post(
    "/rotate",
    response_model=RotateKeyResponse,
    summary="Rotate API Key",
    description="Rotates the API key for the authenticated organization. The old key becomes invalid immediately.",
)
async def rotate_key(
    org: Annotated[Organization, Depends(get_current_org)],
    session: Annotated[AsyncSession, Depends(get_session)],
) -> RotateKeyResponse:
    """Rotate the API key for the authenticated organization."""
    prefix, new_api_key, new_api_key_hash = generate_api_key()

    # Update organization with new key
    org.api_key_hash = new_api_key_hash
    org.api_key_prefix = prefix.rstrip("_")
    org.updated_at = datetime.utcnow()

    session.add(org)
    await session.commit()
    await session.refresh(org)

    return RotateKeyResponse(
        organization_id=org.id,
        organization_name=org.name,
        api_key=new_api_key,
        rotated_at=org.updated_at,
    )


@router.get(
    "",
    response_model=KeyInfoResponse,
    summary="Get API Key Info",
    description="Returns metadata about the authenticated organization's API key (but not the key itself).",
)
async def get_key_info(
    org: Annotated[Organization, Depends(get_current_org)],
) -> KeyInfoResponse:
    """Get information about the current API key."""
    return KeyInfoResponse(
        organization_id=org.id,
        organization_name=org.name,
        tier=org.plan,
        rate_limit_per_minute=org.rate_limit,
        webhook_url=org.webhook_url,
        created_at=org.created_at,
        updated_at=org.updated_at,
    )
