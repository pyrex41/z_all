"""Authentication and API key management."""

import hashlib
import secrets
from typing import Annotated

from fastapi import Depends, HTTPException, Security, status
from fastapi.security import APIKeyHeader
from sqlmodel.ext.asyncio.session import AsyncSession

from zapier_triggers_api.database import get_session
from zapier_triggers_api.models import Organization

api_key_header = APIKeyHeader(name="X-API-Key", auto_error=False)


def generate_api_key(prefix: str = "zap_live_") -> str:
    """Generate a secure API key with prefix."""
    random_part = secrets.token_urlsafe(48)  # 64 chars after prefix
    return f"{prefix}{random_part}"


def hash_api_key(api_key: str) -> str:
    """Hash API key with SHA-256.

    SHA-256 is appropriate for API keys because:
    - API keys are high-entropy cryptographic secrets (48 random bytes)
    - No need for slow key derivation (bcrypt is for low-entropy passwords)
    - Fast verification (<1ms) enables <100ms response time target
    """
    return hashlib.sha256(api_key.encode()).hexdigest()


def verify_api_key(api_key: str, hashed: str) -> bool:
    """Verify API key against SHA-256 hash using constant-time comparison."""
    return secrets.compare_digest(hash_api_key(api_key), hashed)


async def get_current_org(
    api_key: Annotated[str | None, Security(api_key_header)],
    session: Annotated[AsyncSession, Depends(get_session)],
) -> Organization:
    """Get current organization from API key."""
    if not api_key:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Missing API key",
            headers={"WWW-Authenticate": "ApiKey"},
        )

    # Extract prefix from API key (first 12 characters)
    key_prefix = api_key[:12] if len(api_key) >= 12 else None
    if not key_prefix:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid API key format",
            headers={"WWW-Authenticate": "ApiKey"},
        )

    # Query organizations by prefix for faster lookup (indexed)
    from sqlmodel import select

    result = await session.execute(
        select(Organization).where(Organization.api_key_prefix == key_prefix)
    )
    orgs: list[Organization] = list(result.scalars().all())

    for org in orgs:
        if verify_api_key(api_key, org.api_key_hash):
            return org

    raise HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Invalid API key",
        headers={"WWW-Authenticate": "ApiKey"},
    )
