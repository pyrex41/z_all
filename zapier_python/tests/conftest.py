"""Pytest configuration and shared fixtures for FastAPI tests."""

import asyncio
import os
from collections.abc import AsyncGenerator
from typing import Any

import httpx
import pytest
import pytest_asyncio
from fastapi.testclient import TestClient
from redis.asyncio import Redis
from sqlalchemy.ext.asyncio import AsyncEngine, create_async_engine
from sqlmodel import SQLModel
from sqlmodel.ext.asyncio.session import AsyncSession

from zapier_triggers_api.auth import generate_api_key, hash_api_key
from zapier_triggers_api.config import settings
from zapier_triggers_api.database import get_session
from zapier_triggers_api.main import app
from zapier_triggers_api.models import Organization, PlanTier
from zapier_triggers_api.redis_client import get_redis


# Test database URL - use in-memory SQLite for unit tests
TEST_DATABASE_URL = os.getenv(
    "TEST_DATABASE_URL",
    "sqlite+aiosqlite:///:memory:"
)

# Test Redis URL
TEST_REDIS_URL = os.getenv(
    "TEST_REDIS_URL",
    settings.redis_url  # Fall back to default Redis
)


@pytest.fixture(scope="session")
def event_loop():
    """Create event loop for async tests."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


@pytest_asyncio.fixture(scope="function")
async def test_engine() -> AsyncGenerator[AsyncEngine, None]:
    """Create test database engine."""
    engine = create_async_engine(
        TEST_DATABASE_URL,
        echo=False,
        future=True,
    )

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(SQLModel.metadata.create_all)

    yield engine

    # Drop tables
    async with engine.begin() as conn:
        await conn.run_sync(SQLModel.metadata.drop_all)

    await engine.dispose()


@pytest_asyncio.fixture(scope="function")
async def test_session(test_engine: AsyncEngine) -> AsyncGenerator[AsyncSession, None]:
    """Create test database session."""
    async with AsyncSession(test_engine) as session:
        yield session


@pytest_asyncio.fixture(scope="function")
async def test_redis() -> AsyncGenerator[Redis, None]:
    """Create test Redis connection with in-memory fake."""
    from fakeredis import aioredis as fakeredis

    redis = fakeredis.FakeRedis(decode_responses=True)

    yield redis

    await redis.aclose()


@pytest_asyncio.fixture(scope="function")
async def test_org_data(test_session: AsyncSession) -> tuple[Organization, str]:
    """Create test organization with API key and return both org and key."""
    # Generate API key
    api_key = generate_api_key("zap_test_")
    api_key_hash = hash_api_key(api_key)
    api_key_prefix = api_key[:12]

    # Create organization
    org = Organization(
        name="Test Organization",
        api_key_hash=api_key_hash,
        api_key_prefix=api_key_prefix,
        webhook_url="http://localhost:8888/webhook",
        rate_limit=100,
        plan=PlanTier.PRO,
    )

    test_session.add(org)
    await test_session.commit()
    await test_session.refresh(org)

    # Return tuple of (org, api_key)
    return org, api_key


@pytest.fixture(scope="function")
def test_client(test_session: AsyncSession, test_redis: Redis) -> TestClient:
    """Create FastAPI test client with dependency overrides (synchronous)."""
    from fastapi import FastAPI
    from zapier_triggers_api.routes import api_keys, events, health, inbox, webhooks

    # Create a test app without the problematic middleware
    test_app = FastAPI(title="Test API")
    test_app.include_router(api_keys.router)
    test_app.include_router(health.router)
    test_app.include_router(events.router)
    test_app.include_router(inbox.router)
    test_app.include_router(webhooks.router)

    @test_app.get("/")
    async def root() -> dict[str, str]:
        return {"status": "ok", "message": "Zapier Triggers API"}

    @test_app.get("/health")
    async def health_check() -> dict[str, str]:
        return {"status": "healthy"}

    # Override dependencies to use test instances
    async def override_get_session() -> AsyncGenerator[AsyncSession, None]:
        yield test_session

    async def override_get_redis() -> AsyncGenerator[Redis, None]:
        yield test_redis

    test_app.dependency_overrides[get_session] = override_get_session
    test_app.dependency_overrides[get_redis] = override_get_redis

    # Use sync TestClient
    client = TestClient(test_app, raise_server_exceptions=True)
    yield client

    # Clean up overrides
    test_app.dependency_overrides.clear()


@pytest.fixture(scope="function")
def auth_headers(test_org_data: tuple[Organization, str]) -> dict[str, str]:
    """Create authentication headers with test API key."""
    _org, api_key = test_org_data
    return {
        "X-API-Key": api_key
    }


@pytest.fixture(scope="function")
def sample_event_data() -> dict[str, Any]:
    """Create sample event data for testing."""
    return {
        "type": "test.event",
        "data": {
            "message": "Test event",
            "timestamp": "2025-01-01T00:00:00Z",
        },
        "dedup_id": "test-dedup-123",
    }
