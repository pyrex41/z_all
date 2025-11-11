"""Database connection and session management."""

import os
from collections.abc import AsyncGenerator

from sqlalchemy.ext.asyncio import AsyncEngine, AsyncSession, create_async_engine
from sqlmodel import SQLModel
from sqlmodel.ext.asyncio.session import AsyncSession as SQLModelAsyncSession

from zapier_triggers_api.config import settings

# Performance: Connection pool sized for concurrent processing
# Formula: MAX_CONCURRENT_EVENTS + API workers + buffer
# Default: 10 (concurrent events) + 5 (API) + 15 (buffer) = 30
MAX_CONCURRENT_EVENTS = int(os.environ.get("MAX_CONCURRENT_EVENTS", "10"))
POOL_SIZE = int(os.environ.get("DB_POOL_SIZE", str(MAX_CONCURRENT_EVENTS * 2 + 10)))
POOL_MAX_OVERFLOW = int(os.environ.get("DB_POOL_MAX_OVERFLOW", "20"))

# Create async engine with connection pooling
engine: AsyncEngine = create_async_engine(
    settings.database_url,
    echo=settings.environment == "development",
    future=True,
    pool_size=POOL_SIZE,  # Sized for concurrent processing
    max_overflow=POOL_MAX_OVERFLOW,  # Additional connections when pool is full
    pool_pre_ping=True,  # Verify connection health before using
    pool_recycle=3600,  # Recycle connections after 1 hour
    pool_timeout=30,  # Timeout waiting for connection from pool
)


async def init_db() -> None:
    """Initialize database tables."""
    async with engine.begin() as conn:
        await conn.run_sync(SQLModel.metadata.create_all)


async def get_session() -> AsyncGenerator[AsyncSession, None]:
    """Get database session for dependency injection."""
    async with SQLModelAsyncSession(engine) as session:
        yield session
