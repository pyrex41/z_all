"""Create database tables directly."""
import asyncio
from sqlmodel import SQLModel
from sqlalchemy.ext.asyncio import create_async_engine

from zapier_triggers_api.config import settings
from zapier_triggers_api.models import AuditLog, Event, EventDelivery, Organization  # noqa: F401


async def create_tables():
    """Create all database tables."""
    engine = create_async_engine(settings.database_url, echo=True)

    async with engine.begin() as conn:
        await conn.run_sync(SQLModel.metadata.create_all)

    await engine.dispose()
    print("✅ All tables created successfully!")


if __name__ == "__main__":
    asyncio.run(create_tables())
