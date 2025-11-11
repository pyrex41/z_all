#!/usr/bin/env python3
"""Setup a test organization for benchmarking.

Creates an organization with a test API key and webhook URL.
"""

import asyncio
from uuid import uuid4

from sqlmodel import select
from sqlmodel.ext.asyncio.session import AsyncSession

from zapier_triggers_api.auth import hash_api_key
from zapier_triggers_api.database import engine
from zapier_triggers_api.models import Organization, PlanTier


async def create_benchmark_org() -> tuple[str, str]:
    """Create a benchmark organization and return API key."""
    api_key = "zap_test_benchmark_key_for_load_testing_purposes_only_12345"
    api_key_hash = hash_api_key(api_key)

    async with AsyncSession(engine) as session:
        # Check if org already exists
        query = select(Organization).where(Organization.name == "Benchmark Test Org")
        result = await session.exec(query)
        existing_org = result.first()

        if existing_org:
            print(f"✅ Organization already exists: {existing_org.id}")
            return api_key, str(existing_org.id)

        # Create new org
        org = Organization(
            id=uuid4(),
            name="Benchmark Test Org",
            api_key_hash=api_key_hash,
            webhook_url="http://httpbin.org/post",  # Test webhook endpoint
            rate_limit=100000,  # High limit for testing
            plan=PlanTier.ENTERPRISE,
        )

        session.add(org)
        await session.commit()
        await session.refresh(org)

        print(f"✅ Created benchmark organization: {org.id}")
        print(f"   API Key: {api_key}")
        print(f"   Webhook: {org.webhook_url}")
        print(f"   Rate Limit: {org.rate_limit}/min")

        return api_key, str(org.id)


async def main() -> None:
    """Main setup function."""
    print("🔧 Setting up benchmark organization...")
    api_key, org_id = await create_benchmark_org()
    print(f"\n💡 Use this API key for benchmarking:")
    print(f"   {api_key}")
    print(f"\n🚀 Run benchmark with:")
    print(f"   uv run python benchmark.py --api-key {api_key}")


if __name__ == "__main__":
    asyncio.run(main())
