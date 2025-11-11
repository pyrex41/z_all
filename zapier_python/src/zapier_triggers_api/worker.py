"""Background worker for event processing (persistence + delivery)."""

import asyncio
import json
import logging
from collections.abc import Callable
from datetime import datetime, timedelta
from typing import Any
from uuid import UUID, uuid4

import httpx
from redis.asyncio import Redis
from sqlalchemy import update
from sqlmodel import select
from sqlmodel.ext.asyncio.session import AsyncSession

from zapier_triggers_api.config import settings
from zapier_triggers_api.database import get_session
from zapier_triggers_api.models import DeliveryStatus, Event, EventDelivery, Organization

logger = logging.getLogger(__name__)

MAX_RETRIES = 5
RETRY_DELAYS = [30, 60, 300, 900, 3600]  # 30s, 1m, 5m, 15m, 1h


async def calculate_next_retry(attempts: int) -> datetime | None:
    """Calculate next retry time with exponential backoff."""
    if attempts >= MAX_RETRIES:
        return None
    delay = RETRY_DELAYS[attempts] if attempts < len(RETRY_DELAYS) else RETRY_DELAYS[-1]
    return datetime.utcnow() + timedelta(seconds=delay)


async def deliver_event(
    event: Event,
    delivery: EventDelivery,
    org: Organization,
    session: AsyncSession,
) -> bool:
    """Deliver event to webhook URL."""
    try:
        # Check if webhook delivery is disabled (for performance testing)
        if settings.disable_webhook_delivery:
            logger.debug(f"Webhook delivery disabled, marking event {event.id} as delivered without HTTP call")
        else:
            async with httpx.AsyncClient(timeout=30.0) as client:
                response = await client.post(
                    org.webhook_url,
                    json={
                        "event_id": str(event.id),
                        "event_type": event.type,
                        "payload": event.payload,
                        "created_at": event.created_at.isoformat(),
                    },
                    headers={"Content-Type": "application/json"},
                )
                response.raise_for_status()

        # Success
        update_stmt = (
            update(EventDelivery)
            .where(EventDelivery.id == delivery.id)  # type: ignore[arg-type]
            .values(
                status=DeliveryStatus.DELIVERED,
                delivered_at=datetime.utcnow(),
                updated_at=datetime.utcnow(),
            )
        )
        await session.exec(update_stmt)
        await session.commit()
        logger.info(f"Delivered event {event.id} to {org.name}")
        return True

    except Exception as e:
        # Failed - update retry logic
        attempts = delivery.attempts + 1
        next_retry = await calculate_next_retry(attempts)

        if next_retry:
            status = DeliveryStatus.RETRYING
            logger.warning(f"Delivery failed for event {event.id}, retry #{attempts}: {e}")
        else:
            status = DeliveryStatus.FAILED
            logger.error(f"Delivery permanently failed for event {event.id}: {e}")

        update_stmt = (
            update(EventDelivery)
            .where(EventDelivery.id == delivery.id)  # type: ignore[arg-type]
            .values(
                status=status,
                attempts=attempts,
                error_message=str(e)[:2048],
                last_attempt_at=datetime.utcnow(),
                updated_at=datetime.utcnow(),
            )
        )
        await session.exec(update_stmt)
        await session.commit()
        return False


async def check_deduplication(
    dedup_id: str,
    org_id: str,
    redis: Redis,
) -> bool:
    """Check if event with dedup_id already exists. Returns True if duplicate."""
    key = f"dedup:{org_id}:{dedup_id}"
    exists = await redis.get(key)

    if exists:
        return True  # Duplicate detected

    # Set dedup key with 24h expiry
    await redis.setex(key, 86400, "1")
    return False  # Not a duplicate


async def process_event_from_queue(
    event_data: dict[str, bytes],
    redis: Redis,
    session: AsyncSession,
) -> None:
    """Process event from Redis Streams queue - full persistence + delivery.

    This handles the complete event lifecycle:
    1. Deduplication check
    2. Database persistence
    3. Delivery record creation
    4. Webhook delivery
    """
    try:
        # Parse queue data
        event_id = UUID(event_data[b"event_id"].decode())
        org_id = UUID(event_data[b"org_id"].decode())
        event_type = event_data[b"type"].decode()
        payload = json.loads(event_data[b"payload"].decode())
        dedup_id = event_data[b"dedup_id"].decode() or None
        timestamp_str = event_data[b"timestamp"].decode()
        created_at = datetime.fromisoformat(timestamp_str)

        # 1. Check deduplication (async)
        if dedup_id:
            is_duplicate = await check_deduplication(dedup_id, str(org_id), redis)
            if is_duplicate:
                logger.warning(
                    f"Duplicate event {event_id} (dedup_id: {dedup_id}) detected, skipping"
                )
                return

        # 2. Check if event already persisted (idempotency)
        existing_event_query = select(Event).where(Event.id == event_id)
        existing_event = (await session.exec(existing_event_query)).first()

        if existing_event:
            logger.info(f"Event {event_id} already persisted, skipping to delivery")
            event = existing_event
        else:
            # 3. Persist event to database
            event = Event(
                id=event_id,
                org_id=org_id,
                type=event_type,
                payload=payload,
                dedup_id=dedup_id,
                created_at=created_at,
            )
            session.add(event)

        # 4. Check if delivery record exists
        delivery_query = select(EventDelivery).where(EventDelivery.event_id == event_id)
        delivery = (await session.exec(delivery_query)).first()

        if not delivery:
            # Create delivery record
            delivery = EventDelivery(
                id=uuid4(),
                event_id=event_id,
                status=DeliveryStatus.PENDING,
                attempts=0,
                created_at=datetime.utcnow(),
                updated_at=datetime.utcnow(),
            )
            session.add(delivery)

        # Commit event + delivery
        await session.commit()
        logger.info(f"Event {event_id} persisted successfully")

        # 5. Fetch org and deliver webhook
        org_query = select(Organization).where(Organization.id == org_id)
        org = (await session.exec(org_query)).first()

        if not org:
            logger.error(f"Organization {org_id} not found for event {event_id}")
            return

        if not org.webhook_url:
            logger.warning(f"No webhook URL configured for org {org_id}, skipping delivery")
            return

        # Check if already delivered
        if delivery.status == DeliveryStatus.DELIVERED:
            logger.info(f"Event {event_id} already delivered")
            return

        # Deliver webhook
        await deliver_event(event, delivery, org, session)

    except Exception as e:
        logger.error(f"Error processing event from queue: {e}", exc_info=True)


async def consume_stream(redis: Redis, session_factory: Callable[[], Any]) -> None:
    """Consume events from Redis Streams for async processing."""
    consumer_group = "event-processors"
    consumer_name = "processor-1"

    # Create consumer group if not exists
    try:
        await redis.xgroup_create(
            settings.redis_stream_name, consumer_group, id="0", mkstream=True
        )
    except Exception:
        pass  # Group already exists

    logger.info("Event processor started, listening for events...")
    logger.info("Processing: deduplication, persistence, and delivery")

    while True:
        try:
            # Read from stream
            messages = await redis.xreadgroup(
                groupname=consumer_group,
                consumername=consumer_name,
                streams={settings.redis_stream_name: ">"},
                count=10,
                block=5000,
            )

            for stream_name, stream_messages in messages:
                for message_id, data in stream_messages:
                    try:
                        # Process event from queue (full lifecycle)
                        async for session in session_factory():
                            await process_event_from_queue(data, redis, session)
                            break

                        # Acknowledge message
                        await redis.xack(settings.redis_stream_name, consumer_group, message_id)

                    except Exception as e:
                        logger.error(f"Error processing message {message_id}: {e}", exc_info=True)

        except Exception as e:
            logger.error(f"Stream consumer error: {e}", exc_info=True)
            await asyncio.sleep(5)


async def main() -> None:
    """Main worker entry point."""
    logging.basicConfig(level=logging.INFO)

    from zapier_triggers_api.redis_client import redis

    try:
        await consume_stream(redis, get_session)

    finally:
        await redis.aclose()


if __name__ == "__main__":
    asyncio.run(main())
