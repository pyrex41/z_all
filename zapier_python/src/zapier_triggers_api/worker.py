"""Background worker for event processing (persistence + delivery).

PERFORMANCE OPTIMIZATIONS:
- Concurrent event processing (configurable concurrency limit)
- Separate webhook delivery queue to prevent blocking
- Fast DB persistence (< 100ms target)
- Async webhook delivery with retries
"""

import asyncio
import json
import logging
import os
import time
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

# Performance configuration
MAX_CONCURRENT_EVENTS = int(os.environ.get("MAX_CONCURRENT_EVENTS", "10"))
WEBHOOK_DELIVERY_STREAM = "zapier:webhook-deliveries"


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


async def queue_webhook_delivery(
    event_id: str,
    delivery_id: str,
    redis: Redis,
) -> None:
    """Queue webhook delivery to separate stream for async processing.

    This prevents webhook delivery (which can take 30s) from blocking
    event persistence and processing.

    Uses MAXLEN to prevent unbounded stream growth.
    """
    delivery_data: dict[str, str | int | float] = {
        "event_id": event_id,
        "delivery_id": delivery_id,
        "timestamp": datetime.utcnow().isoformat(),
    }
    # Use MAXLEN to prevent unbounded growth
    await redis.xadd(
        WEBHOOK_DELIVERY_STREAM,
        delivery_data,  # type: ignore[arg-type]
        maxlen=settings.redis_stream_max_length,
        approximate=True,
    )
    logger.debug(f"Queued webhook delivery for event {event_id}")


async def process_event_from_queue(
    event_data: dict[str, bytes],
    redis: Redis,
    session: AsyncSession,
) -> None:
    """Process event from Redis Streams queue - FAST persistence only.

    PERFORMANCE OPTIMIZED: This now handles ONLY:
    1. Deduplication check (Redis - fast)
    2. Database persistence (< 100ms target)
    3. Queue webhook delivery (separate worker)

    Webhook delivery is queued separately to prevent blocking (30s timeout issue).
    """
    start_time = time.perf_counter()
    event_id = None
    processing_status = "unknown"

    try:
        # Parse queue data
        event_id = UUID(event_data[b"event_id"].decode())
        org_id = UUID(event_data[b"org_id"].decode())
        event_type = event_data[b"type"].decode()
        payload = json.loads(event_data[b"payload"].decode())
        dedup_id = event_data[b"dedup_id"].decode() or None
        timestamp_str = event_data[b"timestamp"].decode()
        created_at = datetime.fromisoformat(timestamp_str)

        # 1. Check deduplication (Redis - ~1ms)
        if dedup_id:
            is_duplicate = await check_deduplication(dedup_id, str(org_id), redis)
            if is_duplicate:
                duration_ms = (time.perf_counter() - start_time) * 1000
                logger.warning(
                    f"Duplicate event {event_id} (dedup_id: {dedup_id}) detected "
                    f"in {duration_ms:.2f}ms"
                )
                processing_status = "duplicate"
                return

        # 2. Check if event already persisted (idempotency)
        existing_event_query = select(Event).where(Event.id == event_id)
        existing_event = (await session.exec(existing_event_query)).first()

        if existing_event:
            duration_ms = (time.perf_counter() - start_time) * 1000
            logger.info(
                f"Event {event_id} already persisted, skipping to delivery queue "
                f"({duration_ms:.2f}ms)"
            )
            event = existing_event
            processing_status = "already_exists"

            # Still queue webhook delivery if needed
            delivery_query = select(EventDelivery).where(EventDelivery.event_id == event_id)
            delivery = (await session.exec(delivery_query)).first()
            if delivery:
                await queue_webhook_delivery(
                    event_id=str(event_id),
                    delivery_id=str(delivery.id),
                    redis=redis,
                )
            return
        else:
            # 3. Persist event to database (fast write)
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

        # Commit event + delivery (fast DB write)
        await session.commit()

        # Calculate processing time
        duration_ms = (time.perf_counter() - start_time) * 1000
        processing_status = "persisted"

        logger.info(
            f"Event {event_id} persisted in {duration_ms:.2f}ms "
            f"(target: < 100ms)"
        )

        # 5. Queue webhook delivery (separate worker - NON-BLOCKING)
        # This prevents 30s webhook timeout from blocking event processing
        await queue_webhook_delivery(
            event_id=str(event_id),
            delivery_id=str(delivery.id),
            redis=redis,
        )

        # Log performance warning if too slow
        if duration_ms > 100:
            logger.warning(
                f"SLOW event processing: {event_id} took {duration_ms:.2f}ms "
                f"(target: < 100ms)"
            )

    except Exception as e:
        duration_ms = (time.perf_counter() - start_time) * 1000
        processing_status = "error"
        logger.error(
            f"Error processing event {event_id} from queue after {duration_ms:.2f}ms: {e}",
            exc_info=True,
        )


async def process_single_event(
    message_id: bytes,
    data: dict[str, bytes],
    redis: Redis,
    session_factory: Callable[[], Any],
    consumer_group: str,
    stream_name: str,
) -> tuple[bool, str]:
    """Process a single event and acknowledge it.

    Returns: (success: bool, status: str) for error tracking
    """
    try:
        # Process event from queue (persistence only - fast)
        async for session in session_factory():
            await process_event_from_queue(data, redis, session)
            break

        # Acknowledge message
        await redis.xack(stream_name, consumer_group, message_id)
        return (True, "success")

    except Exception as e:
        # Error is isolated - doesn't affect other concurrent events
        logger.error(
            f"Error processing message {message_id} (isolated, won't affect other events): {e}",
            exc_info=True,
        )
        return (False, f"error: {str(e)[:100]}")


async def consume_stream(redis: Redis, session_factory: Callable[[], Any]) -> None:
    """Consume events from Redis Streams with CONCURRENT processing.

    PERFORMANCE OPTIMIZED:
    - Processes multiple events concurrently (up to MAX_CONCURRENT_EVENTS)
    - Non-blocking webhook delivery (queued separately)
    - Fast persistence (< 100ms target per event)
    - Error isolation: One failing event doesn't affect others
    """
    consumer_group = "event-processors"
    # Dynamic consumer name for horizontal scaling
    consumer_name = f"processor-{os.getpid()}"

    # Create consumer group if not exists
    try:
        await redis.xgroup_create(
            settings.redis_stream_name, consumer_group, id="0", mkstream=True
        )
    except Exception:
        pass  # Group already exists

    logger.info(f"Event processor started (PID: {os.getpid()})")
    logger.info(f"Concurrent processing: {MAX_CONCURRENT_EVENTS} events")
    logger.info("Processing: deduplication + persistence (webhooks queued separately)")

    while True:
        try:
            # Read batch of messages
            messages = await redis.xreadgroup(
                groupname=consumer_group,
                consumername=consumer_name,
                streams={settings.redis_stream_name: ">"},
                count=MAX_CONCURRENT_EVENTS,  # Fetch up to concurrency limit
                block=5000,
            )

            for stream_name, stream_messages in messages:
                if not stream_messages:
                    continue

                batch_start = time.perf_counter()

                # Process messages concurrently with error isolation
                tasks = [
                    process_single_event(
                        message_id,
                        data,
                        redis,
                        session_factory,
                        consumer_group,
                        stream_name.decode() if isinstance(stream_name, bytes) else stream_name,
                    )
                    for message_id, data in stream_messages
                ]

                # Wait for all concurrent tasks to complete
                # return_exceptions=True ensures error isolation
                results = await asyncio.gather(*tasks, return_exceptions=True)

                # Log batch statistics
                batch_duration = (time.perf_counter() - batch_start) * 1000
                success_count = sum(1 for r in results if isinstance(r, tuple) and r[0])
                error_count = len(results) - success_count

                if error_count > 0:
                    logger.warning(
                        f"Batch processed: {success_count} success, {error_count} errors "
                        f"in {batch_duration:.2f}ms"
                    )
                else:
                    logger.debug(
                        f"Batch processed: {success_count} events in {batch_duration:.2f}ms"
                    )

        except Exception as e:
            logger.error(f"Stream consumer error: {e}", exc_info=True)
            await asyncio.sleep(5)


async def process_webhook_delivery(
    delivery_data: dict[str, bytes],
    redis: Redis,
    session: AsyncSession,
) -> None:
    """Process webhook delivery from queue (can take up to 30s - non-blocking)."""
    try:
        event_id = UUID(delivery_data[b"event_id"].decode())
        delivery_id = UUID(delivery_data[b"delivery_id"].decode())

        # Fetch event and delivery
        event_query = select(Event).where(Event.id == event_id)
        event = (await session.exec(event_query)).first()

        delivery_query = select(EventDelivery).where(EventDelivery.id == delivery_id)
        delivery = (await session.exec(delivery_query)).first()

        if not event or not delivery:
            logger.error(f"Event {event_id} or delivery {delivery_id} not found")
            return

        # Check if already delivered
        if delivery.status == DeliveryStatus.DELIVERED:
            logger.info(f"Event {event_id} already delivered")
            return

        # Fetch organization
        org_query = select(Organization).where(Organization.id == event.org_id)
        org = (await session.exec(org_query)).first()

        if not org:
            logger.error(f"Organization {event.org_id} not found")
            return

        if not org.webhook_url:
            logger.warning(f"No webhook URL for org {org.id}, skipping delivery")
            return

        # Deliver webhook (can take up to 30s - but doesn't block event processing)
        await deliver_event(event, delivery, org, session)

    except Exception as e:
        logger.error(f"Error processing webhook delivery: {e}", exc_info=True)


async def consume_webhook_deliveries(redis: Redis, session_factory: Callable[[], Any]) -> None:
    """Consume webhook deliveries from separate stream.

    This runs independently from event processing to prevent blocking.
    Webhook delivery can take up to 30s with timeouts.
    """
    consumer_group = "webhook-delivery-processors"
    consumer_name = f"webhook-processor-{os.getpid()}"

    # Create consumer group if not exists
    try:
        await redis.xgroup_create(
            WEBHOOK_DELIVERY_STREAM, consumer_group, id="0", mkstream=True
        )
    except Exception:
        pass  # Group already exists

    logger.info(f"Webhook delivery processor started (PID: {os.getpid()})")
    logger.info("Processing webhook deliveries (can take up to 30s each)")

    while True:
        try:
            # Read from webhook delivery stream
            messages = await redis.xreadgroup(
                groupname=consumer_group,
                consumername=consumer_name,
                streams={WEBHOOK_DELIVERY_STREAM: ">"},
                count=5,  # Process fewer at a time (webhooks are slow)
                block=5000,
            )

            for stream_name, stream_messages in messages:
                for message_id, data in stream_messages:
                    try:
                        # Process webhook delivery
                        async for session in session_factory():
                            await process_webhook_delivery(data, redis, session)
                            break

                        # Acknowledge message
                        await redis.xack(WEBHOOK_DELIVERY_STREAM, consumer_group, message_id)

                    except Exception as e:
                        logger.error(f"Error processing webhook delivery {message_id}: {e}", exc_info=True)

        except Exception as e:
            logger.error(f"Webhook delivery consumer error: {e}", exc_info=True)
            await asyncio.sleep(5)


async def main() -> None:
    """Main worker entry point - runs both event and webhook processors."""
    logging.basicConfig(level=logging.INFO)

    from zapier_triggers_api.redis_client import redis

    try:
        # Run both consumers concurrently
        await asyncio.gather(
            consume_stream(redis, get_session),
            consume_webhook_deliveries(redis, get_session),
        )

    finally:
        await redis.aclose()


if __name__ == "__main__":
    asyncio.run(main())
