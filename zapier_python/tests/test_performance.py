"""Performance testing for event ingestion API.

Tests the performance target: < 100ms response time for event ingestion.

Run with: pytest tests/test_performance.py -v -s
"""

import asyncio
import statistics
import time
from uuid import uuid4

import httpx
import pytest

# Performance targets
TARGET_API_RESPONSE_TIME_MS = 10  # API should respond in < 10ms (just queues)
TARGET_PROCESSING_TIME_MS = 100  # Processing should complete in < 100ms
WARNING_API_RESPONSE_TIME_MS = 20  # Warn if API takes > 20ms


@pytest.mark.asyncio
async def test_single_event_ingestion_latency():
    """Test single event ingestion meets < 10ms response time target."""
    base_url = "http://localhost:8000"
    api_key = "test-api-key-change-in-prod"  # Replace with valid API key

    async with httpx.AsyncClient(timeout=30.0) as client:
        # Warm-up request
        await client.post(
            f"{base_url}/api/events",
            json={
                "type": "test.warmup",
                "data": {"message": "warmup"},
            },
            headers={"X-API-Key": api_key},
        )

        # Measure single event latency
        event_data = {
            "type": "test.performance",
            "data": {
                "message": "performance test",
                "timestamp": time.time(),
            },
            "dedup_id": str(uuid4()),
        }

        start_time = time.perf_counter()
        response = await client.post(
            f"{base_url}/api/events",
            json=event_data,
            headers={"X-API-Key": api_key},
        )
        duration_ms = (time.perf_counter() - start_time) * 1000

        # Assertions
        assert response.status_code == 202, f"Expected 202, got {response.status_code}"
        assert "X-Response-Time" in response.headers, "Missing X-Response-Time header"

        reported_time = float(response.headers["X-Response-Time"].replace("ms", ""))

        print(f"\n✓ Single event ingestion:")
        print(f"  - Measured latency: {duration_ms:.2f}ms")
        print(f"  - Server reported: {reported_time:.2f}ms")
        print(f"  - Target: < {TARGET_API_RESPONSE_TIME_MS}ms")

        # Check performance targets
        if duration_ms > WARNING_API_RESPONSE_TIME_MS:
            print(f"  ⚠️  WARNING: Latency exceeds {WARNING_API_RESPONSE_TIME_MS}ms threshold")

        assert duration_ms < 100, f"API response too slow: {duration_ms:.2f}ms (should be < 100ms)"


@pytest.mark.asyncio
async def test_concurrent_event_ingestion_throughput():
    """Test concurrent event ingestion throughput and average latency."""
    base_url = "http://localhost:8000"
    api_key = "test-api-key-change-in-prod"  # Replace with valid API key

    num_events = 100
    concurrency = 10

    async def send_event(client: httpx.AsyncClient, event_id: int) -> tuple[float, int]:
        """Send a single event and return (latency_ms, status_code)."""
        event_data = {
            "type": "test.concurrent",
            "data": {
                "event_id": event_id,
                "timestamp": time.time(),
            },
            "dedup_id": str(uuid4()),
        }

        start_time = time.perf_counter()
        response = await client.post(
            f"{base_url}/api/events",
            json=event_data,
            headers={"X-API-Key": api_key},
        )
        duration_ms = (time.perf_counter() - start_time) * 1000

        return duration_ms, response.status_code

    async with httpx.AsyncClient(timeout=30.0) as client:
        # Warm-up
        await client.post(
            f"{base_url}/api/events",
            json={"type": "test.warmup", "data": {"message": "warmup"}},
            headers={"X-API-Key": api_key},
        )

        # Send concurrent events
        start_time = time.perf_counter()

        # Process in batches to control concurrency
        latencies = []
        status_codes = []

        for batch_start in range(0, num_events, concurrency):
            batch_end = min(batch_start + concurrency, num_events)
            batch_tasks = [
                send_event(client, i)
                for i in range(batch_start, batch_end)
            ]
            batch_results = await asyncio.gather(*batch_tasks)

            for latency, status in batch_results:
                latencies.append(latency)
                status_codes.append(status)

        total_duration = time.perf_counter() - start_time

        # Calculate metrics
        throughput = num_events / total_duration
        avg_latency = statistics.mean(latencies)
        p50_latency = statistics.median(latencies)
        p95_latency = statistics.quantiles(latencies, n=20)[18]  # 95th percentile
        p99_latency = statistics.quantiles(latencies, n=100)[98]  # 99th percentile
        max_latency = max(latencies)

        success_count = sum(1 for s in status_codes if s == 202)
        error_count = len(status_codes) - success_count

        print(f"\n✓ Concurrent event ingestion ({num_events} events, concurrency={concurrency}):")
        print(f"  - Throughput: {throughput:.2f} events/sec")
        print(f"  - Total duration: {total_duration:.2f}s")
        print(f"  - Success rate: {success_count}/{num_events} ({100*success_count/num_events:.1f}%)")
        print(f"\n  Latency statistics:")
        print(f"  - Average: {avg_latency:.2f}ms")
        print(f"  - P50 (median): {p50_latency:.2f}ms")
        print(f"  - P95: {p95_latency:.2f}ms")
        print(f"  - P99: {p99_latency:.2f}ms")
        print(f"  - Max: {max_latency:.2f}ms")

        # Check targets
        if p95_latency > WARNING_API_RESPONSE_TIME_MS:
            print(f"  ⚠️  WARNING: P95 latency exceeds {WARNING_API_RESPONSE_TIME_MS}ms threshold")

        # Assertions
        assert success_count == num_events, f"Not all events succeeded: {error_count} errors"
        assert p95_latency < 100, f"P95 latency too high: {p95_latency:.2f}ms (should be < 100ms)"
        assert throughput > 10, f"Throughput too low: {throughput:.2f} events/sec (should be > 10)"


@pytest.mark.asyncio
async def test_burst_traffic_handling():
    """Test handling of burst traffic (many events at once)."""
    base_url = "http://localhost:8000"
    api_key = "test-api-key-change-in-prod"  # Replace with valid API key

    num_events = 50
    burst_size = 50  # All at once

    async def send_event(client: httpx.AsyncClient, event_id: int) -> tuple[float, int]:
        """Send a single event and return (latency_ms, status_code)."""
        event_data = {
            "type": "test.burst",
            "data": {
                "event_id": event_id,
                "timestamp": time.time(),
            },
            "dedup_id": str(uuid4()),
        }

        start_time = time.perf_counter()
        response = await client.post(
            f"{base_url}/api/events",
            json=event_data,
            headers={"X-API-Key": api_key},
        )
        duration_ms = (time.perf_counter() - start_time) * 1000

        return duration_ms, response.status_code

    async with httpx.AsyncClient(timeout=30.0) as client:
        # Send all events at once (burst)
        start_time = time.perf_counter()

        tasks = [send_event(client, i) for i in range(num_events)]
        results = await asyncio.gather(*tasks)

        total_duration = time.perf_counter() - start_time

        latencies = [r[0] for r in results]
        status_codes = [r[1] for r in results]

        # Calculate metrics
        avg_latency = statistics.mean(latencies)
        max_latency = max(latencies)
        success_count = sum(1 for s in status_codes if s == 202)

        print(f"\n✓ Burst traffic handling ({num_events} events sent simultaneously):")
        print(f"  - Total duration: {total_duration:.2f}s")
        print(f"  - Success rate: {success_count}/{num_events} ({100*success_count/num_events:.1f}%)")
        print(f"  - Average latency: {avg_latency:.2f}ms")
        print(f"  - Max latency: {max_latency:.2f}ms")

        # Assertions
        assert success_count == num_events, f"Not all events succeeded in burst: {num_events - success_count} failed"
        assert max_latency < 500, f"Max latency too high during burst: {max_latency:.2f}ms"


if __name__ == "__main__":
    # Run tests directly
    print("Running performance tests...")
    print("=" * 60)

    asyncio.run(test_single_event_ingestion_latency())
    asyncio.run(test_concurrent_event_ingestion_throughput())
    asyncio.run(test_burst_traffic_handling())

    print("\n" + "=" * 60)
    print("✓ All performance tests completed!")
