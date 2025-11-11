"""Performance testing for event ingestion API.

Tests the performance target: < 100ms response time for event ingestion.

Run with: pytest tests/test_performance.py -v -s
"""

import asyncio
import statistics
import time
from typing import Any
from uuid import uuid4

import pytest
from fastapi.testclient import TestClient

from zapier_triggers_api.models import Organization

# Performance targets
TARGET_API_RESPONSE_TIME_MS = 10  # API should respond in < 10ms (just queues)
TARGET_PROCESSING_TIME_MS = 100  # Processing should complete in < 100ms
WARNING_API_RESPONSE_TIME_MS = 20  # Warn if API takes > 20ms


def test_single_event_ingestion_latency(
    test_client: TestClient,
    auth_headers: dict[str, str],
    sample_event_data: dict[str, Any],
) -> None:
    """Test single event ingestion meets < 10ms response time target."""
    # Warm-up request
    test_client.post(
        "/api/events",
        json={
            "type": "test.warmup",
            "data": {"message": "warmup"},
        },
        headers=auth_headers,
    )

    # Measure single event latency
    event_data = {
        **sample_event_data,
        "type": "test.performance",
        "dedup_id": str(uuid4()),
    }

    start_time = time.perf_counter()
    response = test_client.post(
        "/api/events",
        json=event_data,
        headers=auth_headers,
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


def test_concurrent_event_ingestion_throughput(
    test_client: TestClient,
    auth_headers: dict[str, str],
    sample_event_data: dict[str, Any],
) -> None:
    """Test concurrent event ingestion throughput and average latency."""
    num_events = 100

    # Warm-up
    test_client.post(
        "/api/events",
        json={"type": "test.warmup", "data": {"message": "warmup"}},
        headers=auth_headers,
    )

    # Send events and measure performance
    start_time = time.perf_counter()
    latencies = []
    status_codes = []

    for i in range(num_events):
        event_data = {
            **sample_event_data,
            "type": "test.concurrent",
            "dedup_id": str(uuid4()),
        }

        event_start = time.perf_counter()
        response = test_client.post(
            "/api/events",
            json=event_data,
            headers=auth_headers,
        )
        event_duration_ms = (time.perf_counter() - event_start) * 1000

        latencies.append(event_duration_ms)
        status_codes.append(response.status_code)

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

    print(f"\n✓ Event ingestion ({num_events} events):")
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


def test_burst_traffic_handling(
    test_client: TestClient,
    auth_headers: dict[str, str],
    sample_event_data: dict[str, Any],
) -> None:
    """Test handling of burst traffic (many events at once)."""
    num_events = 50

    # Send all events in rapid succession (burst)
    start_time = time.perf_counter()
    latencies = []
    status_codes = []

    for i in range(num_events):
        event_data = {
            **sample_event_data,
            "type": "test.burst",
            "dedup_id": str(uuid4()),
        }

        event_start = time.perf_counter()
        response = test_client.post(
            "/api/events",
            json=event_data,
            headers=auth_headers,
        )
        event_duration_ms = (time.perf_counter() - event_start) * 1000

        latencies.append(event_duration_ms)
        status_codes.append(response.status_code)

    total_duration = time.perf_counter() - start_time

    # Calculate metrics
    avg_latency = statistics.mean(latencies)
    max_latency = max(latencies)
    success_count = sum(1 for s in status_codes if s == 202)

    print(f"\n✓ Burst traffic handling ({num_events} events in rapid succession):")
    print(f"  - Total duration: {total_duration:.2f}s")
    print(f"  - Success rate: {success_count}/{num_events} ({100*success_count/num_events:.1f}%)")
    print(f"  - Average latency: {avg_latency:.2f}ms")
    print(f"  - Max latency: {max_latency:.2f}ms")

    # Assertions
    assert success_count == num_events, f"Not all events succeeded in burst: {num_events - success_count} failed"
    assert max_latency < 500, f"Max latency too high during burst: {max_latency:.2f}ms"
