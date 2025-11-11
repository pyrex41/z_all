#!/usr/bin/env python3
"""Standalone performance test for FastAPI implementation.

Tests the FastAPI implementation in isolation with various load patterns.
"""

import statistics
import time
from typing import Any

import pytest
from fastapi.testclient import TestClient


def run_performance_test(client: TestClient, auth_headers: dict[str, str], num_requests: int = 1000) -> dict[str, Any]:
    """Run a performance test with specified number of requests."""
    latencies = []
    status_codes = []

    print(f"\n🚀 Running performance test with {num_requests} requests...")

    start_time = time.perf_counter()

    for i in range(num_requests):
        event_data = {
            "type": "test.perf",
            "data": {"iteration": i, "timestamp": time.time()},
            "dedup_id": f"perf-test-{i}",
        }

        req_start = time.perf_counter()
        response = client.post(
            "/api/events",
            json=event_data,
            headers=auth_headers,
        )
        req_duration_ms = (time.perf_counter() - req_start) * 1000

        latencies.append(req_duration_ms)
        status_codes.append(response.status_code)

        # Progress indicator
        if (i + 1) % 100 == 0:
            print(f"  Completed {i + 1}/{num_requests} requests...")

    total_duration = time.perf_counter() - start_time

    # Calculate statistics
    success_count = sum(1 for s in status_codes if s == 202)
    throughput = num_requests / total_duration

    results = {
        "total_requests": num_requests,
        "successful": success_count,
        "failed": num_requests - success_count,
        "success_rate": (success_count / num_requests) * 100,
        "duration_seconds": total_duration,
        "throughput_rps": throughput,
        "latency_avg_ms": statistics.mean(latencies),
        "latency_p50_ms": statistics.median(latencies),
        "latency_p95_ms": statistics.quantiles(latencies, n=20)[18],
        "latency_p99_ms": statistics.quantiles(latencies, n=100)[98],
        "latency_min_ms": min(latencies),
        "latency_max_ms": max(latencies),
    }

    return results


def print_results(results: dict[str, Any], test_name: str) -> None:
    """Print formatted test results."""
    print(f"\n{'='*80}")
    print(f"📊 {test_name}")
    print(f"{'='*80}")
    print(f"Total Requests:     {results['total_requests']:,}")
    print(f"Successful:         {results['successful']:,} ({results['success_rate']:.1f}%)")
    print(f"Failed:             {results['failed']:,}")
    print(f"Duration:           {results['duration_seconds']:.2f}s")
    print(f"Throughput:         {results['throughput_rps']:.1f} req/s")
    print(f"\nLatency Statistics:")
    print(f"  Average:          {results['latency_avg_ms']:.2f}ms")
    print(f"  P50 (median):     {results['latency_p50_ms']:.2f}ms")
    print(f"  P95:              {results['latency_p95_ms']:.2f}ms")
    print(f"  P99:              {results['latency_p99_ms']:.2f}ms")
    print(f"  Min:              {results['latency_min_ms']:.2f}ms")
    print(f"  Max:              {results['latency_max_ms']:.2f}ms")

    # Target analysis
    print(f"\n🎯 Target Analysis:")
    if results['latency_p95_ms'] < 10:
        print(f"  P95 < 10ms:       ✅ PASS ({results['latency_p95_ms']:.2f}ms)")
    elif results['latency_p95_ms'] < 100:
        print(f"  P95 < 100ms:      ✅ PASS ({results['latency_p95_ms']:.2f}ms)")
    else:
        print(f"  P95 < 100ms:      ❌ FAIL ({results['latency_p95_ms']:.2f}ms)")

    if results['throughput_rps'] > 100:
        print(f"  Throughput > 100: ✅ PASS ({results['throughput_rps']:.1f} req/s)")
    else:
        print(f"  Throughput > 100: ⚠️  WARNING ({results['throughput_rps']:.1f} req/s)")

    print(f"{'='*80}\n")


if __name__ == "__main__":
    # This will be run via pytest with fixtures
    print("Run this test using: pytest performance_test.py -v -s")
