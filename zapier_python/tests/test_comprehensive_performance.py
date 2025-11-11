"""Comprehensive performance tests for FastAPI implementation."""

import sys
from pathlib import Path

# Add parent directory to path to import performance_test
sys.path.insert(0, str(Path(__file__).parent.parent))

from typing import Any

import pytest
from fastapi.testclient import TestClient

from performance_test import print_results, run_performance_test


@pytest.mark.performance
def test_performance_100_requests(
    test_client: TestClient,
    auth_headers: dict[str, str],
) -> None:
    """Test performance with 100 requests (quick test)."""
    results = run_performance_test(test_client, auth_headers, num_requests=100)
    print_results(results, "Quick Performance Test (100 requests)")

    # Assertions
    assert results["success_rate"] == 100.0, "All requests should succeed"
    assert results["latency_p95_ms"] < 100, f"P95 latency should be < 100ms, got {results['latency_p95_ms']:.2f}ms"


@pytest.mark.performance
def test_performance_500_requests(
    test_client: TestClient,
    auth_headers: dict[str, str],
) -> None:
    """Test performance with 500 requests (medium test)."""
    results = run_performance_test(test_client, auth_headers, num_requests=500)
    print_results(results, "Medium Performance Test (500 requests)")

    # Assertions
    assert results["success_rate"] == 100.0, "All requests should succeed"
    assert results["latency_p95_ms"] < 100, f"P95 latency should be < 100ms, got {results['latency_p95_ms']:.2f}ms"
    assert results["throughput_rps"] > 50, f"Throughput should be > 50 req/s, got {results['throughput_rps']:.1f}"


@pytest.mark.performance
@pytest.mark.slow
def test_performance_1000_requests(
    test_client: TestClient,
    auth_headers: dict[str, str],
) -> None:
    """Test performance with 1000 requests (full test)."""
    results = run_performance_test(test_client, auth_headers, num_requests=1000)
    print_results(results, "Full Performance Test (1000 requests)")

    # Assertions
    assert results["success_rate"] == 100.0, "All requests should succeed"
    assert results["latency_p95_ms"] < 100, f"P95 latency should be < 100ms, got {results['latency_p95_ms']:.2f}ms"
    assert results["throughput_rps"] > 100, f"Throughput should be > 100 req/s, got {results['throughput_rps']:.1f}"


@pytest.mark.performance
def test_performance_summary(
    test_client: TestClient,
    auth_headers: dict[str, str],
) -> None:
    """Run all performance tests and create a summary."""
    test_sizes = [100, 250, 500]
    all_results = []

    print(f"\n{'='*80}")
    print("🔥 COMPREHENSIVE PERFORMANCE TEST SUITE")
    print(f"{'='*80}\n")

    for size in test_sizes:
        results = run_performance_test(test_client, auth_headers, num_requests=size)
        print_results(results, f"Performance Test ({size} requests)")
        all_results.append(results)

    # Overall summary
    print(f"\n{'='*80}")
    print("📈 OVERALL PERFORMANCE SUMMARY")
    print(f"{'='*80}")

    avg_p95 = sum(r["latency_p95_ms"] for r in all_results) / len(all_results)
    avg_throughput = sum(r["throughput_rps"] for r in all_results) / len(all_results)
    total_requests = sum(r["total_requests"] for r in all_results)
    total_successful = sum(r["successful"] for r in all_results)

    print(f"Total Requests:      {total_requests:,}")
    print(f"Total Successful:    {total_successful:,}")
    print(f"Success Rate:        {(total_successful/total_requests)*100:.1f}%")
    print(f"Average P95 Latency: {avg_p95:.2f}ms")
    print(f"Average Throughput:  {avg_throughput:.1f} req/s")

    print(f"\n🏆 FastAPI Performance Grade:")
    if avg_p95 < 5:
        print(f"   Grade: A+ (Excellent - {avg_p95:.2f}ms)")
    elif avg_p95 < 10:
        print(f"   Grade: A  (Great - {avg_p95:.2f}ms)")
    elif avg_p95 < 50:
        print(f"   Grade: B  (Good - {avg_p95:.2f}ms)")
    elif avg_p95 < 100:
        print(f"   Grade: C  (Acceptable - {avg_p95:.2f}ms)")
    else:
        print(f"   Grade: D  (Needs Improvement - {avg_p95:.2f}ms)")

    print(f"{'='*80}\n")

    # Final assertions
    assert (total_successful / total_requests) * 100 == 100.0, "All requests should succeed"
    assert avg_p95 < 100, f"Average P95 latency should be < 100ms, got {avg_p95:.2f}ms"
