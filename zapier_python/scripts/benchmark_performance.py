#!/usr/bin/env python3
"""Standalone performance benchmark for event ingestion API.

This script measures:
1. API response time (target: < 10ms)
2. Throughput (events/sec)
3. Latency distribution (P50, P95, P99)

Usage:
    python scripts/benchmark_performance.py
    python scripts/benchmark_performance.py --url http://localhost:8000 --api-key YOUR_KEY
    python scripts/benchmark_performance.py --events 1000 --concurrency 20
"""

import argparse
import asyncio
import statistics
import sys
import time
from uuid import uuid4

try:
    import httpx
except ImportError:
    print("Error: httpx is required. Install with: pip install httpx")
    sys.exit(1)


class PerformanceBenchmark:
    """Performance benchmark runner."""

    def __init__(self, base_url: str, api_key: str):
        self.base_url = base_url
        self.api_key = api_key

    async def send_event(self, client: httpx.AsyncClient, event_id: int) -> tuple[float, int, str]:
        """Send a single event and return (latency_ms, status_code, event_id)."""
        event_data = {
            "type": "benchmark.test",
            "data": {
                "event_id": event_id,
                "timestamp": time.time(),
            },
            "dedup_id": str(uuid4()),
        }

        start_time = time.perf_counter()
        try:
            response = await client.post(
                f"{self.base_url}/api/events",
                json=event_data,
                headers={"X-API-Key": self.api_key},
            )
            duration_ms = (time.perf_counter() - start_time) * 1000

            returned_event_id = ""
            if response.status_code == 202:
                data = response.json()
                returned_event_id = data.get("id", "")

            return duration_ms, response.status_code, returned_event_id

        except Exception as e:
            duration_ms = (time.perf_counter() - start_time) * 1000
            print(f"  ✗ Error sending event {event_id}: {e}")
            return duration_ms, 0, ""

    async def warmup(self):
        """Warm up the API (caches, connections, etc.)."""
        print("🔥 Warming up...")
        async with httpx.AsyncClient(timeout=30.0) as client:
            for _ in range(5):
                await self.send_event(client, -1)
        print("✓ Warmup complete\n")

    async def run_single_event_test(self):
        """Test single event ingestion latency."""
        print("=" * 60)
        print("TEST 1: Single Event Ingestion Latency")
        print("=" * 60)

        async with httpx.AsyncClient(timeout=30.0) as client:
            latencies = []

            for i in range(10):
                duration_ms, status_code, event_id = await self.send_event(client, i)
                latencies.append(duration_ms)

                status_icon = "✓" if status_code == 202 else "✗"
                print(f"  {status_icon} Event {i+1}: {duration_ms:.2f}ms (status: {status_code})")

            avg_latency = statistics.mean(latencies)
            min_latency = min(latencies)
            max_latency = max(latencies)

            print(f"\n  Results:")
            print(f"    - Average: {avg_latency:.2f}ms")
            print(f"    - Min: {min_latency:.2f}ms")
            print(f"    - Max: {max_latency:.2f}ms")
            print(f"    - Target: < 10ms")

            if avg_latency < 10:
                print(f"    ✓ PASSED: Average latency meets target")
            elif avg_latency < 20:
                print(f"    ⚠️  WARNING: Average latency is acceptable but above target")
            else:
                print(f"    ✗ FAILED: Average latency exceeds target")

    async def run_throughput_test(self, num_events: int, concurrency: int):
        """Test concurrent event ingestion throughput."""
        print("\n" + "=" * 60)
        print(f"TEST 2: Throughput Test ({num_events} events, concurrency={concurrency})")
        print("=" * 60)

        async with httpx.AsyncClient(timeout=30.0) as client:
            latencies = []
            status_codes = []

            start_time = time.perf_counter()

            # Process in batches to control concurrency
            for batch_start in range(0, num_events, concurrency):
                batch_end = min(batch_start + concurrency, num_events)
                batch_tasks = [
                    self.send_event(client, i)
                    for i in range(batch_start, batch_end)
                ]
                batch_results = await asyncio.gather(*batch_tasks)

                for latency, status, _ in batch_results:
                    latencies.append(latency)
                    status_codes.append(status)

                # Progress indicator
                progress = (batch_end / num_events) * 100
                print(f"  Progress: {progress:.1f}% ({batch_end}/{num_events})")

            total_duration = time.perf_counter() - start_time

            # Calculate metrics
            throughput = num_events / total_duration
            avg_latency = statistics.mean(latencies)
            p50_latency = statistics.median(latencies)
            p95_latency = statistics.quantiles(latencies, n=20)[18] if len(latencies) >= 20 else max(latencies)
            p99_latency = statistics.quantiles(latencies, n=100)[98] if len(latencies) >= 100 else max(latencies)
            max_latency = max(latencies)
            min_latency = min(latencies)

            success_count = sum(1 for s in status_codes if s == 202)
            error_count = len(status_codes) - success_count

            print(f"\n  Results:")
            print(f"    Throughput:")
            print(f"      - {throughput:.2f} events/sec")
            print(f"      - Total duration: {total_duration:.2f}s")
            print(f"      - Success rate: {success_count}/{num_events} ({100*success_count/num_events:.1f}%)")
            print(f"\n    Latency Distribution:")
            print(f"      - Min: {min_latency:.2f}ms")
            print(f"      - P50 (median): {p50_latency:.2f}ms")
            print(f"      - Average: {avg_latency:.2f}ms")
            print(f"      - P95: {p95_latency:.2f}ms")
            print(f"      - P99: {p99_latency:.2f}ms")
            print(f"      - Max: {max_latency:.2f}ms")

            # Performance assessment
            print(f"\n    Assessment:")
            if p95_latency < 10:
                print(f"      ✓ EXCELLENT: P95 < 10ms")
            elif p95_latency < 20:
                print(f"      ✓ GOOD: P95 < 20ms")
            elif p95_latency < 50:
                print(f"      ⚠️  ACCEPTABLE: P95 < 50ms")
            else:
                print(f"      ✗ NEEDS IMPROVEMENT: P95 > 50ms")

            if throughput > 100:
                print(f"      ✓ EXCELLENT throughput: > 100 events/sec")
            elif throughput > 50:
                print(f"      ✓ GOOD throughput: > 50 events/sec")
            else:
                print(f"      ⚠️  LOW throughput: {throughput:.2f} events/sec")

    async def run_burst_test(self, burst_size: int):
        """Test handling of burst traffic."""
        print("\n" + "=" * 60)
        print(f"TEST 3: Burst Traffic Handling ({burst_size} events simultaneously)")
        print("=" * 60)

        async with httpx.AsyncClient(timeout=30.0) as client:
            start_time = time.perf_counter()

            # Send all events at once
            tasks = [self.send_event(client, i) for i in range(burst_size)]
            results = await asyncio.gather(*tasks)

            total_duration = time.perf_counter() - start_time

            latencies = [r[0] for r in results]
            status_codes = [r[1] for r in results]

            # Calculate metrics
            avg_latency = statistics.mean(latencies)
            max_latency = max(latencies)
            success_count = sum(1 for s in status_codes if s == 202)

            print(f"\n  Results:")
            print(f"    - Total duration: {total_duration:.2f}s")
            print(f"    - Success rate: {success_count}/{burst_size} ({100*success_count/burst_size:.1f}%)")
            print(f"    - Average latency: {avg_latency:.2f}ms")
            print(f"    - Max latency: {max_latency:.2f}ms")

            if success_count == burst_size and max_latency < 100:
                print(f"    ✓ PASSED: All events accepted with reasonable latency")
            elif success_count == burst_size:
                print(f"    ⚠️  WARNING: All events accepted but some slow responses")
            else:
                print(f"    ✗ FAILED: {burst_size - success_count} events failed")

    async def run_all_tests(self, num_events: int, concurrency: int, burst_size: int):
        """Run all performance tests."""
        await self.warmup()
        await self.run_single_event_test()
        await self.run_throughput_test(num_events, concurrency)
        await self.run_burst_test(burst_size)

        print("\n" + "=" * 60)
        print("✓ All benchmark tests completed!")
        print("=" * 60)


async def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Performance benchmark for event ingestion API")
    parser.add_argument(
        "--url",
        default="http://localhost:8000",
        help="Base URL of the API (default: http://localhost:8000)",
    )
    parser.add_argument(
        "--api-key",
        default="test-api-key-change-in-prod",
        help="API key for authentication",
    )
    parser.add_argument(
        "--events",
        type=int,
        default=100,
        help="Number of events for throughput test (default: 100)",
    )
    parser.add_argument(
        "--concurrency",
        type=int,
        default=10,
        help="Concurrency level for throughput test (default: 10)",
    )
    parser.add_argument(
        "--burst",
        type=int,
        default=50,
        help="Burst size for burst test (default: 50)",
    )

    args = parser.parse_args()

    print("\n" + "=" * 60)
    print("EVENT INGESTION API - PERFORMANCE BENCHMARK")
    print("=" * 60)
    print(f"  URL: {args.url}")
    print(f"  Events: {args.events}")
    print(f"  Concurrency: {args.concurrency}")
    print(f"  Burst size: {args.burst}")
    print("=" * 60 + "\n")

    benchmark = PerformanceBenchmark(args.url, args.api_key)
    await benchmark.run_all_tests(args.events, args.concurrency, args.burst)


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("\n\nBenchmark interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\n\nError running benchmark: {e}")
        sys.exit(1)
