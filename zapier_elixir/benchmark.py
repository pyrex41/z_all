#!/usr/bin/env python3
"""Performance benchmark for Zapier Triggers API.

Usage:
    # Local testing
    uv run python benchmark.py

    # Against deployed instance
    uv run python benchmark.py --url https://api.example.com --api-key zap_live_xxx

    # Custom load profile
    uv run python benchmark.py --users 100 --duration 60 --rps 1000
"""

import argparse
import asyncio
import json
import statistics
import time
from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from typing import Any
from uuid import uuid4

import httpx


@dataclass
class BenchmarkResult:
    """Results from a benchmark run."""

    operation: str
    total_requests: int
    successful: int
    failed: int
    duration_seconds: float
    requests_per_second: float
    avg_latency_ms: float
    p50_latency_ms: float
    p95_latency_ms: float
    p99_latency_ms: float
    min_latency_ms: float
    max_latency_ms: float
    error_rate: float


class APIBenchmark:
    """Benchmark suite for API performance testing."""

    def __init__(self, base_url: str, api_key: str):
        self.base_url = base_url.rstrip("/")
        self.api_key = api_key
        self.results: list[BenchmarkResult] = []

    async def benchmark_event_ingestion(
        self, num_requests: int, concurrency: int
    ) -> BenchmarkResult:
        """Benchmark POST /events endpoint."""
        print(f"\n🔥 Benchmarking event ingestion: {num_requests} requests, {concurrency} concurrent")

        latencies: list[float] = []
        successes = 0
        failures = 0

        async with httpx.AsyncClient(timeout=30.0) as client:
            start_time = time.time()

            async def send_event() -> None:
                nonlocal successes, failures
                event_start = time.time()

                try:
                    response = await client.post(
                        f"{self.base_url}/events",
                        headers={
                            "X-API-Key": self.api_key,
                            "Content-Type": "application/json",
                        },
                        json={
                            "type": "benchmark.test",
                            "data": {
                                "user_id": str(uuid4()),
                                "action": "click",
                                "timestamp": datetime.utcnow().isoformat(),
                                "metadata": {"source": "benchmark"},
                            },
                            "dedup_id": str(uuid4()),
                        },
                    )
                    response.raise_for_status()
                    successes += 1
                except Exception as e:
                    failures += 1
                finally:
                    latencies.append((time.time() - event_start) * 1000)

            # Execute in batches with concurrency limit
            tasks = []
            for i in range(num_requests):
                tasks.append(send_event())
                if len(tasks) >= concurrency:
                    await asyncio.gather(*tasks)
                    tasks = []

            if tasks:
                await asyncio.gather(*tasks)

            duration = time.time() - start_time

        return self._create_result(
            "POST /events",
            num_requests,
            successes,
            failures,
            duration,
            latencies,
        )

    async def benchmark_inbox_retrieval(
        self, num_requests: int, concurrency: int
    ) -> BenchmarkResult:
        """Benchmark GET /inbox endpoint."""
        print(f"\n📥 Benchmarking inbox retrieval: {num_requests} requests, {concurrency} concurrent")

        latencies: list[float] = []
        successes = 0
        failures = 0

        async with httpx.AsyncClient(timeout=30.0) as client:
            start_time = time.time()

            async def get_inbox() -> None:
                nonlocal successes, failures
                req_start = time.time()

                try:
                    response = await client.get(
                        f"{self.base_url}/inbox",
                        headers={"X-API-Key": self.api_key},
                        params={"limit": 100},
                    )
                    response.raise_for_status()
                    successes += 1
                except Exception:
                    failures += 1
                finally:
                    latencies.append((time.time() - req_start) * 1000)

            tasks = []
            for i in range(num_requests):
                tasks.append(get_inbox())
                if len(tasks) >= concurrency:
                    await asyncio.gather(*tasks)
                    tasks = []

            if tasks:
                await asyncio.gather(*tasks)

            duration = time.time() - start_time

        return self._create_result(
            "GET /inbox",
            num_requests,
            successes,
            failures,
            duration,
            latencies,
        )

    async def benchmark_mixed_workload(
        self, duration_seconds: int, target_rps: int
    ) -> dict[str, BenchmarkResult]:
        """Benchmark mixed workload (70% ingestion, 20% inbox, 10% ack)."""
        print(f"\n⚡ Benchmarking mixed workload: {target_rps} RPS for {duration_seconds}s")

        stats: dict[str, dict[str, Any]] = {
            "ingestion": {"latencies": [], "successes": 0, "failures": 0},
            "inbox": {"latencies": [], "successes": 0, "failures": 0},
            "ack": {"latencies": [], "successes": 0, "failures": 0},
        }

        event_ids: list[str] = []
        request_count = 0

        async with httpx.AsyncClient(timeout=30.0) as client:
            start_time = time.time()
            interval = 1.0 / target_rps

            async def send_request(operation: str) -> None:
                nonlocal request_count
                req_start = time.time()

                try:
                    if operation == "ingestion":
                        response = await client.post(
                            f"{self.base_url}/events",
                            headers={
                                "X-API-Key": self.api_key,
                                "Content-Type": "application/json",
                            },
                            json={
                                "type": "benchmark.mixed",
                                "data": {"index": request_count},
                                "dedup_id": str(uuid4()),
                            },
                        )
                        if response.status_code == 201:
                            data = response.json()
                            event_ids.append(data["id"])

                    elif operation == "inbox":
                        response = await client.get(
                            f"{self.base_url}/inbox",
                            headers={"X-API-Key": self.api_key},
                            params={"limit": 50},
                        )

                    elif operation == "ack" and event_ids:
                        # Ack up to 10 events
                        ids_to_ack = event_ids[:10]
                        response = await client.post(
                            f"{self.base_url}/inbox/ack",
                            headers={
                                "X-API-Key": self.api_key,
                                "Content-Type": "application/json",
                            },
                            json={"event_ids": ids_to_ack},
                        )
                        event_ids[:10] = []

                    response.raise_for_status()
                    stats[operation]["successes"] += 1

                except Exception:
                    stats[operation]["failures"] += 1
                finally:
                    stats[operation]["latencies"].append((time.time() - req_start) * 1000)
                    request_count += 1

            # Run for specified duration
            while time.time() - start_time < duration_seconds:
                # Determine operation based on distribution
                rand = (request_count % 10) / 10
                if rand < 0.7:
                    operation = "ingestion"
                elif rand < 0.9:
                    operation = "inbox"
                else:
                    operation = "ack"

                await send_request(operation)
                await asyncio.sleep(interval)

            actual_duration = time.time() - start_time

        # Create results for each operation
        results = {}
        for op, data in stats.items():
            total = data["successes"] + data["failures"]
            if total > 0:
                results[op] = self._create_result(
                    op,
                    total,
                    data["successes"],
                    data["failures"],
                    actual_duration,
                    data["latencies"],
                )

        return results

    def _create_result(
        self,
        operation: str,
        total: int,
        successes: int,
        failures: int,
        duration: float,
        latencies: list[float],
    ) -> BenchmarkResult:
        """Create a benchmark result from collected data."""
        sorted_latencies = sorted(latencies)

        return BenchmarkResult(
            operation=operation,
            total_requests=total,
            successful=successes,
            failed=failures,
            duration_seconds=duration,
            requests_per_second=total / duration if duration > 0 else 0,
            avg_latency_ms=statistics.mean(latencies) if latencies else 0,
            p50_latency_ms=self._percentile(sorted_latencies, 50),
            p95_latency_ms=self._percentile(sorted_latencies, 95),
            p99_latency_ms=self._percentile(sorted_latencies, 99),
            min_latency_ms=min(latencies) if latencies else 0,
            max_latency_ms=max(latencies) if latencies else 0,
            error_rate=(failures / total * 100) if total > 0 else 0,
        )

    def _percentile(self, sorted_values: list[float], percentile: int) -> float:
        """Calculate percentile from sorted values."""
        if not sorted_values:
            return 0
        index = int(len(sorted_values) * percentile / 100)
        return sorted_values[min(index, len(sorted_values) - 1)]

    def print_results(self) -> None:
        """Print benchmark results in a formatted table."""
        print("\n" + "=" * 100)
        print("📊 BENCHMARK RESULTS")
        print("=" * 100)

        for result in self.results:
            print(f"\n{result.operation}")
            print("-" * 100)
            print(f"Total Requests:    {result.total_requests:,}")
            print(f"Successful:        {result.successful:,} ({result.successful/result.total_requests*100:.1f}%)")
            print(f"Failed:            {result.failed:,} ({result.error_rate:.2f}%)")
            print(f"Duration:          {result.duration_seconds:.2f}s")
            print(f"Throughput:        {result.requests_per_second:.1f} req/s")
            print()
            print(f"Latency (ms):")
            print(f"  Average:         {result.avg_latency_ms:.2f}")
            print(f"  P50:             {result.p50_latency_ms:.2f}")
            print(f"  P95:             {result.p95_latency_ms:.2f}")
            print(f"  P99:             {result.p99_latency_ms:.2f}")
            print(f"  Min:             {result.min_latency_ms:.2f}")
            print(f"  Max:             {result.max_latency_ms:.2f}")

        print("\n" + "=" * 100)

        # Summary
        total_requests = sum(r.total_requests for r in self.results)
        total_duration = sum(r.duration_seconds for r in self.results)
        avg_throughput = sum(r.requests_per_second for r in self.results) / len(self.results)
        avg_p95 = sum(r.p95_latency_ms for r in self.results) / len(self.results)

        print("\n📈 SUMMARY")
        print("-" * 100)
        print(f"Total Requests:    {total_requests:,}")
        print(f"Total Duration:    {total_duration:.2f}s")
        print(f"Avg Throughput:    {avg_throughput:.1f} req/s")
        print(f"Avg P95 Latency:   {avg_p95:.2f}ms")
        print()

        # PRD targets comparison
        print("🎯 PRD TARGET COMPARISON")
        print("-" * 100)
        for result in self.results:
            if "POST /events" in result.operation or "ingestion" in result.operation:
                target = 100  # <100ms p95
                status = "✅ PASS" if result.p95_latency_ms < target else "❌ FAIL"
                print(f"Ingestion P95 < 100ms:  {result.p95_latency_ms:.2f}ms {status}")

        print("=" * 100 + "\n")


async def main() -> None:
    """Main benchmark execution."""
    parser = argparse.ArgumentParser(description="Benchmark Zapier Triggers API")
    parser.add_argument(
        "--url",
        default="http://localhost:8000",
        help="API base URL (default: http://localhost:8000)",
    )
    parser.add_argument(
        "--api-key",
        default="zap_test_benchmark",
        help="API key for authentication",
    )
    parser.add_argument(
        "--requests",
        type=int,
        default=1000,
        help="Number of requests per test (default: 1000)",
    )
    parser.add_argument(
        "--concurrency",
        type=int,
        default=50,
        help="Concurrent requests (default: 50)",
    )
    parser.add_argument(
        "--duration",
        type=int,
        default=30,
        help="Duration for mixed workload test in seconds (default: 30)",
    )
    parser.add_argument(
        "--rps",
        type=int,
        default=100,
        help="Target requests per second for mixed workload (default: 100)",
    )
    parser.add_argument(
        "--quick",
        action="store_true",
        help="Quick test mode (100 requests, 10 concurrent)",
    )

    args = parser.parse_args()

    if args.quick:
        args.requests = 100
        args.concurrency = 10
        args.duration = 10
        args.rps = 50

    print("🚀 Zapier Triggers API Benchmark")
    print("=" * 100)
    print(f"Target:       {args.url}")
    print(f"API Key:      {args.api_key[:20]}...")
    print(f"Requests:     {args.requests:,}")
    print(f"Concurrency:  {args.concurrency}")
    print("=" * 100)

    benchmark = APIBenchmark(args.url, args.api_key)

    # Test 1: Event Ingestion
    result = await benchmark.benchmark_event_ingestion(args.requests, args.concurrency)
    benchmark.results.append(result)

    # Test 2: Inbox Retrieval
    result = await benchmark.benchmark_inbox_retrieval(args.requests, args.concurrency)
    benchmark.results.append(result)

    # Test 3: Mixed Workload
    print(f"\n⏱️  Running mixed workload for {args.duration} seconds...")
    mixed_results = await benchmark.benchmark_mixed_workload(args.duration, args.rps)
    benchmark.results.extend(mixed_results.values())

    # Print all results
    benchmark.print_results()


if __name__ == "__main__":
    asyncio.run(main())
