#!/usr/bin/env elixir

# Benchmark script to measure async event ingestion performance
# Usage: mix run scripts/benchmark_async.exs

defmodule AsyncBenchmark do
  @moduledoc """
  Benchmarks the async event ingestion endpoint to validate <10ms response time.
  """

  def run do
    IO.puts("\n=== Async Event Ingestion Performance Benchmark ===\n")

    # Configuration
    base_url = System.get_env("API_URL") || "http://localhost:4000"
    num_requests = String.to_integer(System.get_env("REQUESTS") || "100")

    IO.puts("API URL: #{base_url}")
    IO.puts("Requests: #{num_requests}")
    IO.puts("\nGenerating API key...")

    # Generate API key
    api_key = generate_api_key(base_url)
    IO.puts("API key generated: #{String.slice(api_key, 0..10)}...")

    # Warm-up (5 requests)
    IO.puts("\nWarming up...")
    Enum.each(1..5, fn _i ->
      send_event(base_url, api_key)
      Process.sleep(100)
    end)

    # Benchmark event ingestion
    IO.puts("\nBenchmarking #{num_requests} event ingestion requests...")
    latencies = benchmark_ingestion(base_url, api_key, num_requests)

    # Calculate statistics
    print_statistics(latencies)

    # Verify <100ms target (ideally <10ms)
    p95 = percentile(latencies, 95)
    avg = Enum.sum(latencies) / length(latencies)

    IO.puts("\n=== Results ===")
    if avg < 10.0 do
      IO.puts("✅ EXCELLENT: Average latency #{Float.round(avg, 2)}ms (target <10ms)")
    elsif avg < 100.0 do
      IO.puts("✅ PASS: Average latency #{Float.round(avg, 2)}ms (target <100ms)")
    else
      IO.puts("❌ FAIL: Average latency #{Float.round(avg, 2)}ms exceeds 100ms target")
    end

    if p95 < 10.0 do
      IO.puts("✅ EXCELLENT: P95 latency #{Float.round(p95, 2)}ms (target <10ms)")
    elsif p95 < 100.0 do
      IO.puts("✅ PASS: P95 latency #{Float.round(p95, 2)}ms (target <100ms)")
    else
      IO.puts("❌ FAIL: P95 latency #{Float.round(p95, 2)}ms exceeds 100ms target")
    end

    IO.puts("\nThroughput: #{Float.round(1000.0 / avg, 2)} req/s")
    IO.puts("")
  end

  defp generate_api_key(base_url) do
    body = Jason.encode!(%{organization_name: "Benchmark Org", tier: "enterprise"})
    headers = [{"Content-Type", "application/json"}]

    case HTTPoison.post("#{base_url}/api/keys/generate", body, headers) do
      {:ok, %{status_code: 201, body: response_body}} ->
        %{"api_key" => api_key} = Jason.decode!(response_body)
        api_key

      {:error, reason} ->
        IO.puts("Failed to generate API key: #{inspect(reason)}")
        System.halt(1)
    end
  end

  defp benchmark_ingestion(base_url, api_key, num_requests) do
    Enum.map(1..num_requests, fn i ->
      latency = send_event(base_url, api_key)

      if rem(i, 10) == 0 do
        IO.write(".")
      end

      latency
    end)
  end

  defp send_event(base_url, api_key) do
    event = %{
      type: "benchmark.test",
      payload: %{
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
        data: "test payload"
      },
      dedup_id: "bench-#{:rand.uniform(1_000_000)}"
    }

    body = Jason.encode!(event)
    headers = [
      {"Content-Type", "application/json"},
      {"X-API-Key", api_key}
    ]

    start_time = System.monotonic_time(:microsecond)

    case HTTPoison.post("#{base_url}/api/events", body, headers) do
      {:ok, %{status_code: status_code}} when status_code in [200, 201, 202] ->
        end_time = System.monotonic_time(:microsecond)
        (end_time - start_time) / 1000.0  # Convert to milliseconds

      {:error, reason} ->
        IO.puts("\nError: #{inspect(reason)}")
        0.0
    end
  end

  defp print_statistics(latencies) do
    sorted = Enum.sort(latencies)
    count = length(sorted)

    avg = Enum.sum(sorted) / count
    min = Enum.min(sorted)
    max = Enum.max(sorted)
    p50 = percentile(sorted, 50)
    p95 = percentile(sorted, 95)
    p99 = percentile(sorted, 99)

    IO.puts("\n\n=== Latency Statistics ===")
    IO.puts("Requests: #{count}")
    IO.puts("Min:      #{Float.round(min, 2)}ms")
    IO.puts("Avg:      #{Float.round(avg, 2)}ms")
    IO.puts("P50:      #{Float.round(p50, 2)}ms")
    IO.puts("P95:      #{Float.round(p95, 2)}ms")
    IO.puts("P99:      #{Float.round(p99, 2)}ms")
    IO.puts("Max:      #{Float.round(max, 2)}ms")
  end

  defp percentile(sorted_list, p) do
    count = length(sorted_list)
    index = Float.ceil(count * p / 100) |> round() |> min(count) |> max(1)
    Enum.at(sorted_list, index - 1)
  end
end

# Run the benchmark
AsyncBenchmark.run()
