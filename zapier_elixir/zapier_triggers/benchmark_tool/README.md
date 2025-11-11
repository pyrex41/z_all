# Zapier Triggers API Benchmark Tool

Self-contained benchmark tool for testing both Python and Elixir implementations.

## Setup

```bash
# Install dependencies with uv (one-time setup)
uv sync
```

## Quick Start

```bash
# Elixir API (auto-detect)
uv run python benchmark.py --url http://localhost:4000 --api-key YOUR_KEY --quick

# Python API (auto-detect)
uv run python benchmark.py --url http://localhost:8000 --api-key zap_test_benchmark --quick
```

## Full Documentation

See [BENCHMARK.md](BENCHMARK.md) for complete documentation.

## What's Included

- `benchmark.py` - The benchmark tool with auto-detection for Python/Elixir APIs
- `BENCHMARK.md` - Complete usage documentation
- `pyproject.toml` - Dependency management (httpx)

## Requirements

- Python 3.10+
- `uv` package manager ([install](https://github.com/astral-sh/uv))

## Example

```bash
# Start your Elixir server
cd .. && mix phx.server

# In another terminal, generate API key
curl -X POST http://localhost:4000/api/keys/generate \
  -H "Content-Type: application/json" \
  -d '{"organization_name":"Benchmark","tier":"enterprise"}'

# Run benchmark
cd benchmark_tool
uv run python benchmark.py \
  --url http://localhost:4000 \
  --api-key zap_test_YOUR_KEY \
  --requests 1000 \
  --concurrency 50
```
