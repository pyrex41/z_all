# Zapier Triggers API - Python Implementation

> **Note**: This is part of a [monorepo](../README.md) with multiple implementations. See [comparison](../COMPARISON_SUMMARY.md) for performance analysis.

RESTful Triggers API for webhook ingestion, durable storage, and reliable delivery to Zapier workflows.

## Overview

A simple, reliable webhook ingestion system modeled after proven patterns (Stripe Webhooks). This Python implementation prioritizes simplicity and rapid development, making it ideal for MVPs and prototyping.

## Tech Stack

- **API Framework:** FastAPI (async)
- **Database:** PostgreSQL 16 with SQLModel/Alembic
- **Queue/Cache:** Redis 7 (Streams)
- **Package Manager:** UV (fast Python package management)
- **Testing:** pytest, ruff, mypy

## Quick Start

### Prerequisites

- Python 3.12+
- UV ([installation guide](https://github.com/astral-sh/uv))
- PostgreSQL 16
- Redis 7

### Installation

**From monorepo root:**
```bash
./scripts/setup-python.sh
```

**Or manually:**
```bash
# Navigate to Python implementation
cd zapier_python

# Install dependencies with UV
uv sync --all-extras

# Run tests
uv run pytest

# Start development server
uv run uvicorn src.zapier_triggers_api.main:app --reload
```

## Development

### Commands

```bash
# Install dependencies
uv sync

# Run linting
uv run ruff check .

# Run type checking
uv run mypy .

# Run tests with coverage
uv run pytest --cov

# Format code
uv run ruff format .
```

## API Endpoints

- `POST /events` - Ingest JSON events
- `GET /inbox` - Retrieve undelivered events
- `POST /ack` - Acknowledge delivered events

## Documentation

Full API documentation available at `/docs` when running the server.

## Monorepo Context

This Python implementation is part of a comparative study with:
- **[Elixir (Phoenix)](../zapier_elixir/zapier_triggers/README.md)** - Production-ready, 3-4x faster
- **[Rust](../zapier_rust/)** - High-performance (WIP)
- **[Unified Test Suite](../unified_test_suite/README.md)** - Cross-implementation testing

### When to Choose Python

✅ **Choose Python if:**
- Building MVP/prototype
- Team is primarily Python developers
- Need rapid development velocity
- Expected load < 500 req/s
- Integrating with Python data/ML ecosystem

⚠️ **Consider alternatives if:**
- Need high throughput (>500 req/s) → Use Elixir
- Infrastructure cost is critical → Use Elixir (no Redis)
- Building for production scale → Use Elixir

See [full comparison](../COMPARISON_SUMMARY.md) for performance benchmarks.

## License

MIT
