# Multi-stage Dockerfile optimized for UV
FROM ghcr.io/astral-sh/uv:python3.12-bookworm-slim AS builder

# Set working directory
WORKDIR /app

# Copy dependency files
COPY pyproject.toml .python-version uv.lock README.md ./

# Install dependencies (leverages UV's cache)
RUN uv sync --frozen --no-dev

# Copy application code
COPY src ./src

# Production stage
FROM python:3.12-slim-bookworm

# Install UV in production
COPY --from=ghcr.io/astral-sh/uv:latest /uv /usr/local/bin/uv

WORKDIR /app

# Copy virtual environment from builder
COPY --from=builder /app/.venv /app/.venv
COPY --from=builder /app/src /app/src
COPY --from=builder /app/pyproject.toml /app/pyproject.toml

# Add venv to PATH and set PYTHONPATH to include src directory
ENV PATH="/app/.venv/bin:$PATH"
ENV PYTHONPATH="/app/src:$PYTHONPATH"

# Expose port
EXPOSE 8000

# Run with uvicorn using src module path
CMD ["python", "-m", "uvicorn", "zapier_triggers_api.main:app", "--host", "0.0.0.0", "--port", "8000"]
