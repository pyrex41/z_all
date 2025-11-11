"""Performance monitoring middleware."""

import logging
import time
from typing import Callable

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware

logger = logging.getLogger(__name__)


class PerformanceMonitoringMiddleware(BaseHTTPMiddleware):
    """Middleware to track API response times and log slow requests."""

    async def dispatch(
        self, request: Request, call_next: Callable
    ) -> Response:
        """Track request timing and log performance metrics."""
        start_time = time.perf_counter()

        # Process request
        response = await call_next(request)

        # Calculate duration
        duration_ms = (time.perf_counter() - start_time) * 1000

        # Add performance header
        response.headers["X-Response-Time"] = f"{duration_ms:.2f}ms"

        # Log performance metrics
        log_msg = (
            f"{request.method} {request.url.path} "
            f"status={response.status_code} "
            f"duration={duration_ms:.2f}ms"
        )

        # Warn on slow requests (> 100ms target)
        if duration_ms > 100:
            logger.warning(f"SLOW REQUEST: {log_msg}")
        else:
            logger.info(log_msg)

        # Log alarm for very slow requests (> 1000ms)
        if duration_ms > 1000:
            logger.error(f"CRITICAL SLOW REQUEST: {log_msg}")

        return response
