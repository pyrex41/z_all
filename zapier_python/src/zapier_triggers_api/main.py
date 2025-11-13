"""FastAPI application entry point."""

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from zapier_triggers_api.config import settings
from zapier_triggers_api.database import init_db
from zapier_triggers_api.middleware import PerformanceMonitoringMiddleware
from zapier_triggers_api.routes import api_keys, events, health, inbox, webhooks

app = FastAPI(
    title="Zapier Triggers API",
    description="RESTful API for webhook ingestion and delivery to Zapier workflows",
    version="0.1.0",
    docs_url="/docs" if settings.environment == "development" else None,
    redoc_url="/redoc" if settings.environment == "development" else None,
)


@app.on_event("startup")
async def startup_event() -> None:
    """Initialize database on startup."""
    await init_db()

# Performance monitoring (must be first to measure full request time)
app.add_middleware(PerformanceMonitoringMiddleware)

# CORS configuration
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"] if settings.environment == "development" else [],
    allow_credentials=True,
    allow_methods=["GET", "POST"],
    allow_headers=["*"],
)

# Include routers
# Public routes (no auth required)
app.include_router(api_keys.router)
app.include_router(health.router)

# Protected routes (require X-API-Key header)
app.include_router(events.router)
app.include_router(inbox.router)
app.include_router(webhooks.router)


@app.get("/")
async def root() -> dict[str, str]:
    """Health check endpoint."""
    return {"status": "ok", "message": "Zapier Triggers API"}


@app.get("/health")
async def health() -> dict[str, str]:
    """Detailed health check."""
    return {"status": "healthy"}
