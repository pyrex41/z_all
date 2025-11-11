"""Application configuration."""

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """Application settings from environment."""

    model_config = SettingsConfigDict(env_file=".env", env_file_encoding="utf-8")

    # Database
    database_url: str = Field(
        default="postgresql+asyncpg://zapier:zapier_dev_password@localhost:5432/zapier_triggers"
    )

    # Redis
    redis_url: str = Field(default="redis://localhost:6379")
    redis_stream_name: str = Field(default="zapier:events")
    redis_consumer_group: str = Field(default="zapier:workers")

    # Redis Stream Limits (prevent unbounded growth)
    redis_stream_max_length: int = Field(default=100000)  # Max events in stream
    redis_stream_ttl: int = Field(default=86400)  # 24 hours in seconds

    # Security
    api_key_secret: str = Field(default="dev-secret-change-in-prod")

    # Rate Limiting (requests per minute)
    rate_limit_free: int = Field(default=100)
    rate_limit_pro: int = Field(default=1000)
    rate_limit_business: int = Field(default=10000)
    rate_limit_enterprise: int = Field(default=100000)

    # Webhook Delivery
    webhook_timeout: int = Field(default=10)
    webhook_max_retries: int = Field(default=5)
    webhook_retry_backoff_base: int = Field(default=2)
    disable_webhook_delivery: bool = Field(default=False)

    # Environment
    environment: str = Field(default="development")
    log_level: str = Field(default="INFO")

    # Monitoring
    prometheus_enabled: bool = Field(default=True)
    otel_enabled: bool = Field(default=False)
    otel_endpoint: str = Field(default="http://localhost:4318")


settings = Settings()
