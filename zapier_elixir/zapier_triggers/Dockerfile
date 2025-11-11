# Multi-stage Dockerfile for Zapier Triggers API
# Builds a production-ready release with minimal image size

ARG ELIXIR_VERSION=1.17.3
ARG OTP_VERSION=27.2
ARG DEBIAN_VERSION=bookworm-20241111-slim

ARG BUILDER_IMAGE="hexpm/elixir:${ELIXIR_VERSION}-erlang-${OTP_VERSION}-debian-${DEBIAN_VERSION}"
ARG RUNNER_IMAGE="debian:${DEBIAN_VERSION}"

# ==============================================================================
# Stage 1: Build
# ==============================================================================
FROM ${BUILDER_IMAGE} as builder

# Install build dependencies
RUN apt-get update -y && \
    apt-get install -y build-essential git curl && \
    apt-get clean && rm -f /var/lib/apt/lists/*_*

# Set working directory
WORKDIR /app

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Set build environment
ENV MIX_ENV="prod"

# Copy dependency files
COPY mix.exs mix.lock ./

# Install dependencies
RUN mix deps.get --only $MIX_ENV
RUN mkdir config

# Copy compile-time config files
COPY config/config.exs config/${MIX_ENV}.exs config/
RUN mix deps.compile

# Copy application code
COPY priv priv
COPY lib lib

# Compile the application
RUN mix compile

# Generate OpenAPI spec at build time
RUN mix openapi.spec.json --spec ZapierTriggersWeb.ApiSpec --check

# Build release
COPY config/runtime.exs config/
COPY rel rel
RUN mix release

# ==============================================================================
# Stage 2: Runtime
# ==============================================================================
FROM ${RUNNER_IMAGE}

# Install runtime dependencies
RUN apt-get update -y && \
    apt-get install -y libstdc++6 openssl libncurses5 locales ca-certificates && \
    apt-get clean && rm -f /var/lib/apt/lists/*_*

# Set locale
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# Create non-root user
RUN useradd -m -u 1000 -s /bin/bash zapier

WORKDIR /app

# Set runtime environment
ENV MIX_ENV="prod"

# Copy release from builder
COPY --from=builder --chown=zapier:zapier /app/_build/${MIX_ENV}/rel/zapier_triggers ./

# Switch to non-root user
USER zapier

# Expose ports
EXPOSE 4000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD curl -f http://localhost:4000/health/live || exit 1

# Set default command
CMD ["/app/bin/zapier_triggers", "start"]
