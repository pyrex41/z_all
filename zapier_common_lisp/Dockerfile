# Multi-stage build for Common Lisp Zapier Triggers API
FROM fukamachi/sbcl:2.4.11-debian as builder

# Install build dependencies including CFFI requirements
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    git \
    build-essential \
    libpq-dev \
    libssl-dev \
    libev-dev \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Set up application directory (Quicklisp already installed in fukamachi/sbcl image)
WORKDIR /app
COPY . .

# Precompile dependencies and create standalone executable
# Use zapier-triggers:main as toplevel - it calls start-server and keeps process alive
RUN sbcl --non-interactive \
         --eval '(push (truename ".") asdf:*central-registry*)' \
         --eval '(ql:quickload :cffi-grovel)' \
         --eval '(ql:quickload :zapier-triggers)' \
         --eval '(sb-ext:save-lisp-and-die "zapier-triggers-server" :toplevel #'"'"'zapier-triggers:main :executable t :compression t)'

# Runtime stage - minimal image
FROM debian:bookworm-slim

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    libpq5 \
    libev4 \
    libssl3 \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1001 appuser

WORKDIR /app

# Copy the compiled executable and SQL schema from builder
COPY --from=builder --chown=appuser:appuser /app/zapier-triggers-server /app/zapier-triggers-server
COPY --from=builder --chown=appuser:appuser /app/sql /app/sql

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 5000

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:5000/health || exit 1

# Run the server
CMD ["/app/zapier-triggers-server"]
