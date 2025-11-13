# Build stage
FROM rust:1-alpine AS builder

WORKDIR /app

# Install build dependencies including static openssl
RUN apk add --no-cache musl-dev pkgconfig openssl-dev openssl-libs-static

# Copy all source files
COPY . .

# Build release binary (skip benches to avoid missing files)
# Use --bins to only build binaries, not benches or tests
RUN cargo build --release --bins

# Runtime stage (minimal)
FROM alpine:3.19

# Install runtime dependencies
RUN apk add --no-cache ca-certificates libgcc

WORKDIR /app

# Copy binary from builder
COPY --from=builder /app/target/release/zapier-triggers /usr/local/bin/zapier-triggers

# Copy migrations
COPY --from=builder /app/migrations /app/migrations

# Non-root user
RUN addgroup -g 1000 appuser && \
    adduser -D -u 1000 -G appuser appuser
USER appuser

EXPOSE 8080 9090

CMD ["zapier-triggers"]
