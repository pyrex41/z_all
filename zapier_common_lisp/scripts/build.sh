#!/bin/bash
# Build script for Common Lisp Zapier Triggers

set -e

echo "Building Common Lisp Zapier Triggers..."

# Build Docker image
docker build -t zapier-triggers-lisp:latest .

echo "Build complete!"
echo "Run with: docker run -p 5000:5000 --env-file .env zapier-triggers-lisp:latest"
