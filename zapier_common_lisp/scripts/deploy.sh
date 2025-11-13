#!/bin/bash
# Deploy script for Fly.io

set -e

echo "Deploying to Fly.io..."

# Check if flyctl is installed
if ! command -v flyctl &> /dev/null; then
    echo "Error: flyctl is not installed"
    echo "Install with: curl -L https://fly.io/install.sh | sh"
    exit 1
fi

# Check if logged in
if ! flyctl auth whoami &> /dev/null; then
    echo "Please login to Fly.io first:"
    flyctl auth login
fi

# Deploy
flyctl deploy

echo "Deployment complete!"
echo "View logs with: flyctl logs"
echo "Check status with: flyctl status"
