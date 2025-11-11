#!/bin/bash
# Start script for Zapier Triggers API - Common Lisp

set -e

# Default configuration
PORT=${PORT:-5000}
WORKER_COUNT=${WORKER_COUNT:-4}
ENVIRONMENT=${ENVIRONMENT:-development}
DATABASE_URL=${DATABASE_URL:-postgresql://postgres:postgres@localhost:5432/zapier_triggers}

echo "========================================="
echo "Zapier Triggers API - Common Lisp"
echo "========================================="
echo "Port: $PORT"
echo "Workers: $WORKER_COUNT"
echo "Environment: $ENVIRONMENT"
echo "Database: $DATABASE_URL"
echo "========================================="
echo

# Create start script
cat > /tmp/zapier-start.lisp <<EOF
(ql:quickload :zapier-triggers :silent t)
(in-package :zapier-triggers)
(setf zapier-triggers.config:*port* $PORT)
(setf zapier-triggers.config:*worker-count* $WORKER_COUNT)
(setf zapier-triggers.config:*environment* "$ENVIRONMENT")
(setf zapier-triggers.config:*database-url* "$DATABASE_URL")
(main)
EOF

# Start server
sbcl --load /tmp/zapier-start.lisp

# Cleanup
rm -f /tmp/zapier-start.lisp
