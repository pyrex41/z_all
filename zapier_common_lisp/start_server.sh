#!/bin/bash

# Start Zapier Triggers API - Common Lisp

echo "Starting Zapier Triggers API (Common Lisp)..."
echo ""

# Check if PostgreSQL is running
if ! pg_isready -h localhost -p 5432 > /dev/null 2>&1; then
    echo "⚠️  PostgreSQL is not running on localhost:5432"
    echo "   Please start PostgreSQL first"
    exit 1
fi

# Check if SBCL is installed
if ! command -v sbcl &> /dev/null; then
    echo "❌ SBCL is not installed"
    echo "   Please install SBCL first"
    exit 1
fi

# Start the server
sbcl --non-interactive \
     --eval "(require :asdf)" \
     --eval "(push (truename \".\") asdf:*central-registry*)" \
     --eval "(ql:quickload :zapier-triggers)" \
     --eval "(zapier-triggers:main)"
