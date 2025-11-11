#!/bin/bash
# Test script for Zapier Triggers API - Common Lisp

set -e

echo "========================================="
echo "Zapier Triggers API - Tests"
echo "========================================="
echo

# Run Lisp tests
echo "🧪 Running Lisp unit tests..."
sbcl --eval '(ql:quickload :zapier-triggers/tests)' \
     --eval '(asdf:test-system :zapier-triggers)' \
     --quit

echo
echo "✅ All tests passed!"
echo
