#!/bin/bash
# Run tests for all implementations

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"

echo "🧪 Running tests for all Zapier Triggers API implementations..."
echo ""

# Track results
PYTHON_PASSED=0
ELIXIR_PASSED=0
RUST_PASSED=0
UNIFIED_PASSED=0

# Python tests
echo "================================"
echo "🐍 Testing Python implementation"
echo "================================"
if [ -d "$ROOT_DIR/zapier_python" ]; then
    cd "$ROOT_DIR/zapier_python"
    if command -v uv &> /dev/null; then
        if uv run pytest; then
            PYTHON_PASSED=1
            echo "✅ Python tests passed"
        else
            echo "❌ Python tests failed"
        fi
    else
        echo "⚠️  UV not found, skipping Python tests"
    fi
else
    echo "⚠️  Python implementation not found"
fi

echo ""

# Elixir tests
echo "================================"
echo "💧 Testing Elixir implementation"
echo "================================"
if [ -d "$ROOT_DIR/zapier_elixir/zapier_triggers" ]; then
    cd "$ROOT_DIR/zapier_elixir/zapier_triggers"
    if command -v mix &> /dev/null; then
        if mix test; then
            ELIXIR_PASSED=1
            echo "✅ Elixir tests passed"
        else
            echo "❌ Elixir tests failed"
        fi
    else
        echo "⚠️  Mix not found, skipping Elixir tests"
    fi
else
    echo "⚠️  Elixir implementation not found"
fi

echo ""

# Rust tests
echo "================================"
echo "🦀 Testing Rust implementation"
echo "================================"
if [ -d "$ROOT_DIR/zapier_rust" ] && [ -f "$ROOT_DIR/zapier_rust/Cargo.toml" ]; then
    cd "$ROOT_DIR/zapier_rust"
    if command -v cargo &> /dev/null; then
        if cargo test; then
            RUST_PASSED=1
            echo "✅ Rust tests passed"
        else
            echo "❌ Rust tests failed"
        fi
    else
        echo "⚠️  Cargo not found, skipping Rust tests"
    fi
else
    echo "⚠️  Rust implementation not yet ready"
fi

echo ""

# Unified test suite
echo "========================================"
echo "🎯 Running Unified Test Suite"
echo "========================================"
if [ -d "$ROOT_DIR/unified_test_suite" ]; then
    cd "$ROOT_DIR/unified_test_suite"
    if command -v uv &> /dev/null; then
        if uv run pytest tests/test_functional.py -v; then
            UNIFIED_PASSED=1
            echo "✅ Unified tests passed"
        else
            echo "❌ Unified tests failed"
        fi
    else
        echo "⚠️  UV not found, skipping unified tests"
    fi
else
    echo "⚠️  Unified test suite not found"
fi

echo ""
echo "========================================"
echo "📊 Test Results Summary"
echo "========================================"
echo "Python:  $([ $PYTHON_PASSED -eq 1 ] && echo '✅ PASS' || echo '❌ FAIL')"
echo "Elixir:  $([ $ELIXIR_PASSED -eq 1 ] && echo '✅ PASS' || echo '❌ FAIL')"
echo "Rust:    $([ $RUST_PASSED -eq 1 ] && echo '✅ PASS' || echo '⚠️  N/A')"
echo "Unified: $([ $UNIFIED_PASSED -eq 1 ] && echo '✅ PASS' || echo '❌ FAIL')"
echo ""

# Exit with error if any tests failed
if [ $PYTHON_PASSED -eq 0 ] || [ $ELIXIR_PASSED -eq 0 ] || [ $UNIFIED_PASSED -eq 0 ]; then
    exit 1
fi
