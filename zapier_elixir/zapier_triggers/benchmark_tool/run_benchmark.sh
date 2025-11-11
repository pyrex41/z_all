#!/bin/bash
# Convenience wrapper for running benchmarks

cd "$(dirname "$0")"

if [ "$1" == "--help" ] || [ "$1" == "-h" ]; then
    echo "Zapier Triggers Benchmark Runner"
    echo ""
    echo "Usage:"
    echo "  ./run_benchmark.sh [OPTIONS]"
    echo ""
    echo "Examples:"
    echo "  ./run_benchmark.sh --quick"
    echo "  ./run_benchmark.sh --url http://localhost:4000 --api-key YOUR_KEY"
    echo "  ./run_benchmark.sh --url http://localhost:8000 --api-type python"
    echo ""
    echo "For all options, run:"
    echo "  uv run python benchmark.py --help"
    exit 0
fi

exec uv run python benchmark.py "$@"
