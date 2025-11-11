#!/bin/bash
# Setup script for Zapier Triggers API - Common Lisp

set -e

echo "========================================="
echo "Zapier Triggers API - Common Lisp Setup"
echo "========================================="
echo

# Check for SBCL
if ! command -v sbcl &> /dev/null; then
    echo "❌ SBCL not found. Please install SBCL first."
    echo "   macOS: brew install sbcl"
    echo "   Linux: apt-get install sbcl"
    exit 1
fi

echo "✅ SBCL found: $(sbcl --version | head -1)"

# Check for Quicklisp
if [ ! -d "$HOME/quicklisp" ]; then
    echo "📦 Installing Quicklisp..."
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql:add-to-init-file)' \
         --quit
    rm quicklisp.lisp
    echo "✅ Quicklisp installed"
else
    echo "✅ Quicklisp already installed"
fi

# Check for PostgreSQL
if ! command -v psql &> /dev/null; then
    echo "⚠️  PostgreSQL not found. Install PostgreSQL 16 to use the API."
    echo "   macOS: brew install postgresql@16"
    echo "   Linux: apt-get install postgresql-16"
else
    echo "✅ PostgreSQL found: $(psql --version)"
fi

# Create database if needed
DB_NAME=${DATABASE_NAME:-zapier_triggers}
DB_USER=${DATABASE_USER:-postgres}

if command -v psql &> /dev/null; then
    echo "📊 Setting up database..."

    # Try to create database (ignore if exists)
    createdb -U $DB_USER $DB_NAME 2>/dev/null || echo "   Database already exists"

    # Run schema
    psql -U $DB_USER -d $DB_NAME -f sql/schema.sql > /dev/null 2>&1 || true

    echo "✅ Database ready"
fi

# Load dependencies
echo "📚 Loading Lisp dependencies..."
sbcl --eval '(ql:quickload :zapier-triggers)' --quit 2>&1 | tail -5

echo
echo "✅ Setup complete!"
echo
echo "To start the server, run:"
echo "  ./scripts/start.sh"
echo
echo "Or manually:"
echo "  sbcl --load start.lisp"
echo
