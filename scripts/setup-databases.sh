#!/bin/bash
# Setup separate databases for each implementation
# This ensures each implementation has its own clean database

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}  Setting up separate databases for each implementation${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Database credentials
DB_USER="zapier"
DB_PASS="zapier_dev_password"
DB_HOST="localhost"
DB_PORT="5432"

# Create databases if they don't exist
create_database() {
    local db_name=$1
    echo -e "${YELLOW}Creating database: ${db_name}${NC}"

    psql -U postgres -h $DB_HOST -tc "SELECT 1 FROM pg_database WHERE datname = '${db_name}'" | grep -q 1 || \
    psql -U postgres -h $DB_HOST -c "CREATE DATABASE ${db_name} OWNER ${DB_USER};"

    echo -e "${GREEN}✓ Database ${db_name} ready${NC}"
}

# Drop and recreate database (for fresh start)
recreate_database() {
    local db_name=$1
    echo -e "${YELLOW}Recreating database: ${db_name}${NC}"

    # Drop if exists
    psql -U postgres -h $DB_HOST -c "DROP DATABASE IF EXISTS ${db_name};" 2>/dev/null || true

    # Create fresh
    psql -U postgres -h $DB_HOST -c "CREATE DATABASE ${db_name} OWNER ${DB_USER};"

    echo -e "${GREEN}✓ Database ${db_name} recreated${NC}"
}

# Create zapier user if doesn't exist
echo -e "${YELLOW}Ensuring database user exists...${NC}"
psql -U postgres -h $DB_HOST -tc "SELECT 1 FROM pg_user WHERE usename = '${DB_USER}'" | grep -q 1 || \
psql -U postgres -h $DB_HOST -c "CREATE USER ${DB_USER} WITH PASSWORD '${DB_PASS}';"
echo -e "${GREEN}✓ User ${DB_USER} ready${NC}"
echo ""

# Setup Python database
echo -e "${BLUE}1. Python Database${NC}"
recreate_database "zapier_triggers_python"
cd zapier_python
echo -e "${YELLOW}Running Python migrations...${NC}"
uv run alembic upgrade head
echo -e "${GREEN}✓ Python database ready${NC}"
echo ""

# Setup Elixir database
echo -e "${BLUE}2. Elixir Database${NC}"
recreate_database "zapier_triggers_elixir"
cd ../zapier_elixir/zapier_triggers
echo -e "${YELLOW}Running Elixir migrations...${NC}"
mix ecto.migrate
echo -e "${GREEN}✓ Elixir database ready${NC}"
echo ""

# Setup Rust database
echo -e "${BLUE}3. Rust Database${NC}"
recreate_database "zapier_triggers_rust"
cd ../../zapier_rust
echo -e "${YELLOW}Running Rust migrations...${NC}"
# Add Rust migration command here when available
# sqlx migrate run
echo -e "${YELLOW}⚠ Rust migrations not yet implemented${NC}"
echo ""

# Setup Common Lisp database
echo -e "${BLUE}4. Common Lisp Database${NC}"
recreate_database "zapier_triggers_cl"
cd ../zapier_common_lisp
echo -e "${YELLOW}Running Common Lisp schema setup...${NC}"
# Common Lisp uses direct SQL, run schema file if it exists
if [ -f "schema.sql" ]; then
    psql -U $DB_USER -h $DB_HOST -d zapier_triggers_cl -f schema.sql
elif [ -f "db/schema.sql" ]; then
    psql -U $DB_USER -h $DB_HOST -d zapier_triggers_cl -f db/schema.sql
else
    echo -e "${YELLOW}⚠ No schema.sql found, will be created on first run${NC}"
fi
echo -e "${GREEN}✓ Common Lisp database ready${NC}"
echo ""

cd ..

echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}  All databases setup complete!${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo ""
echo -e "Databases created:"
echo -e "  • ${BLUE}zapier_triggers_python${NC}  - Python (FastAPI)"
echo -e "  • ${BLUE}zapier_triggers_elixir${NC}  - Elixir (Phoenix)"
echo -e "  • ${BLUE}zapier_triggers_rust${NC}    - Rust (Axum)"
echo -e "  • ${BLUE}zapier_triggers_cl${NC}      - Common Lisp (SBCL)"
echo ""
