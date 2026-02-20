#!/usr/bin/env bash
# Create dvdrental database (if missing) and load schema.sql.
# Run from project root with devbox: devbox run init-db
# Requires PostgreSQL running (e.g. devbox services up postgresql).

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

DB="${PGDATABASE:-dvdrental}"

createdb "$DB" 2>/dev/null || true
psql -d "$DB" -f schema.sql
echo "Database $DB ready (schema loaded)."
