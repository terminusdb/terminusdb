#!/usr/bin/env bash
# E2E test runner: brings up the full plugin stack, waits for readiness,
# runs the mocha e2e suite, then tears down.
#
# Stack: TerminusDB + tdb-search + legacy_vectorlink + Ollama (embeddings)
#
# Usage:
#   ./tests/run-e2e.sh              # up, test, down
#   ./tests/run-e2e.sh --no-down    # up, test (leave stack running for debugging)
#   ./tests/run-e2e.sh --no-up      # run tests against an already-running stack
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COMPOSE_FILE="$PROJECT_ROOT/docker-compose.e2e.yml"
E2E_PORT="${E2E_PORT:-6370}"
SEARCH_PORT="${SEARCH_PORT:-8090}"

NO_DOWN=false
NO_UP=false
for arg in "$@"; do
  case "$arg" in
    --no-down) NO_DOWN=true ;;
    --no-up)   NO_UP=true ;;
  esac
done

# Cleanup function
cleanup() {
  if [ "$NO_DOWN" = false ]; then
    echo "→ tearing down e2e stack"
    docker compose -f "$COMPOSE_FILE" down -v --remove-orphans 2>/dev/null || true
  else
    echo "→ leaving e2e stack running (use --no-down was not passed)"
  fi
}
if [ "$NO_UP" = false ]; then
  trap cleanup EXIT
fi

# ── Bring up the stack ──────────────────────────────────────────────────────
if [ "$NO_UP" = false ]; then
  echo "→ building and starting e2e stack (this may take a while on first run)"
  docker compose -f "$COMPOSE_FILE" up -d --build

  echo "→ waiting for TerminusDB to be ready"
  for _ in $(seq 1 60); do
    if curl -fsS "http://localhost:$E2E_PORT/api/ok" >/dev/null 2>&1; then
      echo "  ✓ TerminusDB ready at http://localhost:$E2E_PORT"
      break
    fi
    sleep 2
  done

  if ! curl -fsS "http://localhost:$E2E_PORT/api/ok" >/dev/null 2>&1; then
    echo "✗ TerminusDB did not become ready" >&2
    docker compose -f "$COMPOSE_FILE" logs terminusdb >&2 || true
    exit 1
  fi

  echo "→ waiting for tdb-search engine to be ready"
  for _ in $(seq 1 60); do
    if curl -fsS "http://localhost:$SEARCH_PORT/health/live" >/dev/null 2>&1; then
      echo "  ✓ tdb-search ready at http://localhost:$SEARCH_PORT"
      break
    fi
    sleep 2
  done

  if ! curl -fsS "http://localhost:$SEARCH_PORT/health/live" >/dev/null 2>&1; then
    echo "✗ tdb-search did not become ready" >&2
    docker compose -f "$COMPOSE_FILE" logs tdb-search >&2 || true
    exit 1
  fi

  echo "→ waiting for Ollama model to be ready"
  for _ in $(seq 1 120); do
    # Check readiness via the engine's /health/ready endpoint
    ready=$(curl -fsS "http://localhost:$SEARCH_PORT/health/ready" 2>/dev/null || true)
    if echo "$ready" | grep -q '"ready":true' 2>/dev/null; then
      echo "  ✓ Ollama embeddings ready"
      break
    fi
    sleep 3
  done
fi

# ── Run the e2e test suite ──────────────────────────────────────────────────
echo "→ running e2e test suite"
cd "$PROJECT_ROOT/tests"

TERMINUSDB_BASE_URL="http://localhost:$E2E_PORT" \
TDB_SEARCH_URL="http://localhost:$SEARCH_PORT" \
TERMINUSDB_USER=admin \
TERMINUSDB_PASSWORD=root \
  npx mocha e2e/plugin-e2e.js --timeout 120000

echo "✓ e2e tests passed"
