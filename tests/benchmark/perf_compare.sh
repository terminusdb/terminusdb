#!/bin/bash
# Performance comparison script: runs 17 test files twice against a branch.
# Usage: ./perf_compare.sh <label> <server_script> [backend]
# Output: /tmp/perf_<label>_run<n>.log and timing summary on stdout.

set -e

LABEL="$1"
SERVER_SCRIPT="$2"
BACKEND="${3:-swipl}"
TEST_DIR="/Users/hoijnet/Code/devops/twinfoxdb/terminusdb/tests/test"

TEST_FILES=(
  document-auth.js
  document-backlink.js
  document-delete.js
  document-embedding.js
  document-full-replace.js
  document-get-hierarchy.js
  document-get.js
  document-hyphenated-prefix.js
  document-interval.js
  document-read-consistency.js
  document-utf8.js
  woql-auth.js
  woql-noauth.js
  data-version.js
  error-handling.js
  log.js
  info_ok.js
)

METRICS_URL="http://127.0.0.1:6363"
# Try /api/metrics first (current branch), fall back to /metrics (12.1-rc)
fetch_metrics() {
  local out
  out=$(curl -s "${METRICS_URL}/api/metrics" 2>/dev/null)
  if echo "$out" | grep -q "twinfoxdb_atoms"; then
    echo "$out"
  else
    curl -s "${METRICS_URL}/metrics" 2>/dev/null
  fi
}

for run in 1 2; do
  echo "=== ${LABEL} Run ${run} ==="

  # Stop and start clean server
  "$SERVER_SCRIPT" stop 2>/dev/null || true
  sleep 1
  if [ "$BACKEND" = "swipl" ]; then
    TERMINUSDB_SERVER_BACKEND=swipl "$SERVER_SCRIPT" start --clean > /dev/null 2>&1
  else
    "$SERVER_SCRIPT" start --clean > /dev/null 2>&1
  fi
  sleep 2

  # Capture atoms before
  ATOMS_BEFORE=$(fetch_metrics | grep "^twinfoxdb_atoms " | awk '{print $2}')
  TABLE_BEFORE=$(fetch_metrics | grep "^twinfoxdb_table_space_used_bytes " | awk '{print $2}')
  echo "  atoms_before=${ATOMS_BEFORE} table_space_before=${TABLE_BEFORE}"

  TOTAL_MS=0
  TOTAL_PASS=0
  TOTAL_FAIL=0

  for tf in "${TEST_FILES[@]}"; do
    START=$(python3 -c 'import time; print(int(time.time()*1000))')
    RESULT=$(cd /Users/hoijnet/Code/devops/twinfoxdb/terminusdb && npx mocha --reporter dot "${TEST_DIR}/${tf}" 2>&1 || true)
    END=$(python3 -c 'import time; print(int(time.time()*1000))')
    ELAPSED=$((END - START))

    PASS=$(echo "$RESULT" | grep -E "^\s+[0-9]+ passing" | awk '{print $1}')
    FAIL=$(echo "$RESULT" | grep -E "^\s+[0-9]+ failing" | awk '{print $1}')
    PASS=${PASS:-0}
    FAIL=${FAIL:-0}

    TOTAL_MS=$((TOTAL_MS + ELAPSED))
    TOTAL_PASS=$((TOTAL_PASS + PASS))
    TOTAL_FAIL=$((TOTAL_FAIL + FAIL))

    echo "  ${tf}: ${ELAPSED}ms pass=${PASS} fail=${FAIL}"
  done

  # Capture atoms after
  ATOMS_AFTER=$(fetch_metrics | grep "^twinfoxdb_atoms " | awk '{print $2}')
  TABLE_AFTER=$(fetch_metrics | grep "^twinfoxdb_table_space_used_bytes " | awk '{print $2}')
  echo "  atoms_after=${ATOMS_AFTER} table_space_after=${TABLE_AFTER}"
  echo "  TOTAL: ${TOTAL_MS}ms pass=${TOTAL_PASS} fail=${TOTAL_FAIL}"
  echo "  ---"

  # Save to file
  echo "run=${run} total_ms=${TOTAL_MS} pass=${TOTAL_PASS} fail=${TOTAL_FAIL} atoms_before=${ATOMS_BEFORE} atoms_after=${ATOMS_AFTER} table_before=${TABLE_BEFORE} table_after=${TABLE_AFTER}" >> "/tmp/perf_${LABEL}_summary.log"
done

# Stop server
"$SERVER_SCRIPT" stop 2>/dev/null || true
