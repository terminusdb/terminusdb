#!/bin/bash
# ALICE harness entrypoint — runs inside the terminusdb-acid-alice container.
#
#   1. `store init` populates the workload directory untraced: ALICE treats it
#      as the base state every reconstructed crash state is built from.
#   2. `alice-record` traces the commit workload's syscalls (with write
#      payloads) against that directory.
#   3. `alice-check` enumerates the legal crash states and runs checker.py
#      on each one.
set -euo pipefail

WORK=${ACID_WORK:-/acid-work}
WL=$WORK/workload_dir
TRACES=$WORK/traces_dir

# traces_dir may be a bind mount — clean contents, don't remove the dir.
rm -rf "$WL"
mkdir -p "$WL" "$TRACES"
rm -rf "$TRACES"/* 2>/dev/null || true

echo "==> store init (untraced base state)"
TERMINUSDB_SERVER_DB_PATH="$WL" /app/terminusdb/terminusdb store init --key root

echo "==> alice-record"
alice-record --workload_dir "$WL" --traces_dir "$TRACES" /acid/workload.sh "$WL"

echo "==> alice-check"
alice-check --traces_dir="$TRACES" --checker=/acid/checker.py

echo "==> ALICE run complete; trace artefacts in $TRACES"
