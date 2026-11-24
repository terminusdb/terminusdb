#!/bin/bash
# The traced workload. alice-record traces this process and its children:
# the server performs the commits (its writes/fsyncs/renames inside the
# workload dir are what ALICE models) while driver.py's stdout ACK lines mark
# which commits were acknowledged before each simulated crash.
set -uo pipefail

WL=${1:?usage: workload.sh <workload_dir>}

export TERMINUSDB_SERVER_DB_PATH=$WL
export TERMINUSDB_SERVER_PORT=6363
export TERMINUSDB_SERVER_NAME=127.0.0.1
export TERMINUSDB_ADMIN_PASS=root
export TERMINUSDB_JWT_ENABLED=false
export TERMINUSDB_ENABLE_OPTIMIZER=0
export TERMINUSDB_INDEXER_BACKEND=none
# Small worker pool: fewer threads, fewer interleavings, faster exploration.
export TERMINUSDB_WORKER_POOL_SIZE=${ACID_WORKERS:-4}
export TERMINUSDB_COMMIT_QUEUE_TIMEOUT=5
# Server log lines share this stdout with the driver's ACK markers; keep them
# quiet so the trace stays small. Only `^ACK ` lines are parsed downstream.
export TERMINUSDB_LOG_LEVEL=${TERMINUSDB_LOG_LEVEL:-ERROR}
export TERMINUSDB_PLUGINS_PATH=${TERMINUSDB_PLUGINS_PATH:-/plugins}
export TERMINUSDB_BUILTIN_PLUGINS_PATH=${TERMINUSDB_BUILTIN_PLUGINS_PATH:-/builtin_plugins}

/app/terminusdb/terminusdb serve &
SERVER_PID=$!
trap 'kill $SERVER_PID 2>/dev/null; wait $SERVER_PID 2>/dev/null' EXIT

python3 /acid/driver.py
