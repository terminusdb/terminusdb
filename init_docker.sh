#!/bin/sh
SERVER_NAME=${SERVER_NAME:-localhost}
ADMIN_PASS=${ADMIN_PASS:-root}
SERVER_PORT=${SERVER_PORT:-6363}
WORKERS_AMOUNT=${WORKERS_AMOUNT:-8}
if [ ! -f /app/terminusdb/storage/prefix.db ]; then
    /app/terminusdb/utils/initialize_database -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS_AMOUNT"
fi
/app/terminusdb/start.pl
