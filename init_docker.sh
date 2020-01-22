#!/bin/sh
SERVER_NAME=${SERVER_NAME:-localhost}
ADMIN_PASS=${ADMIN_PASS:-root}
SERVER_PORT=${SERVER_PORT:-6363}
WORKERS=${WORKERS:-8}
PUBLIC_URL=${PUBLIC_URL:-false}
if [ ! -f /app/terminusdb/storage/prefix.db ]; then
    /app/terminusdb/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL"
fi
if [ ! -f /app/terminusdb/config/config.pl ]; then
    /app/terminusdb/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL" --only-config
fi
/app/terminusdb/start.pl
