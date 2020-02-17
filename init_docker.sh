#!/bin/sh
SERVER_MODE=${SERVER_MODE:-attach}
SERVER_NAME=${SERVER_NAME:-localhost}
ADMIN_PASS=${ADMIN_PASS:-root}
SERVER_PORT=${SERVER_PORT:-6363}
WORKERS=${WORKERS:-8}
PUBLIC_URL=${PUBLIC_URL:-false}
FILE_DIR=${FILE_DIR:-/app/terminusdb}
if [ ! -f "$FILE_DIR"/storage/prefix.db ]; then
    "$FILE_DIR"/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL"
else
    "$FILE_DIR"/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL" --only-config
fi
nohup swipl --no-tty --quiet -f "$FILE_DIR"/start.pl
