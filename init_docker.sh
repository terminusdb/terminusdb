#!/bin/sh
SERVER_MODE=${SERVER_MODE:-serve}
SERVER_NAME=${SERVER_NAME:-localhost}
ADMIN_PASS=${ADMIN_PASS:-root}
SERVER_PORT=${SERVER_PORT:-6363}
WORKERS=${WORKERS:-8}
PUBLIC_URL=${PUBLIC_URL:-false}
AUTOATTACH=${AUTOATTACH:-true}
AUTOLOGIN=${AUTOLOGIN:-false}
if [ ! -f /app/terminusdb/storage/prefix.db ]; then
    /app/terminusdb/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL" --autologin="$AUTOLOGIN" --autoattach="$AUTOATTACH"
else
    /app/terminusdb/utils/db_util -s "$SERVER_NAME" -k "$ADMIN_PASS" --port "$SERVER_PORT" --workers "$WORKERS" --public_url "$PUBLIC_URL" --autologin="$AUTOLOGIN" --autoattach="$AUTOATTACH" --only-config
fi

echo "SERVER_MODE $SERVER_MODE"
echo "SERVER_NAME $SERVER_NAME"
echo "SERVER_PORT $SERVER_PORT"
echo "PUBLIC_URL $PUBLIC_URL"
echo "AUTO_ATTACH $AUTOATTACH"
echo "AUTO_LOGIN $AUTOLOGIN"
/app/terminusdb/start.pl "$SERVER_MODE"

