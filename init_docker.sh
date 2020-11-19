#!/bin/sh
TERMINUSDB_ADMIN_PASS=${TERMINUSDB_ADMIN_PASS:-root}
TERMINUSDB_SERVER_PORT=${TERMINUSDB_SERVER_PORT:-6363}
TERMINUSDB_AUTOLOGIN=${TERMINUSDB_AUTOLOGIN:-false}

if [ ! -d /app/terminusdb/storage/db ] && [ "$TERMINUSDB_ENABLE_WELCOME_SCREEN" = false ]; then
    /app/terminusdb/start.pl store init --key "$TERMINUSDB_ADMIN_PASS"
fi

echo "SERVER_PORT $TERMINUSDB_SERVER_PORT"
/app/terminusdb/terminusdb serve

