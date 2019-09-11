#!/bin/sh
SERVER_NAME=${SERVER_NAME:-localhost}
ADMIN_PASS=${ADMIN_PASS:-root}
HTTP_USER=${HTTP_USER:-root}
HTTP_PASS=${HTTP_PASS:-root}
if [ ! -f /app/terminusdb/storage/prefix.db ]; then
    CONFIG_FILE="/app/terminusdb/config/config.pl"
    cp /app/terminusdb/config/config-example.pl $CONFIG_FILE
    sed -i "s/a username/$HTTP_USER/" $CONFIG_FILE
    sed -i "s/A password/$HTTP_PASS/" $CONFIG_FILE
    sed -i "s/localhost/$SERVER_NAME/" $CONFIG_FILE
    /app/terminusdb/utils/initialize_database -s "$SERVER_NAME" -k "$ADMIN_PASS"
fi
/app/terminusdb/start.pl
