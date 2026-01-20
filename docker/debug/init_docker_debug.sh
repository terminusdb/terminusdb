#!/usr/bin/env bash
# Debug init script - runs TerminusDB with chk_secure enabled
# This catches term reference issues before they cause GC crashes

TERMINUSDB_SERVER_PORT=${TERMINUSDB_SERVER_PORT:-6363}

file_env() {
	local var="$1"
	local fileVar="${var}_FILE"
	local def="${2:-}"
	if [ "${!var:-}" ] && [ "${!fileVar:-}" ]; then
		echo >&2 "error: both $var and $fileVar are set (but are exclusive)"
		exit 1
	fi
	local val="$def"
	if [ "${!var:-}" ]; then
		val="${!var}"
	elif [ "${!fileVar:-}" ]; then
		val="$(< "${!fileVar}")"
	fi
	export "$var"="$val"
	unset "$fileVar"
}
file_env 'TERMINUSDB_ADMIN_PASS'
TERMINUSDB_ADMIN_PASS=${TERMINUSDB_ADMIN_PASS:-root}

if [ ! -d /app/terminusdb/storage/db ]; then
    /app/terminusdb/terminusdb store init --key "$TERMINUSDB_ADMIN_PASS"
fi

echo "SERVER_PORT $TERMINUSDB_SERVER_PORT"
echo "🔍 Debug build with TERMINUSDB_CHK_SECURE=${TERMINUSDB_CHK_SECURE:-false}"

# chk_secure is enabled via TERMINUSDB_CHK_SECURE environment variable
# which is checked at runtime in src/start.pl
exec /app/terminusdb/terminusdb serve
