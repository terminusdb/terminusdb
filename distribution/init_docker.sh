#!/usr/bin/env bash
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

if [ -n "${TERMINUSDB_ADMIN_SET_PASS:-}" ]; then
    /app/terminusdb/terminusdb user password admin --password "$TERMINUSDB_ADMIN_SET_PASS" || exit 1
fi

exec /app/terminusdb/terminusdb serve

