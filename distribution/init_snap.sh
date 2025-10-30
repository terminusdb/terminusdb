#!/bin/bash
export SWI_HOME_DIR="$SNAP/usr/lib/swi-prolog"
export TERMINUSDB_SERVER_PACK_DIR="$SNAP"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}${SWI_HOME_DIR}/lib/x86_64-linux/"

# Set default storage location if not already set (allows override)
if [ -z "$TERMINUSDB_SERVER_DB_PATH" ]; then
    export TERMINUSDB_SERVER_DB_PATH="$SNAP_DATA/storage"
fi

# Silent pack validation - only report errors
if [ ! -d "$SNAP/tus" ] || [ ! -f "$SNAP/tus/pack.pl" ]; then
    echo "ERROR: TUS pack not found or incomplete at $SNAP/tus" >&2
fi
if [ ! -d "$SNAP/jwt_io" ] || [ ! -f "$SNAP/jwt_io/pack.pl" ]; then
    echo "ERROR: JWT pack not found or incomplete at $SNAP/jwt_io" >&2
fi

$SNAP/terminusdb "$@"
