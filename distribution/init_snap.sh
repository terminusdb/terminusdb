#!/bin/bash
export SWI_HOME_DIR="$SNAP/usr/lib/swi-prolog"
export TERMINUSDB_SERVER_PACK_DIR="$SNAP"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}${SWI_HOME_DIR}/lib/x86_64-linux/"

# Runtime check: Verify HTTP extensions are present
HTML_WRITE_PATH="$SWI_HOME_DIR/library/ext/http/http/html_write.pl"
if [ ! -f "$HTML_WRITE_PATH" ]; then
    echo "ERROR: HTML extensions missing in snap!" >&2
    echo "Expected: $HTML_WRITE_PATH" >&2
    echo "Available in library:" >&2
    ls -la "$SWI_HOME_DIR/library/" 2>&1 | head -20 >&2
    echo "" >&2
    echo "Checking ext directory:" >&2
    ls -la "$SWI_HOME_DIR/library/ext/" 2>&1 | head -20 >&2
    echo "" >&2
    echo "This indicates swi-prolog package is missing HTTP extensions." >&2
    echo "Build may have used swi-prolog-nox instead of swi-prolog." >&2
fi

# Set default storage location if not already set (allows override)
# Use SNAP_USER_COMMON for per-user storage (no sudo needed)
if [ -z "$TERMINUSDB_SERVER_DB_PATH" ]; then
    export TERMINUSDB_SERVER_DB_PATH="$SNAP_USER_COMMON/storage"
fi

# Silent pack validation - only report errors
if [ ! -d "$SNAP/tus" ] || [ ! -f "$SNAP/tus/pack.pl" ]; then
    echo "ERROR: TUS pack not found or incomplete at $SNAP/tus" >&2
fi
if [ ! -d "$SNAP/jwt_io" ] || [ ! -f "$SNAP/jwt_io/pack.pl" ]; then
    echo "ERROR: JWT pack not found or incomplete at $SNAP/jwt_io" >&2
fi

exec "$SNAP/terminusdb" "$@"
