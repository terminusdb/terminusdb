#!/bin/bash
export SWI_HOME_DIR="$SNAP/usr/lib/swi-prolog"
export TERMINUSDB_SERVER_PACK_DIR="$SNAP"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}${SWI_HOME_DIR}/lib/x86_64-linux/"

# DEBUG: Check if packs exist (output to stderr to not interfere with commands)
echo "=== Pack Debug Info ===" >&2
echo "SNAP=$SNAP" >&2
echo "TERMINUSDB_SERVER_PACK_DIR=$TERMINUSDB_SERVER_PACK_DIR" >&2
echo "" >&2
if [ -d "$SNAP/tus" ]; then
    echo "✓ TUS pack found at $SNAP/tus" >&2
    ls -la "$SNAP/tus/pack.pl" 2>/dev/null || echo "  WARNING: pack.pl not found!" >&2
else
    echo "✗ ERROR: TUS pack not found at $SNAP/tus" >&2
fi
if [ -d "$SNAP/jwt_io" ]; then
    echo "✓ JWT pack found at $SNAP/jwt_io" >&2
    ls -la "$SNAP/jwt_io/pack.pl" 2>/dev/null || echo "  WARNING: pack.pl not found!" >&2
else
    echo "✗ ERROR: JWT pack not found at $SNAP/jwt_io" >&2
fi
echo "====================" >&2
echo "" >&2

$SNAP/terminusdb "$@"
