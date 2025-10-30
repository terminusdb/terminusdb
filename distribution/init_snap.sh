#!/bin/bash
export SWI_HOME_DIR="$SNAP/usr/lib/swi-prolog"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}${SWI_HOME_DIR}/lib/x86_64-linux/"

# DEBUG: Check if packs exist
echo "Checking for required packs in $SNAP..."
if [ -d "$SNAP/tus" ]; then
    echo "✓ TUS pack found at $SNAP/tus"
else
    echo "✗ ERROR: TUS pack not found at $SNAP/tus"
fi
if [ -d "$SNAP/jwt_io" ]; then
    echo "✓ JWT pack found at $SNAP/jwt_io"
else
    echo "✗ ERROR: JWT pack not found at $SNAP/jwt_io"
fi

$SNAP/terminusdb "$@"
