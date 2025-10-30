#!/bin/bash
export SWI_HOME_DIR="$SNAP/usr/lib/swi-prolog"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}${SWI_HOME_DIR}/lib/x86_64-linux/"

# DEBUG: Check if TUS exists
if [ -d "$SNAP/tus" ]; then
    echo "TUS pack found at $SNAP/tus"
    ls -la "$SNAP/tus/"
else
    echo "ERROR: TUS pack not found at $SNAP/tus"
fi

$SNAP/terminusdb "$@"
