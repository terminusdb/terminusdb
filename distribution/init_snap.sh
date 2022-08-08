#!/bin/bash
export SWI_HOME_DIR="$SNAP/usr/lib/swi-prolog"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+LD_LIBRARY_PATH:}${SWI_HOME_DIR}/lib/x86_64-linux/"
$SNAP/terminusdb "$@"
