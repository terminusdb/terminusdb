#!/bin/bash
export SWI_HOME_DIR="$SNAP/usr/lib/swi-prolog"
export TERMINUSDB_SERVER_PACK_DIR="$SNAP"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$SNAP/usr/lib"
$SNAP/terminusdb "$@"
