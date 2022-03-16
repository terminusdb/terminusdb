#!/usr/bin/env bash

# This is a script used to test the command-line interface (CLI).
# It should be run in this directory.

# 1. Try to use the locally built executable. This is for development.
# 2. Try to use the Docker image. This is for continuous integration.

# We use `set -x` to show the executed command if the output is a terminal.
# Don't show it if the script is being run by the tests, because the tests
# expect certain output.

if [[ -x "../terminusdb" ]]; then
  set -e
  if [ -t 1 ]; then
    set -x
  fi
  ../terminusdb "$@"
elif docker image inspect terminusdb/terminusdb-server:local > /dev/null; then
  user="$(id -u):$(id -g)"
  set -e
  if [ -t 1 ]; then
    set -x
  fi
  docker run \
    --rm \
    --user $user \
    --volume $PWD:/app/terminusdb/tests \
    --workdir /app/terminusdb/tests \
    terminusdb/terminusdb-server:local \
    /app/terminusdb/terminusdb \
    "$@"
else
  echo "Error! I'm not sure how to run the CLI (terminusdb)."
  exit -1
fi
