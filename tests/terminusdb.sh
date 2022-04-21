#!/usr/bin/env bash

# This is a script used to test the command-line interface (CLI).
# It should be run in this directory.

# We use `set -x` to show the executed command if the output is a terminal.
# Don't show it if the script is being run by the tests, because the tests
# expect certain output.

# Determine whether the $TERMINUSDB_DOCKER_IMAGE_TAG was passed and we should
# use Docker or whether we have a valid executable path and we should use it.
[[ "${TERMINUSDB_DOCKER_IMAGE_TAG:-x}" == "x" ]] && use_docker=1 || use_docker=0
[[ -x "${TERMINUSDB_EXEC_PATH:="../terminusdb"}" ]] && use_exec=0 || use_exec=1

# If neither Docker nor executable, error.
if [[ $use_docker -ne 0 && $use_exec -ne 0 ]]; then
  echo "Error! Missing \$TERMINUSDB_DOCKER_IMAGE_TAG or executable ($TERMINUSDB_EXEC_PATH)."
  exit -1
fi

if [[ $use_docker -eq 0 ]]; then
  # Use the Docker image.
  if docker image inspect "$TERMINUSDB_DOCKER_IMAGE_TAG" &> /dev/null; then
    user="$(id -u):$(id -g)"
    set -e
    if [ -t 1 ]; then
      set -x
    fi
    docker run \
      --rm \
      --user $user \
      --volume $PWD:/app/terminusdb/tests \
      --env TERMINUSDB_SERVER_DB_PATH=$TERMINUSDB_SERVER_DB_PATH
      --workdir /app/terminusdb/tests \
      "$TERMINUSDB_DOCKER_IMAGE_TAG" \
      /app/terminusdb/terminusdb \
      "$@"
  else
    echo "Error! \$TERMINUSDB_DOCKER_IMAGE_TAG does not have a valid image: $TERMINUSDB_DOCKER_IMAGE_TAG"
    exit -1
  fi
elif [[ $use_exec -eq 0 ]]; then
  # Use the locally built executable.
  set -e
  if [ -t 1 ]; then
    set -x
  fi
  "$TERMINUSDB_EXEC_PATH" "$@"
fi
