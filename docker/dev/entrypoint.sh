#!/bin/sh
# Copies the read-only checkout at /src into the /app/rust volume, so build
# products (Linux binary, dylib, cargo target, node_modules) stay in the
# container volumes and never overwrite the host build.
set -eu
rsync -a --delete \
  --exclude /terminusdb \
  --exclude '/src/rust/librust.*' \
  --exclude /src/rust/.last-dist \
  --exclude target/ \
  --exclude node_modules/ \
  --exclude /.deps/ \
  --exclude '/plugins/*.so' \
  --exclude '/plugins/*.dylib' \
  --exclude '/tests/.terminusdb-*' \
  /src/ /app/rust/
[ -d /app/rust/tests/node_modules/mocha ] || (cd /app/rust/tests && npm ci --no-audit --no-fund)
exec "$@"
