#!/bin/sh
# Entrypoint for the test-runner image (tests/Dockerfile.test).
# Starts the server inside the same container the tests run in, so CLI
# invocations are local process spawns rather than `docker exec` calls.

set -e

PORT="${TERMINUSDB_SERVER_PORT:-6363}"

# Server log goes to the bind-mounted /logs volume; the workflow uploads
# it as an artifact like the old docker-logs capture did.
/app/terminusdb/init_docker.sh > /logs/server.log 2>&1 &

i=0
until node -e "fetch('http://127.0.0.1:${PORT}/api/ok').then(r => process.exit(r.status === 200 ? 0 : 1)).catch(() => process.exit(1))"; do
  i=$((i + 1))
  if [ "$i" -ge 120 ]; then
    echo "server did not become ready" >&2
    cat /logs/server.log >&2
    exit 1
  fi
  sleep 1
done

cd /app/terminusdb/tests
npm ci
exec npm run test
