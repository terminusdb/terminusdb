#!/usr/bin/env bash
set -euo pipefail

# Build the local Docker image and run the stress test against a CPU-throttled
# TerminusDB container. The container is configured to use the auto-optimize
# plugin and exposes the server on port 6363.
#
# Usage:
#   cd tests/manual/commit_queue_contention
#   ./run-stress-in-docker.sh [STRESS_DURATION_SECONDS]

DURATION="${1:-120}"
IMAGE="terminusdb/terminusdb-server:local"

REPO_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"

if ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "Local image $IMAGE not found. Building it now..."
  docker build "$REPO_ROOT" \
    --file "$REPO_ROOT/Dockerfile" \
    --tag "$IMAGE" \
    --build-arg SWIPL_VERSION="${SWIPL_VERSION:-10.0.1}" \
    --build-arg SKIP_TESTS=true \
    --build-arg DIST=community \
    --build-arg TERMINUSDB_GIT_HASH="$(cd "$REPO_ROOT" && git rev-parse --verify HEAD)"
fi

echo "Starting CPU-throttled TerminusDB container..."
docker-compose -f docker-compose.stress.yml down -v 2>/dev/null || true
docker-compose -f docker-compose.stress.yml up -d

# Wait for the server to be ready.
echo "Waiting for server..."
for i in $(seq 1 60); do
  if curl -sf -u admin:root http://127.0.0.1:6363/api/ > /dev/null 2>&1; then
    break
  fi
  sleep 1
done

if ! curl -sf -u admin:root http://127.0.0.1:6363/api/ > /dev/null 2>&1; then
  echo "Server did not become ready in time."
  docker-compose -f docker-compose.stress.yml logs
  docker-compose -f docker-compose.stress.yml down -v
  exit 1
fi

echo "Running stress test for ${DURATION}s..."
set +e
STRESS_DURATION_SECONDS="$DURATION" \
  STRESS_CONCURRENCY=4 \
  TERMINUSDB_BASE_URL=http://127.0.0.1:6363 \
  TERMINUSDB_USER=admin \
  TERMINUSDB_PASSWORD=root \
  node stress.js
RESULT=$?
set -e

echo "Collecting container logs..."
docker-compose -f docker-compose.stress.yml logs > terminusdb-stress.log

HTTP_500_COUNT=$(grep -cE '\(500\)' terminusdb-stress.log || true)
OPTIMIZE_FAILED_COUNT=$(grep -icE 'Optimization of .* failed' terminusdb-stress.log || true)
BUILDER_COMMITTED_COUNT=$(grep -cE 'builder has already been committed' terminusdb-stress.log || true)
UNEXPECTED_COMMIT_COUNT=$(grep -cE 'unexpected_commit_failure' terminusdb-stress.log || true)

echo ""
echo "Container log scan:"
echo "  HTTP 500 responses: ${HTTP_500_COUNT}"
echo "  Optimization failed: ${OPTIMIZE_FAILED_COUNT}"
echo "  \"builder has already been committed\": ${BUILDER_COMMITTED_COUNT}"
echo "  \"unexpected_commit_failure\": ${UNEXPECTED_COMMIT_COUNT}"
echo ""

if [ "$OPTIMIZE_FAILED_COUNT" -gt 0 ] || [ "$BUILDER_COMMITTED_COUNT" -gt 0 ] || [ "$UNEXPECTED_COMMIT_COUNT" -gt 0 ]; then
  echo "Result: FAIL (race-condition artifacts detected in container log)"
  RESULT=1
elif [ "$RESULT" -ne 0 ]; then
  echo "Result: FAIL (stress test reported unexpected operation failures)"
else
  echo "Result: PASS (no race-condition artifacts detected in container log)"
fi

echo "Stopping container..."
docker-compose -f docker-compose.stress.yml down -v

exit $RESULT
