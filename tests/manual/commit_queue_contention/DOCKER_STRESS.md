# Docker CPU-throttled stress run

This manual test runs the commit-queue contention stress test against a
TerminusDB server in a CPU-throttled Docker container. The container is
limited to half a CPU or less so commits and scheduled auto-optimizations 
overlap heavily, which is the scenario in which the original race was 
observed.

## What it tests

The race that was fixed: a document commit and a scheduled `_meta` optimization
both try to use the same `commit_graph` builder. Without the
`meta_commit_lock` serialization, the optimizer can commit the builder while
the next commit is still using it, producing
`rust_io_error(InvalidData, builder has already been committed)` or an HTTP 500.

The test interleaves document inserts, branch creation, rebase, push, and explicit
`/api/optimize` calls on a single database while the auto-optimize plugin is
enabled. It then scans the server log for 500 responses, optimization failures,
`builder has already been committed`, and `unexpected_commit_failure`.

## Prerequisites

- Docker installed and running.
- `docker-compose` available.
- The local image `terminusdb/terminusdb-server:local` built, or the ability to
  build it.
- Node.js with the test dependencies installed (`npm install` in `tests/`).

## Quick run

```bash
# From the repo root
make docker SKIP_TESTS=true

# Run the bundled script (defaults to 120 seconds)
cd tests/manual/commit_queue_contention
./run-stress-in-docker.sh

# Or run for a specific duration
./run-stress-in-docker.sh 300
```

The script will:
1. Start the CPU-throttled container on port 6363.
2. Wait for the server to be ready.
3. Run `stress.js` with the configured duration and concurrency.
4. Write `terminusdb-stress.log`.
5. Stop and remove the container and volume.

## Manual run

If you prefer to start the container yourself and run the test separately:

```bash
cd tests/manual/commit_queue_contention

# Start the container
docker-compose -f docker-compose.stress.yml up -d

# Wait for the server to be ready
curl -sf -u admin:root http://127.0.0.1:6363/api/ > /dev/null && echo ready

# Run the stress test
STRESS_DURATION_SECONDS=300 \
  STRESS_CONCURRENCY=4 \
  TERMINUSDB_BASE_URL=http://127.0.0.1:6363 \
  TERMINUSDB_USER=admin \
  TERMINUSDB_PASSWORD=root \
  node stress.js

# Stop and clean up
docker-compose -f docker-compose.stress.yml down -v
```

## Adjusting CPU throttling

Edit `cpus` in `docker-compose.stress.yml`:

```yaml
deploy:
  resources:
    limits:
      cpus: '0.5'
```

Values lower than 0.5 increase contention but may make the container
unresponsive. Values higher than 1.0 reduce overlap.

## Expected result

The test prints operation counts and a log scan. A successful run shows:

```
Log scan:
  HTTP 500 responses: 0
  Optimization failed: 0
  "builder has already been committed": 0
  "unexpected_commit_failure": 0

Result: PASS (no contention artifacts detected in log)
```

## Interpreting failures

If any of the scanned counts are non-zero:
1. Inspect `terminusdb-stress.log` for the failing request.
2. Look for surrounding `commit_graph` / optimization messages.
3. Check whether the failure correlates with the CPU limit or concurrency.

## Clean-up

Both the script and the manual commands use `down -v`, which removes the
container and the named volume. If you interrupted the script, clean up with:

```bash
docker-compose -f docker-compose.stress.yml down -v
```
