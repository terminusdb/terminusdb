# Commit-queue contention stress test

Manual test that runs concurrent document inserts, branch creation, rebase,
push, and explicit optimize calls against a single database while the
auto-optimize plugin is enabled. It is intended to detect 500 errors caused by
races between the commit funnel and scheduled optimizations.

## Requirements

- A running TerminusDB server (test server is easiest).
- The auto-optimize plugin enabled (`TERMINUSDB_PLUGINS_PATH`).
- Node.js with the test dependencies installed (`npm install` in `tests/`).

## Run

### Quick structural test (local test server)

```bash
# From the terminusdb repo root
TERMINUSDB_PLUGINS_PATH="/path/to/terminusdb/docker/plugins" \
  ./tests/terminusdb-test-server.sh start --clean

# In another terminal, from the tests/ directory
cd tests
node manual/commit_queue_contention/stress.js
```

### CPU-throttled run in Docker

This is the run that most closely reproduces the original race. The container
is limited to half a CPU, so commits and scheduled optimizations overlap
heavily.

```bash
# From the repo root
make docker SKIP_TESTS=true

# Run the bundled script (defaults to 120 seconds)
cd tests/manual/commit_queue_contention
./run-stress-in-docker.sh

# Or run for a specific duration
./run-stress-in-docker.sh 300
```

The script starts a container on port 6363, waits for the server, runs the
stress test, writes `terminusdb-stress.log`, and stops the container.

You can also manage the container manually:

```bash
cd tests/manual/commit_queue_contention

# Start the throttled container
docker-compose -f docker-compose.stress.yml up -d

# Wait for the server to be ready, then run the test
STRESS_DURATION_SECONDS=300 \
  STRESS_CONCURRENCY=4 \
  TERMINUSDB_BASE_URL=http://127.0.0.1:6363 \
  TERMINUSDB_USER=admin \
  TERMINUSDB_PASSWORD=root \
  node stress.js

# Stop and clean up
docker-compose -f docker-compose.stress.yml down -v
```

To change the CPU limit, edit `cpus` in `docker-compose.stress.yml`.

## Environment variables

| Variable | Default | Description |
| --- | --- | --- |
| `TERMINUSDB_BASE_URL` | `http://127.0.0.1:6363` | Server URL. |
| `TERMINUSDB_USER` | `admin` | Basic-auth user. |
| `TERMINUSDB_PASSWORD` | `root` | Basic-auth password. |
| `TERMINUSDB_LOG` | `tests/.terminusdb-test.log` | Server log to scan. |
| `STRESS_DURATION_SECONDS` | `60` | How long to run the workload. |
| `STRESS_CONCURRENCY` | `4` | Number of concurrent workers. |

## What it does

- Creates a fresh database with a simple `StressDoc` schema.
- Starts `STRESS_CONCURRENCY` workers that each loop for the configured
  duration.
- Each worker picks a random operation:
  - Insert a document into `main` or one of four feature branches.
  - Create a feature branch from `main`.
  - Rebase a feature branch onto `main`.
  - Push the current state to a feature branch.
  - Explicitly optimize a feature branch via `/api/optimize`.
- Counts successful operations and expected 4xx failures.
- Scans the server log for 500 responses, failed optimizations,
  `builder has already been committed`, and `unexpected_commit_failure`.

## Interpreting results

- **PASS**: No 500 responses, no optimization failures, no
  `builder has already been committed`, and no `unexpected_commit_failure` in
  the log. The test ran successfully under contention.
- **FAIL**: One of the above artifacts appears. The log should be inspected for
  the failing request and surrounding commit/optimization messages.

## Notes

- Rebase and push often fail with 4xx because the random branch may not have
  divergent history. Those failures are counted but do not fail the test.
- The test does not apply CPU throttling itself. To maximize overlap, run it
  under `cpulimit`/`cputhrottle` or on a loaded machine.
