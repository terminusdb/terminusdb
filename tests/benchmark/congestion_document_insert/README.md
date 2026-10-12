# Congestion Document Insert Benchmark

A public benchmark for measuring parallel document insert throughput against a
single TerminusDB branch using the `/api/document` endpoint with NDJSON bulk
inserts.

It compares two execution modes:

- **Parallel**: multiple writers insert chunks concurrently on the same branch.
- **Sequential**: the same total number of chunks is inserted one at a time.

The benchmark creates fresh databases, inserts a simple `Simple` schema, runs
the timed insertions, verifies that every returned document ID can be queried
back, and finally deletes the databases. No setup beyond a running TerminusDB
server is required.

## What you need

- Python 3.11 or newer (only the standard library is used).
- A running TerminusDB server at `http://127.0.0.1:6363` with the default
  `admin` / `root` credentials.

If you do not have a global Python 3 installation, the steps below create a
local virtual environment.

## Quick start with a virtual environment

From the benchmark directory:

```bash
cd /Users/hoijnet/Code/devops/twinfoxdb/terminusdb/tests/benchmark/congestion_document_insert

# Create a virtual environment (only needed once)
python3 -m venv .venv

# Activate it
source .venv/bin/activate

# Run the benchmark with the default settings
python3 bench.py
```

On macOS, `python3` is usually available from the system or Homebrew. The
benchmark does not require installing any third-party packages.

## Start the TerminusDB server

The benchmark expects a server running on the default local test port. The
repository provides a helper script:

```bash
cd /Users/hoijnet/Code/devops/twinfoxdb/terminusdb
./tests/terminusdb-test-server.sh start --clean
```

This starts a fresh, isolated server on `http://127.0.0.1:6363` with
admin password `root`. Use `--clean` the first time or whenever you want to
wipe previous test data.

## Default configuration

The default run is:

- 4 writers
- 3 chunks per writer
- 2 000 documents per chunk
- 24 000 documents total

This is the stable configuration that produced the best throughput in our
tests. It keeps the per-chunk size small enough to avoid the commit-queue
hangs and worker starvation seen with larger chunks, while still amortizing
the per-commit fixed cost over many documents.

## Override the configuration

Use environment variables to change the load:

```bash
WRITERS=2 CHUNKS_PER_WRITER=2 DOCS_PER_CHUNK=20000 \
  python3 bench.py
```

Available variables:

| Variable | Default | Description |
|---|---|---|
| `TERMINUSDB_HOST` | `http://127.0.0.1:6363` | Server URL |
| `TERMINUSDB_USER` | `admin` | Admin user |
| `TERMINUSDB_PASS` | `root` | Admin password |
| `WRITERS` | `4` | Number of concurrent writers |
| `CHUNKS_PER_WRITER` | `3` | Chunks each writer sends |
| `DOCS_PER_CHUNK` | `2000` | Documents per NDJSON chunk |
| `OUTPUT` | `../benchmark_results/congestion_document_insert.json` | Results file |

## Example output

```json
{
  "benchmark": "congestion_document_insert",
  "metrics": {
    "writers": 2,
    "chunks_per_writer": 2,
    "docs_per_chunk": 20000,
    "parallel": {
      "total_ms": 3280.27,
      "documents": 80000,
      "chunks": 4,
      "documents_per_second": 24388.20,
      "ms_per_document": 0.0410,
      "verified_documents": 80000
    },
    "sequential": {
      "total_ms": 4085.87,
      "documents": 80000,
      "chunks": 4,
      "documents_per_second": 19579.67,
      "ms_per_document": 0.0511,
      "verified_documents": 80000
    },
    "congestion_ratio": 0.80
  }
}
```

## Interpreting results

- **`documents_per_second`** under `parallel` is the headline throughput number.
- **`congestion_ratio`** is `parallel_total_ms / sequential_total_ms`. A value
  close to 1 means parallel writes on the same branch add little overhead; a
  much larger value would mean contention is dominating the runtime.
- **`verified_documents`** is the number of inserted IDs that were successfully
  queried back from the database. It must equal `documents`; if it is lower, the
  benchmark raises an error because the insert response reported documents that
  were not persisted.
- On a single branch, TerminusDB serializes commits, so adding more writers
  does not always increase throughput. The bottleneck is the per-commit fixed
  cost.

## Notes

- The script uses only Python standard library modules (`http.client`, etc.).
- The `Simple` schema uses a `Random` key, so the server generates document IDs.
  This exercises the Rust fast path for simple documents and keeps the workload
  non-intersecting (no transaction retries).
- The benchmark creates and deletes the databases `admin/congestion_document_insert_parallel`
  and `admin/congestion_document_insert_sequential`.
