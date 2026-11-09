# ACID verification suite

Formal verification of TerminusDB's atomic commit structure using established,
third-party crash-consistency and history-checking tools — not bespoke test
code. Both harnesses run inside Linux containers so the results are
reproducible on any host with Docker.

## What is verified, and by whom

TerminusDB commits through a versioned label CAS: a transaction
writes immutable `.larch` layer files, fsyncs them, then moves each graph
label via tmp-write → fsync → rename → fsync-parent-dir, asserting the
expected label version. Two properties follow: a commit is durable once
acknowledged, and a crash at any syscall boundary must leave the store either
at the old head or the new head — never torn.

| Layer | Tool | What the tool proves | What we supply |
|---|---|---|---|
| `alice/` | [ALICE](https://github.com/banks/alice) (banks fork of madthanu/alice, OSDI'14; used by HashiCorp raft-wal and Dgraph Badger) | Every legal crash state of the store directory is enumerated from a syscall trace with write payloads, then handed to a checker. If the commit protocol requires a filesystem persistence property it doesn't have, ALICE finds the state. | A thin oracle: label/layer structural invariants + a sampled server boot that asserts every acknowledged commit is retrievable. |
| `jepsen/` | [Jepsen](https://github.com/jepsen-io/jepsen) `jepsen.tests.cycle.append` (Elle) | The observed operation history is strictly serializable — the elle checker proves no G0/G1c/G-single/G2 anomalies and no lost acknowledged writes under kill/pause nemeses. | A client that performs append/read transactions through the document API with the `TerminusDB-Data-Version` CAS guard. |

[`ATOMIC_COMMIT.md`](ATOMIC_COMMIT.md) explains the on-disk commit protocol the
suite verifies — including why the store renames over an open (locked) label
fd, which is the mechanism under test, not a bug — and which ALICE findings
each patch in `alice/Dockerfile` exists for.

## Running

```bash
# Crash-consistency (ALICE). Builds the server image first if stale.
make test-acid-alice

# History checking (Jepsen). Builds the server image first if stale.
make test-acid-jepsen

# Both, in sequence.
make test-acid
```

Neither target is part of `make test`, `make test-int`, or `make pr-light`:
ALICE explores a combinatorial state space and Jepsen runs a live chaos
session — both are far too slow for the regular gate. They exist to be run
deliberately against a candidate build.

## Where results land

- `tests/acid/traces/` — ALICE trace output plus every reconstructed crash
  state that failed the checker (`reconstructeddir-*`), including the checker
  output for debugging.
- `tests/acid/jepsen/store/` — Jepsen run artefacts: the full history, elle's
  analysis, and a timeline HTML report.

## Tuning

- `ACID_COMMITS` (default 8) — commits in the ALICE workload. More commits,
  exponentially more crash states.
- `ACID_BOOT_MOD` (default 1) — server-boot check runs on 1/N of the
  reconstructed states that saw at least one acknowledgement. Set higher to
  trade coverage for runtime.
- `ACID_WORKERS` (default 4) — TerminusDB worker pool size inside the traced
  workload. Kept small deliberately: fewer threads, fewer interleavings,
  faster exploration.
- Jepsen: `TIME_LIMIT` (default 120s), `CONCURRENCY` (default 8),
  `MAX_WRITES_PER_KEY` (default 128) — passed through docker-compose.

## Notes and limitations

- ALICE reconstructs *persisted* states, which includes power-loss states no
  process kill can produce. That is exactly why the harness exists — but it
  also means a failure indicates a property the store relies on that the
  filesystem did not guarantee, not necessarily a code bug. Read the failing
  `reconstructeddir-*` before concluding.
- The Jepsen suite runs a single-node deployment: it verifies commit
  atomicity, durability and strict serializability under process kill/pause,
  not replication properties.
- The pinned ALICE revision is recorded in `alice/Dockerfile`
  (`ARG ALICE_REV`). Bump it deliberately.
