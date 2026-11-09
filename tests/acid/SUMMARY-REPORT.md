# ACID Verification Summary

## What exists now

`tests/acid/` contains a reproducible verification suite with two
external, trusted verifiers. Neither verifier is ours; the repo only
supplies workload drivers and thin oracles.

| Tool | Checks | Method |
|------|--------|--------|
| ALICE (`banks/alice`, pinned `8feae75`) | Atomicity, durability at syscall level | Reconstructs every legal crash state of the store and runs a checker on each |
| Jepsen + Elle | Isolation, strict serializability, lost updates | `append` workload under `kill -9`/`SIGSTOP` nemesis, history checked by `jepsen.tests.cycle.append` |

Run: `make test-acid-alice`, `make test-acid-jepsen`, `make test-acid`.
Opt-in only — not part of `make test` or `make pr-light`. Both build
`terminusdb-server:local` from the current tree first.

## What changed on this branch

Before: a commit moved the label head unconditionally. Concurrent
commits raced; the last writer won silently, so an acknowledged commit
could overwrite another acknowledged commit's head move.

Now: the commit asserts the expected label version at commit time.

- `assert_data_version_at_commit/2` records the version inside the
  meta-commit lock; `nb_force_set_head/3` performs the versioned CAS.
- Store protocol per commit: write temp label, fsync file, rename over
  destination (destination descriptor stays open — it is the lock
  anchor), fsync parent directory.
- CAS loss surfaces as `api:BadDataVersion`/`api:DataVersionMismatch`
  (400) or `data_version_cas_lost` (500); clients retry.
- The `TerminusDB-Data-Version` header is the client-side handle for
  the CAS guard.

The suite exists to prove this structure under real crash and
concurrency conditions — syscall-level (ALICE) and history-level
(Jepsen).

## Findings

1. **ALICE: clean.** >12,000 reconstructed crash states, zero checker
   failures. The commit protocol's file ordering (layer durable before
   label, temp-write fsync before rename, parent-dir fsync) held in
   every state ALICE could build.

2. **Jepsen: real anomaly found.** Two runs report `:valid? false`
   (`:G-nonadjacent-item-realtime`, `:lost-update`). Reads answered
   `404`/empty for committed documents inside the restart-recovery
   window. Root cause: the `store` readiness component covers store
   open, not user-database resolvability — requests dispatch before a
   user db can serve, and `create=true` upserts built on phantom-empty
   reads discard acked values. Filed as
   `ISSUE_USER_DB_READINESS.md`.

3. **Durability verified independently.** 253/253 acked values present
   after `kill -9` mid-churn; startup gate answers only
   refused/`503`-equivalent or correct data on the Rust backend.

## Net assessment

The atomic commit mechanism itself survives every fault the suite can
inject: no lost acked commits, no torn label states, correct CAS
behaviour under contention. The defect found is in the *startup
readiness contract*: user databases must be resolvable before
`server_ready` — or before handlers serve — and today they are not,
in a narrow window after process restart.
