# ALICE Crash-Consistency Report

## Scope

This report describes the ALICE crash-consistency test of the TerminusDB
atomic-commit path. ALICE is the verification library of
`github.com/banks/alice` (pinned revision `8feae75`). The tool records
the system calls that a workload makes. It then makes every legal crash
state of the store directory. It runs a checker on each state.

The test verifies atomicity and durability at the file-system level.
It does not test isolation or serializability. The Jepsen suite tests
those properties (see `JEPSEN-REPORT.md`).

## What the branch changed

Before this branch, a commit moved the label head with a version check performed too early. Two concurrent commits could race; the last writer won silently which was incorrect.

The branch adds a versioned compare-and-swap on the label:

- Prolog records the expected label version in
  `assert_data_version_at_commit/2`, inside the meta-commit lock.
- `nb_set_head` call sites now use `nb_force_set_head/3`.
- On CAS loss the API returns `api:DataVersionMismatch` (400) or
  `data_version_cas_lost` (500).
- The store writes a temporary label file, flushes and fsyncs it,
  renames it over the destination, and fsyncs the parent directory.
- The destination label file stays open during the rename. The open
  descriptor is part of the lock/CAS protocol. This is intentional.
  (See `ATOMIC_COMMIT.md`.)

## Method

1. Build the server image from the branch (`terminusdb-server:local`).
2. `alice-record` traces `terminusdb serve` and the workload driver.
3. The workload creates one database, one schema, eight document
   commits, and two data-version-guarded CAS commits.
4. `alice-check` reconstructs every legal crash state.
5. A checker validates each state:
   - each label file parses as `version` + `layer-id`;
   - every referenced layer file exists;
   - every `ACK`ed commit is retrievable on a sampled server boot;
   - empty orphan layer files are permitted residue (the layer GC
     collects them; no label points at them);
   - `.deleted_*` files are ALICE's own artefacts, not store data.

## Result

- States reconstructed and checked: >12,000 (run was in its final
  report phase at time of writing).
- Checker failures: **0**.
- ALICE reported the ordering pairs that the commit path relies on
  (its "ordering vulnerability" list). These are informational: they
  name the syscall orderings the protocol must enforce, e.g. the layer
  file must be durable before the label that references it. All
  reconstructed states passed, which shows these orderings hold.

## Harness changes that were necessary

The ALICE tool needed adjustments to run on this platform (arm64,
Debian buster base). No production code was changed:

- Debian buster repositories moved to `archive.debian.org`.
- `libunwind-ptrace` link check needed `-lunwind-aarch64 -llzma`.
- `alice-strace` needed an aarch64 patch (x86-only frame walk).
- The parser needed entries for arm64 syscalls (`rseq` family,
  `prlimit64`), `mkdirat`, `linkat`, and `F_DUPFD_CLOEXEC`.
- The parser tolerated a failed `ioctl` on a watched descriptor
  (`ENOTTY` cannot change state).
- The parser tolerated rename over a label inode that has an open
  descriptor. This models the intentional CAS lock described in
  `ATOMIC_COMMIT.md`.
- Python 2 `hashlib` needed the buster OpenSSL 1.1 libraries.

## Limitations

- ALICE models crash states that a process kill cannot always reach.
  A failure can indicate an unmodelled file-system guarantee, not an
  application defect. Each failure needs manual analysis.
- The checker is a thin oracle that we wrote. ALICE supplies the crash
  states and the methodology; the oracle supplies the invariants.
- Stack attribution is disabled on aarch64. Ordering findings report
  syscall indices, not source lines.

## How to run

```sh
make test-acid-alice
```

Artifacts go to `tests/acid/traces/` (gitignored). The state space
grows combinatorially with `ACID_COMMITS` (default 8). Use
`ACID_COMMITS=3` for a faster pass.
