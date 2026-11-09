# The atomic commit under test

This suite exists to verify one mechanism: TerminusDB's atomic commit path —
a versioned label compare-and-swap (improved under #2637: "Assert
TerminusDB-Data-Version at commit time"). This document explains what the
protocol does on disk, because the test harness deliberately encodes
knowledge of it — and because the same mechanism explains several of the
ALICE parser patches in `alice/Dockerfile`.

## What a commit looks like on disk

Every commit in the traced workload (`alice/workload.sh` + `driver.py`) emits
this syscall pattern into the workload dir, verified verbatim in the ALICE
micro-op trace:

```
mkdir <3-hex>/                          # layer directory
creat <3-hex>/<40-hex>.larch            # new layer file
append <3-hex>/<40-hex>.larch           # full payload
fsync <3-hex>/<40-hex>.larch            # data durable
fsync <3-hex>                           # directory entry durable
creat .tmp/<name>.label                 # new label, staged
append .tmp/<name>.label                # "version\n<layer-id>" (43 bytes)
fsync .tmp/<name>.label
rename .tmp/<name>.label -> <name>.label  # atomic commit point
fsync <workload-dir>                    # rename durable
stdout("ACK <name> ...")                # the acknowledgement
```

The single `rename` is the commit point: before it the label still names the
old layer; after it the label names a fully-written, fsynced layer. A crash
anywhere else leaves the old label intact — the commit either happened
completely or not at all.

## Why the label is renamed over an _open_ file

The store does not close the old label file before renaming over it. That fd
is not an oversight — it is the lock.

`force_set_head_version` opens the existing label file under lock, holds it as the
synchronisation token, and re-reads the label _by path_ after the rename. A
rename moves the path to a new inode, so a holder of the old fd observes the
old inode — exactly the CAS semantics needed: if the path's inode changed
between "read version" and "write", someone else committed and the update is
rejected (`api:DataVersionMismatch` / `data_version_cas_lost`).

Consequence for the harness: **we must not close the fd to make the test
pass**. Closing it would release the lock mid-CAS and hide precisely the
window where a lost update or a crash could corrupt the label. ALICE's parser
asserted that a rename destination has no open fds — a modelling limitation
for this protocol — so the Dockerfile relaxes that assert to a warning and
`checker.py` skips the `.deleted_<uuid>` artefact files ALICE leaves to model
the unlinked inode.

## What each tool proves

| Property              | Verifier                     | What it checks                                                                                                                                                                                      |
| --------------------- | ---------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Atomicity, Durability | ALICE (`banks/alice`)        | Reconstructs every legal crash state of the workload dir; `checker.py` asserts label well-formedness, layer-file existence/integrity, and that sampled states boot and contain all `ACK`ed commits. |
| Consistency           | ALICE + Jepsen               | No torn labels; all reconstructed states are bootable stores or cleanly earlier states.                                                                                                             |
| Isolation             | Jepsen (`append/test`, Elle) | Strict-serializability of concurrent guarded writes through `TerminusDB-Data-Version`; kill/restart nemesis exercises recovery.                                                                     |

ALICE proves syscall-level crash consistency — that the on-disk invariants
hold at every legal crash point. Jepsen proves API-level history correctness.
Neither is asked to do the other's job.

## Why the ALICE fork is patched in the image

The harness patches `banks/alice` inside `alice/Dockerfile`. Every patch is
an _environment_ fix (running a 2014-era tool on aarch64/bookworm), not a
semantic weakening — the crash-state enumeration and reconstruction are
untouched:

- `alice-strace` compiles on aarch64 (empty stack-trace frames; attribution
  degrades, checking does not).
- `syscall_*` (rseq, getrandom, statx, clone3…) ignored — unknown-numbered
  syscalls that never touch the workload dir.
- `prlimit64` & co. added to `innocent_syscalls` — no disk effect.
- Failed `ioctl`s (ENOTTY isatty probes) skipped — a failed ioctl mutates
  nothing.
- `mkdirat`/`linkat`/`F_DUPFD_CLOEXEC` mapped to their base forms — same
  treatment the fork already gives `renameat`/`unlinkat`.
- Rename-over-watched-dest downgraded to a warning — required by the lock-fd
  protocol above; the rename op is still recorded and reconstructed.
- `python2`'s `_hashlib` gets buster's `libssl.so.1.1` — bookworm dropped it.
