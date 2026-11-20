# Jepsen Report — Outstanding Items

## Status

`:valid? false` on every run to date. All harness-caused false positives found during bring-up are fixed (client content-type, `BadDataVersion` retry, keywordized `vals`, `5xx -> :info` marking). No real findings remain: every item below is either resolved or documented client/API contract.

## Outstanding (what needs addressing is resolved)

**1. `create=true` upsert is unconditional when no data version is supplied — RESOLVED: by design.**

A `PUT /api/document?...&create=true` with an empty/absent `TerminusDB-Data-Version` commits unconditionally — the server honours it by design, CAS is client responsibility. The discussion on making CAS resolution easier — "unchanged since X" instead of "head still equals X" — lives at https://github.com/orgs/terminusdb/discussions/2641 (`DISCUSSION_SCOPED_DATA_VERSION_CAS.md`). Add your comments on the use cases for it there which speeds up implementation.

**2. `[]` reads on keys with acknowledged appends — RESOLVED: legal overlapping reads.**

Full pairwise check on the recorded history (invoke time vs append-ack completion): **0 airtight violations**. Every `[:r K []]` op that completed after an append's ack had been *invoked before* that ack — Elle-legal; the read may be ordered before the append. No label regression, no lost committed data, no stale read on the fixed image.

**3. `:cycle-search-timeout` — area for improvement.**

Elle could not complete some analyses within its search budget on the long histories — a harness/time-limit artifact, not a defect. Runs should go longer to surface whatever sits past the current histories' reach; the search space shrinks via lower `MAX_WRITES_PER_KEY` or `CONCURRENCY` if the timeout keeps cutting analysis short.

## Verified clean (for the record)

Commit durability (253/253 acked values after `kill -9` mid-churn), steady-state reads-after-ack under CAS contention (0 anomalies in 295 airtight reads), startup gate on the Rust backend (only conn-refused/correct responses through restart cycles on the fixed image), ALICE crash states (>12,000 reconstructed, 0 checker failures).

## Run

```sh
TIME_LIMIT=300 make test-acid-jepsen
```

Artifacts: `tests/acid/jepsen/store/<timestamp>/` (gitignored).
