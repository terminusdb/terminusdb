# Commit Queue Timeout Debugging — Test Plan

## Objective
Resolve the `commit_queue_timeout` / `unexpected_commit_failure` error during parallel elaboration in the commit queue, and verify correctness with unit tests before re-running integration tests.

## TDD Guidance

- **Use TDD.** Always make sure relevant unit tests run before attempting integration tests (unless for a quick verification or to test assumptions).
- **Do not use integration tests to drive correctness.**
- If behaviors are odd, ensure the correct logic is established in unit tests for the relevant components.
- Make code and logic testable.

## Current Status (2026-06-28)
- Rust fast path now handles simple docs with or without explicit `@id`; rejects non-Random keys and subdocuments.
- Contract construction moved from Prolog to Rust for the fast path.
- `parallel_elaboration` unit tests: 21 passed (debug prints/counter removed).
- Relevant unit suites: 105 passed.
- Priority integration tests: 203 passing, 4 pending (no failures): `document-get.js`, `document-delete.js`, `document-full-replace.js`, `document-backlink.js`, `document-auth.js`, `document-get-hierarchy.js`, `document-interval.js`, `document-read-consistency.js`, `migration.js`, `cli-schema-migration.js`.
- `parallel_elaboration` unit test `set_of_people_elaborates` still passes with a choicepoint warning.
- Congestion benchmark (40k docs, 4 writers): ~3.9k docs/s at chunk 1000, ~6.1k docs/s at chunk 5000/10000. Bottleneck is commit-queue serialization, not elaboration or contract construction.
- `manual/parallel-elaboration-comparison.js` now runs cleanly in cached mode with `PARALLEL_ELIGIBLE=true` by default; uncached mode removed.

## Unit Tests to Monitor (relevant to commit queue / parallel elaboration / document insert)

Run these first when making changes:

- `commit_queue_helpers`
- `parallel_elaboration`
- `json_preserve`
- `commit_queue`
- `document_id_generation`
- `replace_document`
- `delete_document`
- `full_replace`
- `document_error_reporting`
- `document_id_capture`
- `subdocument_as_document`

## Priority Integration Tests

Verify these before declaring the fix complete:

1. `test/document-get.js` — original failing test; queued insert with simple docs and nested `Set`/`Optional` references.
2. `test/document-delete.js` — queued delete path.
3. `test/document-full-replace.js` — queued full-replace path.
4. `test/document-backlink.js` — backlink handling in queued commits.
5. `test/document-auth.js` — auth checks with queued commits.
6. `test/document-get-hierarchy.js` — document hierarchy retrieval after queued commits.
7. `test/document-interval.js` — interval/document feature.
8. `test/document-read-consistency.js` — data-version consistency with queued commits.
9. `test/migration.js` — migration API sanity.
10. `test/cli-schema-migration.js` — CLI migration path.

## Known Still-Failing / Investigate

- `test/document-backlink.js` — 6 failures with `Unhandled exception: unable_to_assign_ids`. Appears pre-existing (also fails with `TERMINUSDB_PARALLEL_ELABORATION=off`). Needs a unit test to reproduce.
- `tests/manual/parallel-elaboration-comparison.js` — fails with `Type error for <atom-id> which should be string` in the `cached + disjoint` configuration. Isolated debug scripts using the same data and server settings do **not** reproduce it; likely triggered by the benchmark's specific startup/sequencing. Needs targeted reproduction.
- `test/document-auth.js` — FIXED. `add in parallel` now correctly fails with `api:SameDocumentIdsMutatedInOneTransaction` after changing `pairs_to_insert_ids_and_captures_` to return all non-subdocument IDs from the contract's `id_pairs`.
- `test/document-full-replace.js` — FIXED. Empty-body queued insert now succeeds after: (a) reading the lazy stream directly in `collect_stream_docs` and (b) short-circuiting empty pairs in `api_insert_documents_core`.

## Verification Commands

```bash
# Relevant unit tests
make test SUITE='[commit_queue_helpers,parallel_elaboration,json_preserve,commit_queue,document_id_generation,replace_document,delete_document,full_replace,document_error_reporting,document_id_capture,subdocument_as_document]'

# Priority integration tests (run from tests/)
cd tests
npx mocha 'test/document-get.js'
npx mocha 'test/document-delete.js'
npx mocha 'test/document-full-replace.js'
npx mocha 'test/document-backlink.js'
npx mocha 'test/document-auth.js'
npx mocha 'test/document-get-hierarchy.js'
npx mocha 'test/document-interval.js'
npx mocha 'test/document-read-consistency.js'
npx mocha 'test/migration.js'
npx mocha 'test/cli-schema-migration.js'
```

## Notes

- Keep this file updated whenever a new unit or integration test fails.
- Do not remove tests from the lists without a note explaining why.
