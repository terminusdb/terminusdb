# TDB-175 — Detect delete-versus-edit conflicts in squash apply

## Background

`POST /api/apply` performs a three-way merge. The caller supplies `before_commit` (the merge base), `after_commit` (the head of the incoming branch), and the API squashes the diff between them onto the current head of the target branch — typically `main`. Both heads may move while the request is in flight; `with_transaction/3` already retries the transaction on layer conflicts, so the apply is always evaluated against a live head.

The diff between the two commits is produced by `document_diffs_from_commits/5` in `src/core/api/api_patch.pl`. For each document that changed it emits one of three shapes:

- `json{'@op':'Delete', '@delete': <full base document>}` when the document exists at base but not at the incoming head.
- `json{'@op':'Insert', '@insert': <full incoming document>}` when it exists only at the incoming head.
- A `simple_diff` patch (`'@id'` plus `@op` field operations carrying `@before`/`@after` values) when it exists in both.

Each diff is then applied by `apply_diff/4` in `src/core/document/apply.pl`, which returns either `null` or a conflict witness. Non-null witnesses abort the transaction and surface to the client as HTTP 409 with `api:witnesses`.

## The defect

Two paths lost edits silently.

Incoming delete versus target edit. A `@delete` diff embeds the complete base document, but `apply_diff/4` ran `delete_document` unconditionally. If the target branch had edited the document after the fork — so the live document no longer matched the embedded base — that edit was deleted without a witness. This is the Hub-visible case: the Hub currently runs an extra diff check to reject delete-versus-edit before calling apply, because the engine did not report it.

Incoming edit versus target delete. In the modify clause, `get_document` fails (does not throw) when the target document is gone. The enclosing `findall` in `api_apply_squash_commit` simply dropped the failed goal, so the diff vanished with no witness and no write.

Field-level three-way detection already existed (`@before` in `simple_patch` compares the stored value against the base value); document existence was the missing dimension.

## The change

### `src/core/document/apply.pl`

The `@delete` clause now branches on the shape of the delete candidate. When it is a dict — the squash-apply path always embeds the full base document — the target document is fetched and compared:

- Target missing: the document is already gone, so there is nothing to lose; the outcome falls to `missing_delete_conflict`, which honours `match_final_state(true)` as a no-op and otherwise emits the existing `DeleteConflict` witness.
- Target equals the normalised base (`normalize_document/3` on the embedded dict, compared structurally, the same pattern `match_final_state` uses for inserts): the target is untouched since the base; the delete proceeds.
- Target differs from the normalised base: `DeleteEditConflict` witness.

When the candidate is a bare id string/atom — which does not occur on the squash path but is kept for robustness — the old unconditional delete behaviour is preserved.

The modify clause wraps `get_document` in a soft cut: when the document is absent it now returns a `DeleteEditConflict` witness instead of failing.

Witness shape:

```json
{
  "@op": "DeleteEditConflict",
  "@id": "item/a",
  "@deleted_by": "theirs"
}
```

`'@deleted_by'` is `"theirs"` when the incoming diff deletes a document the target edited, `"ours"` when the incoming diff edits a document the target deleted. Witnesses are identities only — a conflict list tells the caller *which* documents diverge and *who* deleted them, at a fixed small size per witness regardless of document size.

All witness document ids — `'@id'` on `DeleteEditConflict` and field-level `Conflict`, `'@id_already_exists'` on `InsertConflict`, `'@id_does_not_exists'` on `DeleteConflict` — are now compressed against the branch `@base` before reporting. This is the new id convention across the codebase for 12.1 and applies in `apply_diff/4` and `apply_diff_ids_captures/7` alike, so `/api/patch` conflicts change shape too — see *Out of scope* for the contract note.

### `src/core/api/api_patch.pl`

`api_apply_squash_commit/7` gains two behaviours before any diff is computed:

Type validation. `type` is read via `option/3` with default `squash` and must equal `squash`; anything else throws `error(bad_apply_type(Type), _)`. Previously the option was ignored entirely.

Conflict resolution. A new `conflict_resolution` option is normalised by `normalize_conflict_resolution/3` into `resolution{default:Winner, documents:Map}`:

- Bare `"error"` (default), `"ours"`, `"theirs"`, as atoms or strings.
- A dict `{default: <winner>, documents: {<winner>: [<doc-id>, …]}}` for per-document overrides — `documents` maps each winner atom to a list of document ids, so arbitrary ids never sit in JSON object keys. Ids may be written in any form — `@base`-compressed, prefix-compressed, or absolute IRI; each is expanded with `prefix_expand` and normalised internally to `id -> winner`, so lookup always happens in the expanded form the diffs carry. An id listed under two winners is ambiguous and rejected. Every winner key and list shape is validated; malformed input throws `error(bad_conflict_resolution(Raw), _)`.

Inside the transaction, each `apply_diff` witness passes through `resolve_squash_conflict/7` before collection. `patch_id` yields the expanded IRI carried by the diff, which is the lookup key directly — no id conversion happens at lookup time because every `documents` key was already expanded against the same prefixes.

- `error`: the witness is kept; the apply aborts with 409 after all diffs are processed.
- `ours`: the witness is dropped and nothing is written — the target state stands.
- `theirs`: `squash_apply_theirs/5` applies the incoming side. For a `@delete` diff it deletes the document (tolerating already-absent); otherwise it materialises the document as committed at `after_commit` via `document_from_commit/8` and writes it with `squash_force_document/3`, which mirrors the raw-JSON bypass in `apply_replace_document` for `sys:JSONDocument` payloads.

Because resolution happens inside `with_transaction`, a retry after a layer conflict re-decides every conflict against the live head — the recipe is a rule, not a frozen decision.

Insert and field-level conflicts flow through the same resolution path: `ours` keeps the target state, `theirs` writes the incoming document. The per-document map applies to any conflict kind since lookup is by expanded document IRI.

Dry-run preview. `api_check_apply_squash_commit/8` runs the identical merge — same diff enumeration, same `apply_diff`, same `resolve_squash_conflict` recipe logic — against a context whose transaction is never run, so the uncommitted layer is discarded and the branch head does not move. It returns an `api:CheckResponse` dict (`api:valid` plus `api:witnesses`), matching the `api_check_documents` convention the document API uses for `?check=true`. Under dry-run the witnesses additionally carry `@expected`/`@found` — the normalised base document and the live target document — so a merge UI can render both sides before the caller commits to a recipe. The shared setup (`squash_apply_setup`) and merge loop (`squash_merge_witnesses`) are factored out so the commit and preview paths cannot drift.

### `src/server/routes.pl`

`apply_handler` reads `conflict_resolution` and `dry_run` from the request JSON. `dry_run` must be a JSON boolean — any other value is `bad_api_document` (400), because silently ignoring `"dry_run": "yes"` would commit when the caller asked for a preview. When set, the handler calls `api_check_apply_squash_commit` and replies with its `api:CheckResponse` at 200; otherwise it calls `api_apply_squash_commit` and replies `api:ApplyResponse`/`api:success` as before. The `type` field handling changes subtly: `atom_string/2` would fail the whole handler for non-text values (numbers, dicts); such values now pass through as-is so `api_apply_squash_commit` rejects them with `bad_apply_type` and the client receives a proper 400 instead of a handler failure.

### `src/core/api/api_error.pl`

Two `api_global_error_jsonld` clauses map the new error terms to JSON responses: `bad_apply_type` → `api:BadApplyType`, `bad_conflict_resolution` → `api:BadConflictResolution`. Both carry `api:status: api:failure`, which `status_http_code` maps to HTTP 400. The 409 witness path is untouched.

### `tests/test/diff-id.js`

Four mocha tests against the real server: the 409 `DeleteEditConflict` witness (incoming delete, target edited — asserts `@id`, `@deleted_by`, and that ours' edit survives), a `dry_run` preview returning `api:CheckResponse` with the fat witness (`@expected`/`@found`) and proving nothing committed, a per-document recipe resolving `theirs` (target deleted, incoming edited — the document is resurrected at the incoming value), and `type: 'rebase'` rejected with `api:BadApplyType`. The existing `InsertConflict` assertion is updated to the compressed `@id_already_exists` convention.

### `src/core/api/api_patch.pl` test block

Ten new plunit tests sharing a `delete_edit_setup/9-10` fixture that builds the three-point merge (base commit, main-side body, dev branch forked at base, dev-side body): witnesses in both directions, global `ours`/`theirs`, a mixed per-document recipe over two documents, `type(banana)` rejection, malformed-resolution rejection, schema'd delete both ways, `match_final_state` still conflicting, `theirs` on insert and field conflicts, witness completeness — plus three `api_check_apply_squash_commit` tests covering conflict preview (fat witness, nothing committed), a clean merge reporting `api:valid:true` without committing, and a recipe-resolved dry run.

## Semantics summary

| Situation | `error` (default) | `ours` | `theirs` |
|---|---|---|---|
| Diff deletes doc, target edited it | 409 `DeleteEditConflict`, `"theirs"` | keep target edit | delete the doc |
| Diff edits doc, target deleted it | 409 `DeleteEditConflict`, `"ours"` | keep target delete | resurrect doc at incoming state |
| Diff deletes doc, target untouched | delete proceeds | — | — |
| Diff deletes doc already absent | `DeleteConflict` (or no-op with `match_final_state`) | — | — |
| `@insert` id exists | `InsertConflict` | keep target doc | write incoming doc |
| Field `@before` mismatch | `Conflict` | keep target field | write incoming doc |

## Verification

- `run_tests([apply,resolve_refs_in_diff,patch_resource,patch_diff_auth,common_ancestor,commit_history,commit_timestamp])` — 56 plunit tests pass, 24 of them in the `apply` unit.
- `npx mocha tests/test/diff-id.js` — 22 pass including the 4 new tests; run against a `make dev` + test-server restart.
- `npx mocha tests/test/patch.js` — 15 pass; the two conflict assertions now expect compressed `@id`s, matching the new convention.
- `make lint`, `make lint-mocha`, `make lint-openapi` — no findings in the touched files; openapi.yaml validates with the 4 pre-existing index-endpoint warnings only.

The full `make test` / `make test-int` batteries were not run.

## Out of scope

- Apply throughput: the per-document diff/patch path is ~30x slower than bulk insert (≈18 s for 5,000 inserted documents). Tracked as TDB-177. The audit pinned the mechanism: `commits_changed_id/5` opens every commit on both ancestor paths and enumerates changed documents per commit, when comparing the two endpoint layers' id sets would suffice.
- The rebase endpoint's unnecessary `schema_write_access` requirement.
- `apply_diff_ids_captures/7` is deliberately unchanged for delete-versus-edit detection — client-supplied `@delete` patches carry no base document, so there is nothing to compare. Its witness ids are now compressed for convention consistency; that is a `/api/patch` conflict-shape change callers must absorb in 12.1.
- Renaming the document API's `?check=true` flag to `dry_run` for naming consistency is a separate breaking-change decision — this PR uses `dry_run` on apply, matching `/api/migrate`.
- Once deployed, the Hub's extra delete-versus-edit pre-check can be removed, since the engine reports the conflict itself.

## What is left to do

Agreed follow-ups, roughly in landing order. None block this PR; each has pinning tests in place where the machinery changes.

- ~~`document_diffs_from_commits` opened both commit descriptors per changed document — roughly 2×N opens for N changed documents.~~ Done: both commits open once (`open_commit_transaction`) and the transactions are shared between diff enumeration and `theirs` resolution; measured on a two-sided fixture (3 commits per path, 1750 diffs across ~1900 documents) the enumeration dropped from 4808 ms to 1159 ms — about 4×.
- `commits_changed_id` still opens every commit on both ancestor paths and scans changed ids per commit — the remaining cost centre. The pinning test `commits_changed_id_enumerates_touched_documents` records the contract — the positive set (created, modified, deleted, insert-then-delete) and the negative set (untouched never enumerated, reverted documents produce no diff) — before the enumeration mechanism changes.
- ~~`most_recent_common_ancestor` materialised both full ancestor chains via WOQL `path` queries (its own comment admitted the time complexity).~~ Done: `zip_commit_chains` is a two-pointer walk that descends the side with the newer commit, so commits below the base are never touched. Commit timestamps only choose which side moves — each side keeps an assoc set of visited identifiers and a candidate equal to one the other side has already departed is also a match, so equal or unordered timestamps on old data can overshoot a side but cannot skip the ancestor. Two fixes the rewrite surfaced: caller-facing commit ids are atoms while the store holds strings (`commit_id_string` normalises at the walk entry — `=` never matched), and paths are re-walked head→ancestor by `commit_path_to_ancestor` rather than accumulated during descent so a timestamp-tie overshoot cannot leak sub-base commits into a path. `common_ancestor_asymmetric_depth` pins the exact path lists. This is also the machinery for deriving the merge base server-side so callers stop supplying `before_commit`.
- ~~The `union/3` over both paths was a plain list union over segments that are disjoint by construction.~~ Done: `append` now — the paths run strictly from the common base to each head, so no dedup is needed.
- The `stream` flag for NDJSON streaming of dry-run witnesses and diffs is designed but not implemented; the route rejects it today.
- `dry_run` still requires `commit_info`; a preview that commits nothing could accept a default.
- Naming: the document API's `?check=true` and apply's `dry_run` are two names for the same idea; unifying them is a separate breaking-change decision.







## Additional things to verify before closing

v Are there other issues here in the interface?
* Support multiple parents in the commit itself? [#2430](https://github.com/terminusdb/terminusdb/issues/2430)
* MRCA Most Recent Common Ancestor
* Performance, is it acceptable?
* It should be very fast with an ordset-like comparison between two branches to find the common shared commit between two branches to merge so that the user does not have to identify the original commit to merge from, that should maybe be added here?

* Additionally, we should add the source merge commit id to the commit graph following  [https://github.com/terminusdb/terminusdb/issues/2430](https://github.com/terminusdb/terminusdb/issues/2430)
  * It should be possible to ask for the ancestor commit, but the branch parent does not give that information, it should....
* ~~Remember to update the documentation and openapi.yml~~ — done: `conflict_resolution`, `dry_run`, the `api:CheckResponse` 200 note, the 400 description, and a `DeleteEditConflict` witness in the 409 example.
* Schema'd documents now have regression coverage for both sides of the delete-compare (`apply_delete_edit_schema_delete_untouched`, `apply_delete_edit_conflict_schema`); more schema shapes — subdocuments, typed literals, list fields — are worth exercising if the worldOntology test schema grows them.


* `theirs` resolution is covered for all three branches: delete-diff, `@insert` (`apply_resolution_theirs_insert_conflict`), and field-level conflict via whole-document materialisation (`apply_resolution_theirs_field_conflict`).
* `match_final_state` does not suppress a real `DeleteEditConflict` — covered for both schemaless and schema'd documents.
* Witness completeness is asserted (`apply_delete_edit_conflict_reports_all_witnesses`): every conflicting document lands in one `api:witnesses` list so callers resolve in a single pass.
* Merge granularity is per document: non-conflicting documents all apply, but the apply is all-or-nothing — any unresolved witness aborts the transaction and nothing commits until the caller resolves or retries with a recipe.
* `conflict_resolution.documents` ids that match no diff are silently ignored; a misspelt id falls back to `default` without warning.Will the two documents be fully merged so that the two edits, if not conflicting, be fully resolved as long as valid? --> Should that be prevented so that we have a full document level merge too? --> User might want the semantics on a complete document level (i.e. conflict when both have edited, and the resolution must be fully clarified so that only one document can win, keeping current behavior, but providing a flag to only let one full document survive, diff on doc level)
* `dry_run` previews the merge as `api:CheckResponse` at 200 with `@expected`/`@found` on witnesses and never commits; a `conflict_resolution` recipe may be supplied to preview the resolved verdict. The `"ours"`-side `DeleteEditConflict` has no live document to show, so it stays identity-only even under dry run — a UI can fetch the incoming side from the diff.
* The preview runs the same validation the commit would: schema failures thrown mid-merge are caught into `api:valid:false` witnesses (`apply_dry_run_reports_schema_invalid_merge` — dev inserts a Person that violates a schema tightened on main), and `check_validation_witnesses` folds commit-time `pre_commit_hook` checks into the verdict.
* Witness ids are compressed across all ops (`DeleteEditConflict`, `InsertConflict`, `DeleteConflict`, field `Conflict`) in both `apply_diff` and `apply_diff_ids_captures` — the compressed `@base` convention from 12.1 onwards.
* A document that cannot be diffed no longer disappears from enumeration: `simple_diff` throws `missing_diff_option(keep)` when the required `keep` option is absent, and `document_diffs_from_commits`/`api_diff_id`/`api_diff_id_document` throw `unable_to_diff_document(Doc_Id)` on any residual diff failure — pinned by `document_diffs_throw_when_a_document_cannot_be_diffed`.
* Top-level diff `@op` values are strings (`"Insert"`/`"Delete"`) matching the field-op convention from `simple_diff`; consumers read ops through `diff_op`/`atom_string`, so atoms still work on input.