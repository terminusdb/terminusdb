# TerminusDB MCP — Agent UAT Runbook

**Start by calling tools, not by writing.** Your first output is the initialize/`tools/list` calls below — no plan, no checklist, no summary of this document. If you catch yourself enumerating phases or goals in your reply, stop and make the next tool call instead. Do not reason about, summarise, or narrate future phases or goals: your working set at any moment is ONE task document — the database holds the rest.

You are the test agent. You will exercise the `terminusdb` MCP server as a real user would, prove each goal was accomplished, and report exactly what the experience was like — including every stumble.

## Phase 0 — Orient: send these calls now

1. `tools/list` — note the tool names.
2. `admin` `{"action":"docs"}` — the onboarding material; skim it once. It doubles as your schema-writing reference: in Phase 1 you write your own tracking schema from the vocabulary it documents (class docs, `@key`, `@shared`, family wrappers, embedding metadata).
3. `admin` `{"action":"info"}` — note the edition/version and whether an indexer backend is configured and available.
4. `admin` `{"action":"list_databases"}` — pick your test database name `uat_<something-unique>`; it must not already exist.

Then go straight to Phase 1.

## Rules of engagement

1. Use only the MCP tool surface. No shell, no filesystem access to the store, no reading the server's source code or documentation outside what the MCP itself exposes. If the server can't teach you something through `tools/list`, tool schemas, `admin` action `docs`, or error messages, that is a finding — record it and move on.
2. Consulting the onboarding material the server exposes (`admin` action `docs`) is expected — a few goals are easier with it. Note in the report whenever you had to resort to it; it means the tool's own description wasn't enough.
3. Invent one database name for the whole run, e.g. `uat_<something-unique>`. Do not create, modify, or delete anything outside your test databases. One goal (export/import) needs a second database name — derive it from the first (e.g. `<name>_import`) and clean it up too.
4. Choosing which tool, action, and arguments accomplish each goal is part of the test.
5. Some goals are expected rejections — a clear, instructive refusal is the pass condition there, not a crash and not a silent wrong answer.
6. Record every error the MOMENT it happens — write it verbatim into the task's `friction` field immediately, before you retry. Do not hold errors in memory for a later summary: your context is finite, and an error you don't write down is an error the report never sees. Friction is the signal: an error that teaches you the fix is a pass; an error that dead-ends you is a finding.
7. If a goal can't be completed, do not fake it. Mark it `fail` or `skip` with evidence and continue.
8. Transcribe task documents verbatim — `name`, `title`, `status` come straight from the goal entries below; `notes` and `friction` start empty and are filled with your evidence and errors. Do not paraphrase or pre-judge goals while transcribing.
9. Write the bootstrap verbatim too. Some instructions look unusual (e.g. `"@shared":[]`) — they are verified and load-bearing. Write them as given and observe what the server does; do not burn effort reconciling them with schema conventions you remember.

**Run configuration.** If `admin` `info` reports backend `vectorlink`, the embedded indexer is active — semantic search (F3, F6, task lookup) must return real results, and a "no backend" answer is a FAIL.

---

## Phase 1 — Encode the goals into the database

Phase 1 ends when `UatPlan/main` reads back with every step. Do not start executing goals yet.

**1.1 Create the database** — `admin` action `create_database` on your chosen name. (This is also goal B1 — the task is already done when you tick it.)

**1.2 Bootstrap the schema** — one schema `write` defining three classes:

- `UatTask` — `"@shared":[]` marks the class *shared*: each task is an independent document addressable by its own id, which is exactly what `UatPlan.steps` references and what `query.search`/tick-off updates need — without it the list would demand inline subdocuments. (The value is an empty list, not `true` — `true` is rejected.) `@key` `{"@type":"Lexical","@fields":["name"]}`, properties `name`, `title`, `status`, `notes`, `friction`, all `xsd:string` (`notes` = evidence, `friction` = verbatim errors and what the experience was like — they answer different report sections). Pass `indexing:{"query":"query($id: ID){ UatTask(id: $id) { name title status notes } }","template":"- **{{name}} {{title}}** — {{status}}. {{notes}}"}` on the same write — the stored render is used twice: it is the text semantic search indexes, and `format:"embedding"` returns it as a markdown fragment.
- `UatPlan` — `@key` `{"@type":"Lexical","@fields":["name"]}`, `name` `xsd:string`, `steps` `{"@type":"List","@class":"UatTask"}`. Pass `indexing:{"query":"query($id: ID){ UatPlan(id: $id) { name steps { name title status notes } } }","template":"# UAT status\n{{#each steps}}- **{{name}} {{title}}** — {{status}}. {{notes}}\n{{/each}}"}` — rendering this single document produces the whole status page.
- `UatSample` — the scratch class for the data-lifecycle goals: `@key` `{"@type":"Lexical","@fields":["name"]}`, `name` and `greeting` as required `xsd:string`, `count` as `{"@type":"Optional","@class":"xsd:integer"}`.

**1.3 Write the task documents** — ONE `write` (action `insert`) containing `UatPlan/main` plus every `UatTask` below. For each goal entry transcribe verbatim: `{"@type":"UatTask","name":"<id>","title":"<title>","status":"todo","notes":"","friction":""}`. `UatPlan/main` is `{"@type":"UatPlan","name":"main","steps":[<the task ids in catalogue order, as bare id strings like "UatTask/A1" — typed dicts are validated as inline documents and fail>]}`. Tasks A1, A2, A3, B1 may be inserted already `pass` with `notes` = the Phase 0 evidence, since they are done.

**1.4 Verify the encode.** Read `UatPlan/main` — it returns the ordered step ids. Under `vectorlink`, commits index automatically, so a `query.search` for a goal's wording should already find its task document; under `none`, listing tasks by type is the fallback. If tracking setup fights you, record the friction and continue with untracked execution — do not sink the run.

## Phase 2 — Execute the goal catalogue, one task at a time

The loop, repeated until no `todo` remains:

1. **Locate.** Read `UatPlan/main` for the ordered step ids and read the next task whose `status` is `todo` — or locate it by meaning with `query.search`. You only need the step identifier; the database holds the rest. If your context is wiped mid-run, resume here.
2. **Load.** Read that task's section below — only that section. The `title`/`notes` in the task doc plus the goal text tell you exactly what to do and what counts as proof.
3. **Execute.** Do the goal.
4. **Tick.** Cheaply — a `SwapValue` patch changes one field without resending the whole document: `write` `{"action":"patch","resource":"<db>","patch":[{"@id":"UatTask/<id>","status":{"@op":"SwapValue","@before":"todo","@after":"pass"},"notes":{"@op":"SwapValue","@before":"","@after":"<evidence>"}}],"commit_info":{…}}` — add a `friction` SwapValue alongside when there was friction. If you hit an error mid-goal, patch `friction` right then, before retrying — don't wait for the tick. (`@before` must match the current value exactly, and an empty `@before` on a field that has a value throws `empty_key` — re-read the task doc if unsure.)
5. Next task. Do not look ahead.

### Section A — Orientation (done in Phase 0 — tick with evidence)

- **A1** title:`discover the surface` — list every available tool and read the onboarding material the server exposes.
- **A2** title:`assess the environment` — state the edition/version and the indexer backend status.
- **A3** title:`confirm a clean slate` — your test database name did not already exist.

### Section B — Database lifecycle (B1 done in Phase 1 — tick with evidence)

- **B1** title:`create database` — your database appears in the listing.
- **B2** title:`inspect schema same session` — inspect the new database's schema without any restart or reconnect; a resource created moments ago resolves.
- **B3** title:`read default prefixes` — state the default `@base` and `@schema` prefix values.

### Section C — Data lifecycle

- **C1** title:`define a class` — create a class with at least two typed properties (`UatSample` from 1.2 satisfies this — verify via schema inspection that the class and its properties are listed).
- **C2** title:`dry-run before commit` — obtain a validity verdict for a document before writing it; the verdict says valid and tells you whether the document would be embedded for indexing.
- **C3** title:`insert non-ASCII document` — insert a `UatSample` document whose text field contains at least one accented character and one emoji; reading it back returns the field byte-for-byte identical.
- **C4** title:`update a field` — change one property's value; a read shows the new value and the history grew by one commit.
- **C5** title:`reject an invalid write` — dry-run a document that violates the schema (e.g. missing required property); the verdict is invalid AND explains precisely what fails — do not commit it.
- **C6** title:`retrieve by class` — fetch every `UatSample` document; the count matches and the contents are correct.
- **C7** title:`patch a field` — change a document property via the `patch` write action rather than a full replace; a read shows the new value. (The diff-op shape may need the tool description or the onboarding doc — note which you used.)
- **C8** title:`write via WOQL mutation` — commit a document insert expressed as WOQL through `write`; the document exists afterwards and you learn its id.

### Section D — Versioning

- **D1** title:`read the commit log` — it contains your commits, each with the author and message you supplied.
- **D2** title:`diff across an update` — a document-level diff between the commit before an update and after; it names the changed field with old and new values, not merely that something changed.
- **D3** title:`branch and isolate` — create a branch and write a distinct document on it; the branch appears in the listing, a read on main does NOT return it, a read on the branch does.
- **D4** title:`three-way merge` — after branching, commit different changes on BOTH branches, then merge into main; the merge commits and returns a data version, and a read on main shows both sides' documents.
- **D5** title:`conflicting merge` — change the SAME field on the SAME document differently on main and a branch, then merge; it refuses with witnesses naming the contested document/field — it must not silently pick a side. Resolving afterwards is a bonus.
- **D6** title:`rebase` — replay a divergent branch onto the current main head; it commits without a crash on missing optional arguments, and the branch afterwards contains both sides' data.
- **D7** title:`squash` — squash a branch's commits into a single commit; the confirm contract fires first without it, the squash succeeds with it, and the branch log is afterwards one commit holding all the data.
- **D8** title:`reset` — rewind a branch to an earlier commit; the confirm contract fires first without it, the reset succeeds with it, and the branch head is afterwards the commit you named.
- **D9** title:`guarded write after git mutation` — take the data version a git call returned and do a write guarded by it; it commits at the new head — no stale-version error. If a git mutation hands you no version to chain, that is a finding.

### Section E — Guardrails (each must fail, and fail well)

- **E1** title:`mutate via read-only tool` — a data-changing WOQL through `query` is rejected, naming the tool/action to use instead.
- **E2** title:`graphql mutation via read path` — a GraphQL mutation through `query` is rejected with guidance to the write path.
- **E3** title:`stale version guard` — a write guarded by a superseded data version is refused with recovery guidance.
- **E4** title:`write without commit metadata` — refused with an explanation of what the commit information is for.
- **E5** title:`delete without confirming` — a database delete without the confirmation is refused, naming exactly what to pass to proceed.

### Section F — Retrieval and server surface

- **F1** title:`woql read` — run a read-only WOQL query. WOQL JSON spells nodes and values as typed objects: `{"@type":"NodeValue","variable":"s"}` for variables, `{"@type":"Value","node":"@schema:MyClass"}` for class IRIs, `{"@type":"Value","data":"text"}` for literals. Verify bindings return.
- **F2** title:`graphql read` — fetch a document through a GraphQL query; the values match what you wrote.
- **F3** title:`semantic search` — `query.search` over your task docs by meaning, not literal tokens (e.g. `q:"version control conflicts"` should find D5). Under `vectorlink`: ranked hits — indexing is asynchronous, so a `search_not_indexed` right after writes means run `admin.reindex` then poll `admin.index_status` in the SAME session until `documents` climbs; under `none`: a single clean error explaining how indexing gets enabled. Anything else is a finding.
- **F4** title:`list plugins` — list the plugin tools; a structured response, an empty list is a valid answer.
- **F5** title:`export and import` — export the database, import it into the derived-name second database; the bundle is reported written and the imported documents are readable. (Over stdio a `path` under `/tmp` works; over HTTP only inline payloads are accepted — use whichever your transport allows, and record which.)
- **F6** title:`optimize and index status` — the optimize action is accepted; index status returns a per-branch report — under an indexer-free setup an explained "no backend" answer counts.

### Section H — Rendered status page

- **H1** title:`render status page` — fetch `UatPlan/main` with `format:"embedding"`; its template loops `steps`, so this single document renders the whole status page as markdown. Verify `page` covers every goal. If a field renders empty or the template errors, fix the metadata/documents and re-render — that debugging is part of the goal. (Alternative: `type:"UatTask"` + `format:"embedding"` renders every task fragment.)
- **H2** title:`markdown cross-check` — render the same document with `format:"markdown"`; you get a document-shaped markdown view distinct from the embedding render.

## Phase 3 — Finish

- **G1** title:`delete test databases` — delete the primary database and the imported copy, satisfying the confirmation gate. Do this AFTER rendering the status page — the page is generated from live documents. Verify the listing contains neither.
- Paste the `page` output into the report below as the goal-results evidence, then produce the report.

---

## Mandatory report format

Produce this exact structure. Replace every placeholder; keep the tables.

```markdown
# TerminusDB MCP UAT Report

- Date:
- Agent / model:
- Run duration:

## Goal results

| # | Goal | Status | One-line note |
|---|------|--------|---------------|
| A1 | Discover the surface | PASS/FAIL/SKIP | |
(… one row per goal, A1 through G1 …)

## Rendered status page

The verbatim `page` output from goal H1 — the embedding render of `UatPlan/main`.

```markdown
<paste page here>
```

## Evidence for FAIL / SKIP

### <goal id>
- What I attempted (high level):
- What came back (verbatim):
- Why I could not continue:

## Friction log

Every error encountered anywhere in the run — including ones you recovered from. Assemble this by reading the `friction` fields of your task documents — they hold the complete record; your memory does not.

| # | Context | Error / obstacle (verbatim) | Did the error help me recover? | Attempts needed |
|---|---------|------------------------------|-------------------------------|-----------------|

## Tool surface observations

- Error messages that confused or misled me:
- Capabilities I expected but could not find:
- Goals where I had to read the onboarding doc (and why the tool description wasn't enough):
- Places where the docs and the actual behavior disagreed:

## Verdict

Could a new agent complete the full lifecycle — orient → create → schema → write → verify → read → branch → merge → rebase → export/import → clean up — without help from outside the MCP? YES / NO, and the single most important reason.
```
