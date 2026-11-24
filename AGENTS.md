# Agent rules

See `docs/CONTRIBUTING.md` for build, test, and contribution conventions. Rules below state their reason where the reason decides an edge case.

## Validation

`make pr-light` (lint, lint-mocha, lint-openapi, clippy, dev, restart, test, test-int) is the definition of ready-to-commit: green before you present stage/commit commands, failures fixed not reported.

## Prose

One paragraph is one line — in every markdown file, including this one, because wrapped prose produces noisy diffs and hides single-word changes. Never wrap to a column width; re-flow wrapped paragraphs you touch. Line breaks are for code blocks, tables and lists.

## Voice

Write like a human senior software architect — natural phrasing a colleague would write. Roughly 80% ASD-STE-100: one claim per sentence, present tense, `must`/`is` for requirements, active voice, code and predicate names verbatim. Mechanism, evidence, limitation — numbers argue, adjectives don't. Keep the reader's form (issue sections, report headings). Applies to issues, discussions, reports, PR summaries and commit messages.

## Commit messages

Argue *why* — the diff already shows what, and the reader of `git log` needs the motivation: the defect that loses data, the contract that was missing, the semantics established. Subject: imperative, names the behaviour not the files ("Stream sys:JSONDocument verbatim on document reads"). Body: motivation first, then mechanism only where the log reader needs it, then migration cost when a default changes. Short — a subject plus a few lines, not a report; code and parameter names verbatim (`raw_id`, `identifiers=false`).

Format `git commit -m "<subject>" -m "<body>"`: the body is one unwrapped paragraph, attribution on its own line after a blank — never a heredoc with hard-wrapped lines. Commits are human-verified and human-signed, so AI assistance is acknowledged generically ("AI-assisted, human-reviewed") — never a tool name, no "Generated with" or "Co-Authored-By" trailers.

## Code and comments

Code must read like technical English: names carry the meaning (`indexer_worker`, `round_robin_queue`, `serves_ordinary_requests_while_indexing_occupies_its_own_worker`), structure carries the flow, and a reader understands the code without the comments around it. A comment that explains what code does marks unclear code — rename or restructure until the comment is redundant, then delete it, because code and comments diverge at every call-site change and the comment rots first. Comments may carry why — code often cannot: an unintuitive constraint, a caller the language cannot see, the reason a call is deliberately absent — but they stay terse, one line where possible.

Everything a reader needs lives in the repository: the code, the diff, the commit message. No ticket numbers or tracker links in code or comments — an open-source contributor cannot see them and should not have to. Comments are timeless — no "now", "new", "currently" or narration of the diff; history belongs to the commit message, and small commits keep that history legible.

## Public docs

Same bar, plus a stated negative space: every doc page and blog post says plainly what it does not verify, prove, or cover. Precise numbers, named tools and pins over "industry-standard" — trust comes from the reader being able to check every claim.

