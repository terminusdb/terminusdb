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

## Public docs

Same bar, plus a stated negative space: every doc page and blog post says plainly what it does not verify, prove, or cover. Precise numbers, named tools and pins over "industry-standard" — trust comes from the reader being able to check every claim.

