# Agent rules

See `docs/CONTRIBUTING.md` for build, test, and contribution conventions.

## Validation before staging

Before presenting stage/commit commands for review, run the full verification:

```sh
make pr-light
```

This runs `lint`, `lint-mocha`, `lint-openapi`, `clippy`, `dev`, `restart`, `test` and `test-int`. Any failure it surfaces must be fixed before the work is considered ready — a green `pr-light` is part of the definition of ready-to-commit.

## Prose formatting

Do not hard-wrap prose in markdown or similar files — write paragraphs
as single lines. Line breaks belong to code and to lines that have a
length limitation.

## Markdown voice

Issue, discussion, report and PR-summary markdown is written at roughly
80% ASD-STE-100: one claim per sentence, present tense, `must`/`is` for
requirements, active voice, code and predicate names verbatim, no
adjectives that carry no evidence. Tone of voice: senior software
architect — state mechanism, evidence, and limitation; let numbers
argue. Keep the reader's form intact (GitHub issue/discussion template
sections, report headings).
