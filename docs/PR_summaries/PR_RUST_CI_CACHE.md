# Add Rust build caching and a shared build stage to CI

## Background

Every CI run compiled the Rust workspace in `src/rust` from scratch, twice per OS: once in the unit test job and again in the integration test job of `native-build.yml`. On clean runners that meant a full `cargo build --release` — all dependencies plus the workspace crates — duplicated across both test streams. The clippy job in `code-lint.yml` had the same cold-compile problem. That is a lot of build minutes spent recompiling crates that rarely change.

## Task

Introduce a Rust build cache and restructure the native build workflow so the expensive compile happens once per OS and is shared by both test streams.

## Goal

Faster CI feedback and fewer wasted build minutes, with no change to what gets built or tested.

## Changes

`native-build.yml` now has a dedicated `build` job (ubuntu + macOS matrix) that runs `make` once and uploads the result as a `terminusdb-build-<os>` artifact containing the `terminusdb` binary, `src/rust/librust.*`, and `src/rust/.last-dist`. The stamp file matters: without it, `distribution/Makefile.prolog` would delete the dylib at parse time in the consuming job and trigger a hidden rebuild. `unit-tests` and `inter-tests` now declare `needs: build`, download the matching per-OS artifact, and go straight to testing — no protobuf, no cargo, no `make`.

Added `Swatinem/rust-cache@v2` — the most widely adopted Rust caching action — in front of every cargo invocation on the runner:

- `native-build.yml` `build` job (ubuntu + macOS matrix)
- `code-lint.yml` `clippy` job (ubuntu)

Each entry points at the `src/rust` workspace via `workspaces: src/rust`, so the action hashes `src/rust/Cargo.lock` and caches `src/rust/target` plus the cargo registry, keyed per OS. The build job's change filter is the superset of both test jobs' filters, so an artifact always exists when either stream needs one. Build steps in `native-build.yml` keep the existing `relevant_changed` gating convention.

While restructuring, the `check_changes` steps were moved to `>> $GITHUB_OUTPUT`; the previous `::set-output` syntax is disabled on current GitHub runners and fails the step.

## Out of scope

Docker image builds (`docker-*.yml`) compile Rust inside the image build — they would need buildx layer caching rather than an actions/cache step. Snap builds run inside snapcraft's LXD environment for the same reason. Neither is touched here.

## Acceptance criteria

- The `src/rust` cargo workspace is cached and restored across CI runs on ubuntu and macOS
- The release build runs once per OS and is reused by both the unit test and integration test jobs
- Clippy lint job benefits from the same cache
- No change to build outputs, test behaviour, or release artifacts
- Dependency changes (Cargo.lock diffs) invalidate the cache correctly
