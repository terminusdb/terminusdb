# Prebuilt vectorlink shared library

## Background

`src/rust/terminusdb-vectorlink` embeds the vectorlink engine as a pinned git dependency (`vectorlink = { git = ..., rev = "a558244" }`). Every cold build of terminusdb therefore compiles the entire vectorlink tree — lance, arrow, tokenizers, sled — which more than doubled Rust build time: ~45 minutes on macOS/homebrew in CI, and a ~29 GB `src/rust/target` locally. vectorlink already builds with `lance` at `default-features = false`, so feature trimming is exhausted; the remaining dependency tree is the product.

The integration surface is fortunately already HTTP-shaped. `terminusdb-vectorlink` constructs a `vectorlink::http_api::trusted_router` and drives every Prolog FFI call through it as an in-process axum request. That makes a C ABI shim small: the boundary is serialised HTTP either way, so nothing rich has to cross the FFI.

## Task

Build vectorlink once, in the vectorlink repo, and distribute it to terminusdb as a prebuilt shared object.

Vectorlink side:

- Add a `vectorlink-capi` crate (or a `capi` feature on the lib) that exports a stable `extern "C"` surface:
  - `vl_init(config_json) -> handle` — engine owns its own tokio runtime internally; config arrives as JSON equivalent to `vectorlink::config::Config`.
  - `vl_request(handle, method, path, headers_json, body_ptr, body_len) -> (status, headers_json, body_buf)` — implemented as `Router::oneshot` on the trusted router.
  - `vl_free(...)` for the returned buffers; `vl_shutdown(handle)` for engine teardown.
  - `catch_unwind` at every boundary; a panic must return an error, never unwind into the host.
- CI: build `libvectorlink` cdylib on linux-amd64, linux-arm64, and macos-arm64; attach the per-platform archives to a GitHub release on tags. Windows is out of scope — terminusdb has no native Windows build (`DYLIB_EXT` knows `.so` and `.dylib` only).

TerminusDB side:

- `terminusdb-vectorlink` shrinks to a thin FFI wrapper that links the downloaded `libvectorlink` and forwards the existing Prolog-facing calls. The Prolog contract is unchanged.
- Distribution follows the existing `admin-dist`/`data-dist` precedent: a `vectorlink-dist` Makefile target downloads `libvectorlink-{os}-{arch}` for a pinned release tag. The git-rev pin becomes a release-tag pin, same discipline.
- Local development needs an override: `make VECTORLINK_LIB=/path/to/local/libvectorlink.dylib` so vectorlink hackers can build the dylib in a sibling checkout instead of downloading.

## Goal

A cold terminusdb build no longer compiles vectorlink. CI build times return to pre-vectorlink levels on all platforms, the dependency on the flaky rust-cache shrinks to terminusdb's own crates, and bumping vectorlink becomes a tarball swap instead of a workspace-wide recompile.

## Acceptance criteria

- `make` completes without compiling any vectorlink crate; `src/rust/target` no longer contains lance/arrow/tokenizers artifacts.
- `libvectorlink` release archives exist for linux-amd64, linux-arm64, macos-arm64, produced by vectorlink CI on tags.
- `make vectorlink-dist` fetches the pinned-tag archive for the current platform and places it where the build expects.
- A smoke test loads the real `.so` and round-trips a request through `vl_request`, catching ABI drift at test time rather than at runtime.
- All existing unit and integration tests pass unchanged against the shared-library engine, on both CI platforms.
- `make VECTORLINK_LIB=<path>` builds and tests against a locally built dylib.
- A panic inside vectorlink surfaces as a TerminusDB error, not a crash of the Prolog process.

## Risks and open questions

- FFI contract drift between the downloaded dylib and the glue crate; mitigated by the release-tag pin and the smoke test, but version skew is a real failure mode worth a deliberate upgrade procedure.
- Vectorlink's async internals run on a runtime inside the shared object rather than sharing `terminusdb-webserver`'s tokio handle — slightly more threads, and worth a look at how embedded init currently relies on the host runtime before the boundary is drawn.
- Upstream vectorlink changes now need a release cut before terminusdb can consume them; the tight git-rev iteration loop during joint development gets heavier unless the local override covers it well.

Being able to build it from TerminusDB is an important part so that tight development integrates it, whereas release and integration testing uses the binary version.
