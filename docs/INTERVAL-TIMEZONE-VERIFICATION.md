# Interval timezone preservation — verification status

The `xdd:dateTimeInterval` encoding now carries the client-supplied zone designator per endpoint (52-byte entries, `INTERVAL_NO_ZONE` sentinel for zone-less local time). This note records what was verified end to end, and what is still outstanding.

## Verified

| Suite | Result | Scope |
|-------|--------|-------|
| `cargo test` (tdb-succinct, lib) | 187 passed | 52-byte encoding, wrong-width rejection, `Z`/offset/no-zone round-trips, duration tz inheritance |
| `make test SUITE='[typecast]'` (plunit) | 62 passed | Term construction `date_time_interval/6` with `tz(Secs)\|none` args, tz-aware string rendering, `xsd:string` casts both directions |
| `tests/test/document-interval.js` (mocha) | 13 passed | Document API round-trip: `+02:00`, `-05:00`, zone-less, mixed offsets all preserved verbatim |
| `tests/test/graphql-interval.js` (mocha) | 7 passed | GraphQL queries and chronological filters over stored intervals |
| `tests/test/woql-comparison.js` (mocha) | 173 passed | WOQL `Interval`, `IntervalStartDuration`, `IntervalDurationEnd`, and all `IntervalRelation(Typed)` Allen-algebra predicates over the `/6` term |

The Prolog term contract is `date_time_interval(C1, C2, Duration, Flag, Tz1, Tz2)` where each `Tz` is `tz(SecondsWestOfGreenwich)` — SWI's `date_time_stamp` convention — or `none`. Rust stores i16 offset-minutes (`-secs/60`); WOQL-constructed intervals use `tz(0)` since typed literals carry no client zone.

## What to do before committing

- **`src/rust/Cargo.toml` is pinned to the local tdb-succinct checkout** (`{path="../../../tdb-succinct"}`) so the engine builds against the new 52-byte `DateTimeInterval`. Revert to the git dependency once a tdb-succinct release/tag containing the timezone change exists — do not commit the path patch.
- The tdb-succinct change lives on branch `xbrl-json-interval-timezone` and needs to merge/release first; then repin `Cargo.toml` to that version.
- Any store that already contains 48-byte interval entries will fail loudly on read (`invalid DateTimeInterval encoding width`). There is no legacy decoding by design — if real stores turn out to hold old entries, an explicit typed-dictionary rewriter is required.

## Gotcha for future runs

`make test-int`'s `server-clean` does not force-restart an already-running test server. If the interval tests fail while the Prolog round-trip is demonstrably correct (`literals:typecast` shows `tz(...)` args), restart explicitly: `./tests/terminusdb-test-server.sh restart --clean`, then run mocha with `REUSE_SERVER=1`.

## Not yet run

- `woql-comparison.js` interval *duration* tests beyond the Allen battery (all 173 pass, but other files touching `date_time_interval`/`Interval*` WOQL may exist in `tests/test/`).
- Python-client interval round-trips (client change scope is engine-side only for now).
- Full `make test` plunit run — only the `typecast` suite was exercised; `casting.pl` also affects other suites indirectly through `is_date_time_interval` and literal normalization.
