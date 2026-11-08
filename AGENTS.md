# Agent rules

See `docs/CONTRIBUTING.md` for build, test, and contribution conventions.

## Validation before staging

Before presenting stage/commit commands for review, run the full verification:

```sh
make pr-light
```

This runs `lint`, `lint-mocha`, `lint-openapi`, `clippy`, `dev`, `restart`, `test` and `test-int`. Any failure it surfaces must be fixed before the work is considered ready — a green `pr-light` is part of the definition of ready-to-commit.
