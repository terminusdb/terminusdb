# Pull Request Checklist

Before creating a pull request, and to avoid having to go back and forth between the CI/CD and your local environment, follow the PR checklist for contributions.

Instructions for each part of this process are in the [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

1. Prepare a clean build with `make clean && make dev`
1. Restart the test server
1. Run the PL Unit Unit tests
1. Run the integration tests (ensure success)
1. Run `make lint`
1. Run `make lint-mocha` (and `make lint-mocha-fix` to fix lint errors)
1. Run `cargo clippy --message-format=json --all-features --manifest-path=src/rust/Cargo.toml` with GMP etc. installed (if you touched Rust code)

Doing the above clientside will ensure the PR is likely to work well in the build system as well and save you from having to go back and forth between the CI/CD and your local environment.

