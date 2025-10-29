# Pull Request Checklist

Before creating a pull request, and to avoid having to go back and forth between the CI/CD and your local environment, follow the PR checklist for contributions.

Instructions for each part of this process are in the [docs/CONTRIBUTING.md](docs/CONTRIBUTING.md).

1. Prepare a clean build
   * `make clean && make dev`
2. Restart the test server
   * `tests/terminusdb-test-server.sh restart --clean`
1. Run the PL Unit Unit tests
   * `swipl -g run_tests -t halt src/interactive.pl`
2. Run the integration tests (ensure success)
   * `sh -c "cd tests; npx mocha"`
3. Detect any lint issues
   * `make lint`
4. Detect any lint issues in the tests
   * `make lint-mocha`
   * `make lint-mocha-fix` to fix lint errors
5. Run `cargo clippy --message-format=json --all-features --manifest-path=src/rust/Cargo.toml` with GMP etc. installed (if you touched Rust code)

Doing the above clientside will ensure the PR is likely to work well in the build system as well and save you from having to go back and forth between the CI/CD and your local environment.

