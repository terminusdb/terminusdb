
# Contributing to TerminusDB

## How to run

Using swipl (instant start), and in memory. If just for quick tests.

```bash
swipl src/start.pl serve --memory
```

Using docker (takes a long time to build)

```bash
docker build -t terminusdb-dev .
docker run -p 6363:6363 --name terminusdb-test terminusdb-dev
```

### Fast Docker Build (Skip Tests)

For faster development iterations, you can skip both Rust and Prolog tests:

```bash
docker build --build-arg SKIP_TESTS=true -t terminusdb-dev .
docker run -p 6363:6363 --name terminusdb-test terminusdb-dev
```

This significantly reduces build time but should **only be used for development**. Always run the full test suite (without `SKIP_TESTS`) before creating pull requests or deploying to production.
