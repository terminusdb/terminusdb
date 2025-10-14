
# Contributing to TerminusDB

## Quick Start for Development

### Local Development Server (Fastest - Recommended)

For rapid iteration during development, use the test server script:

```bash
# Start test server (builds if needed, reuses existing storage)
./tests/terminusdb-test-server.sh start

# Start with fresh storage (wipes previous data)
./tests/terminusdb-test-server.sh start --clean

# Check server status
./tests/terminusdb-test-server.sh status

# View logs
./tests/terminusdb-test-server.sh logs

# Quick restart (keeps storage, for code changes)
./tests/terminusdb-test-server.sh restart

# Restart with fresh storage
./tests/terminusdb-test-server.sh restart --clean

# Stop server
./tests/terminusdb-test-server.sh stop

# Stop server and remove all test data
./tests/terminusdb-test-server.sh clean
```

**Benefits:**
- ✅ **Fast rebuilds**: Only rebuilds Rust if sources changed (`make rust && make dev`)
- ✅ **Safe by default**: Preserves storage unless `--clean` flag is used
- ✅ **Port conflict detection**: Checks for existing processes on port 6363
- ✅ **Quick readiness check**: Server ready in ~7.5s (15 checks @ 0.5s intervals)
- ✅ **Consistent**: Same setup across all developers
- ✅ **Background process**: Runs in background with PID tracking
- ✅ **Default credentials**: admin/root (configurable with `TERMINUSDB_ADMIN_PASS`)

**Server Details:**
- URL: `http://127.0.0.1:6363`
- User: `admin`
- Pass: `root`

### Manual Development Workflow

If you need more control:

```bash
# Build Rust library and create development binary
make rust && make dev

# Start server manually with custom storage
./terminusdb serve --storage /tmp/my-test-db
```

**Note:** The test server script is preferred as it ensures consistency and prevents storage conflicts between test runs.

## Alternative Run Methods

### Using swipl (instant start, in-memory)

For quick Prolog-only tests without Rust:

```bash
swipl src/start.pl serve --memory
```

### Using Docker (slower, full environment)

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

## Building on macOS

### Development Build (Recommended)

For local development on macOS, use the development build target which avoids code signing issues:

```bash
make dev
```

This creates a `terminusdb` binary that:
- ✅ Works immediately without Gatekeeper/code signing issues
- ✅ Dynamically links to SWI-Prolog libraries (no stripping)
- ✅ Faster to rebuild during development
- ⚠️  Requires SWI-Prolog to be installed (via Homebrew recommended)

Run the development binary:

```bash
./terminusdb help
./terminusdb serve
./terminusdb store init
```

### Production Build

For production or distribution, build the standalone binary:

```bash
make
```

This creates a standalone `terminusdb` binary (strips and embeds libraries), but requires **one-time Gatekeeper approval**:

1. In Finder, navigate to the project directory
2. **Double-click** the `terminusdb` file
3. Click **"OK"** when macOS shows the security warning
4. **Right-click** on `terminusdb` → **"Open"**
5. Click **"Open"** to approve

After this one-time step, the binary will work from the command line.

**Why?** The standalone build strips and embeds SWI-Prolog libraries, invalidating their code signatures. macOS security requires manual approval for such binaries.

### Quick Testing with Wrapper Script

Alternatively, use the shell wrapper for development:

```bash
./terminusdb.sh help
```

This bypasses binary compilation entirely and runs via `swipl` directly.

## Running Tests

### JavaScript/Mocha Tests

Run JavaScript tests from the repository root, that does not NOT cd into tests directory as it becomes a nuisance!

```bash
# Run all JavaScript tests
npx mocha tests/test/*.js --timeout 10000

# Run specific test file
npx mocha tests/test/graphql.js --timeout 10000

# Run tests matching a pattern
npx mocha tests/test/graphql.js --grep "decimal precision" --timeout 10000

# Run with coverage
npm run test:coverage
```

**Important:** Always run `npx mocha` from the repository root directory, not from within the `tests/` directory. Using `cd tests &&` in commands can cause terminal hanging issues.

### Prolog Tests

```bash
# Run all Prolog tests
swipl -g run_tests -t halt src/bootstrap.pl

# Run specific Prolog test module
swipl -g "run_tests(graphql_numeric_serialization)" -t halt src/bootstrap.pl
```

### Test Server Management

Before running JavaScript tests, ensure the test server is running:

```bash
# Start test server
./tests/terminusdb-test-server.sh start

# Check if server is ready
./tests/terminusdb-test-server.sh status

# Restart after code changes
./tests/terminusdb-test-server.sh restart
```

### Quick Test Workflow

Typical development cycle:

```bash
# 1. Make code changes
# 2. Rebuild
make dev

# 3. Restart test server
./tests/terminusdb-test-server.sh restart

# 4. Run relevant tests (from repo root!)
npx mocha tests/test/graphql.js --timeout 10000
```
