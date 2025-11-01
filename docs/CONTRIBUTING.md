# Contributing to TerminusDB

Thanks for taking the time to contribute to TerminusDB!

## Testing

Before submitting a change, please run `make ; ./terminusdb test` to make sure
that all tests pass.  Failure should result in a big fail message, and
success with a final `true`. API tests will require that the admin
password is `root` or that the environment variable
`TERMINUS_ADMIN_PASSWD` is set prior to invocation of `terminusdb`.
It is preferred that the integration tests are run with the test server
script `./tests/terminusdb-test-server.sh start`. Starting the server
with `--clean` will wipe the storage directory. Run the tests with

```bash
npx mocha tests/test/*.js
```

Tools often face issues running directly in folders, getting stuck. Instead, use the below pattern to perform tasks such as running the lint tool for javascript tests:

```bash
sh -c "cd tests && npm run lint"
```

**Note about running tests from different locations:**

Integration tests can be run from **both** the repository root and the tests directory:

```bash
# From repo root
npx mocha tests/test/*.js
```

**CLI Test Wrapper:** The CLI test files use helper functions (`util.terminusdbScript()` and `util.servedPath()`) that automatically detect the correct paths based on your current working directory. The underlying `tests/terminusdb.sh` wrapper script can run tests against either:
- A locally built executable (default: auto-detected relative to script location)
- A Docker container (set `TERMINUSDB_DOCKER_CONTAINER` env var)

**Note:** Some tests verify that the git hash of the repository matches the git hash of the binary. If you get info_ok or , rebuild with `make dev` first.

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
- ✅ **Fast rebuild cycle**: Only rebuilds Rust if sources changed with `make dev`
- ✅ **Safe by default**: Preserves storage unless `--clean` flag is used
- ✅ **Port conflict detection**: Checks for existing processes on port 6363
- ✅ **Quick readiness check**: Server ready in < 10s
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
# (see specific instructions for macOS further down)
make rust

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
# Prefer adding the --rm flag too for container clean up after it exits!
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

# Seems make rust could also work on macOS, but unclear if it does
make rust
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

# Run two tests matching a pattern
npx mocha tests/test/graphql.js tests/test/decimal-precision.js

# Run with coverage
npm run test:coverage
```

Always run `npx mocha` from the repository root directory, not from within the `tests/` directory. Using `cd tests &&` in commands can cause terminal hanging issues in some workflows.

### Prolog Tests

```bash
# Run all Prolog tests
swipl -g run_tests -t halt src/interactive.pl

# Run specific Prolog test module
swipl -g "run_tests(graphql_numeric_serialization)" -t halt src/interactive.pl
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

## Debugging and Logging

### Prolog Logging

TerminusDB uses a structured JSON logging system defined in `src/core/util/json_log.pl`. Use these functions for consistent logging:

**Logging Functions:**
```prolog
:- use_module(core(util)).  % Imports json_log module

% Basic logging (simple message)
json_log_debug(Message).
json_log_info(Message).
json_log_warning(Message).
json_log_error(Message).

% Formatted logging (with format string)
json_log_debug_formatted('Variable value: ~q', [Value]).
json_log_info_formatted('Processing ~w items', [Count]).
json_log_warning_formatted('Deprecated function ~w called', [FuncName]).
json_log_error_formatted('Failed at ~w with ~q', [Location, Error]).
```

**Direct stderr output:**
```prolog
% For quick debug output (not structured)
format(user_error, 'Debug: ~q~n', [SomeValue]).
```

**Log Levels:**
- `debug` - Development debugging (disabled by default in production)
- `info` - General informational messages
- `warning` - Warning conditions
- `error` - Error conditions

**Viewing Logs:**
```bash
# View test server logs
./tests/terminusdb-test-server.sh logs

# Or directly
tail -f tests/.terminusdb-test.log
```

### Rust Logging

For Rust code in `src/rust/terminusdb-community/`, use file-based logging, until we find a better mechanism:

**Standard Pattern:**
```rust
use std::io::Write;

// Inside a function where you need to debug
if let Ok(mut f) = std::fs::OpenOptions::new()
    .create(true)
    .append(true)
    .open("/tmp/debug_output.log")
{
    let _ = writeln!(f, "Debug message: {:?}", some_value);
    let _ = writeln!(f, "Variable x = {}, y = {}", x, y);
}
```

**Best Practices:**
- Use descriptive log file names: `/tmp/graphql_debug.log`, `/tmp/json_fragment.log`
- Include timestamps for multi-request debugging
- Add context to log messages (function name, key identifiers)
- Remove debug logging before committing (unless part of permanent debugging infrastructure)

**Viewing Rust Logs:**
```bash
# Watch log file in real-time
tail -f /tmp/debug_output.log

# View recent entries
tail -20 /tmp/debug_output.log

# Clear old logs before testing
rm /tmp/*.log && npx mocha tests/test/yourtest.js
```

**Example with Timestamps:**
```rust
use std::io::Write;
use chrono::Utc;  // Add chrono to Cargo.toml if needed

if let Ok(mut f) = std::fs::OpenOptions::new()
    .create(true)
    .append(true)
    .open("/tmp/my_debug.log")
{
    let timestamp = Utc::now().to_rfc3339();
    let _ = writeln!(f, "[{}] Function called with value: {:?}", timestamp, value);
}
```

### Debugging GraphQL Queries

GraphQL queries execute in Rust and can be debugged using the file logging pattern above. Key files:

- `src/rust/terminusdb-community/src/graphql/schema.rs` - Schema resolution and field extraction
- `src/rust/terminusdb-community/src/graphql/mod.rs` - Query execution
- `src/rust/terminusdb-community/src/graphql/filter.rs` - Filter logic

### Common Debugging Scenarios

**Tracing function execution:**
```prolog
my_function(Input, Output) :-
    json_log_debug_formatted('my_function called with: ~q', [Input]),
    do_something(Input, Intermediate),
    json_log_debug_formatted('Intermediate result: ~q', [Intermediate]),
    final_step(Intermediate, Output),
    json_log_debug_formatted('Final output: ~q', [Output]).
```

**Debugging data transformations in Rust:**
```rust
fn process_data(input: &Data) -> Result<Output> {
    use std::io::Write;
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open("/tmp/process_data.log")
    {
        let _ = writeln!(f, "INPUT: {:?}", input);
    }
    
    let result = transform(input)?;
    
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open("/tmp/process_data.log")
    {
        let _ = writeln!(f, "OUTPUT: {:?}", result);
    }
    
    Ok(result)
}
```

### Important Notes

⚠️ **stderr vs files:** TerminusDB captures stderr in logs, but file-based logging in `/tmp/` gives more control and easier grepping.

⚠️ **Clean up:** Always remove debug logging code before committing unless it's part of a permanent debugging feature.

⚠️ **Performance:** File I/O in hot paths can slow down execution. Use sparingly in production code paths.

## Submitting Changes

Before submitting a change, please run `make && ./terminusdb test` to make sure that all tests pass. Failure should result in a big fail message, and success with a final `true`. API tests will require that the admin password is `root` or that the environment variable `TERMINUSDB_ADMIN_PASS` is set prior to invocation of `terminusdb`.

Please send a [GitHub Pull Request](https://github.com/terminusdb/terminusdb/pull/new/main) to the main branch.

Please write clear log messages with your commits. Small changes can be a one line message, but big changes should have a descriptive paragraph with a newline after the title in the message.

It should look something like this: 

```bash
$ git commit -m "My change title

This is a paragraph describing my change."
```

## Development Environment

One of the easier ways to set up a development environment is by forking the git repository, cloning it and checking out the `dev` branch. Docker is a prerequisite for setting it up this way, an alternative is following the instructions in [BUILD.md](BUILD.md).

1. Make a fork on GitHub
2. Clone the repository with `git clone git@github.com:[your_username]/terminusdb.git`
3. Go to the directory `cd terminusdb`.
4. Run `docker run -it --mount type=bind,source="$(pwd)",target=/app/terminusdb -p 6363:6363 --rm terminusdb/terminusdb:dev` inside the terminusdb directory. It will mount the current sources to the Docker container.
5. Run `make.` inside the swipl console after you changed the code.

## To establish a testable clean baseline one most platforms

```
make clean && make dev && tests/terminusdb-test-server.sh restart --clean && make test-int && swipl -g run_tests -t halt src/interactive.pl
```

## Coding Conventions

We have a house style for prolog, especially for conditionals. Try to copy what you see.

### Veterans of the Tabs / Spaces War

No tabs please! 4 spaces for indentation. This is non-negotiable.
