# Contributing to TerminusDB

Thanks for taking the time to contribute to TerminusDB!

## Testing

Before submitting a change, please run both plunit and integration tests (see end) to make sure all tests pass.  Failure should result in a big fail message, and success with a final `true`. API tests will require that the admin password is `root` or that the environment variable
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
- **Fast rebuild cycle**: Only rebuilds Rust if sources changed with `make dev`
- **Safe by default**: Preserves storage unless `--clean` flag is used
- **Port conflict detection**: Checks for existing processes on port 6363
- **Quick readiness check**: Server ready in < 10s
- **Consistent**: Same setup across all developers
- **Background process**: Runs in background with PID tracking
- **Default credentials**: admin/root (configurable with `TERMINUSDB_ADMIN_PASS`)

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
```

**When changing Rust code**, you must clean the Rust library first:

```bash
rm src/rust/librust.{dylib,so}
make dev
```

**Why?** The Rust library is cached and `make dev` won't automatically rebuild it if source files change. Removing the library forces a clean rebuild.

This creates a `terminusdb` binary that:
- Works immediately without Gatekeeper/code signing issues
- Dynamically links to SWI-Prolog libraries (no stripping)
- Faster to rebuild during development
- Requires SWI-Prolog to be installed (via Homebrew recommended)

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

## Suggested development workflow for altering existing code

1. Understand in detail what the issue or enhancement is about
2. If needed, build a small throw-away demo code to understand the logic clearly
3. Create new PLUnit tests in the same file for swipl unit tests
4. Run make dev to rebuild the binary and include the autoloaded modules
5. Run the swipl unit tests using commands below as they load correctly
6. Create integration tests once the plunit tests runs for the parts that now work
7. Run the integration tests with the mocha commands below
8. Run the full test suite and fix lint errors

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

### Running Prolog PLUnit Unit Tests

```bash
# Run all Prolog tests
swipl -g run_tests -t halt src/interactive.pl

# Run specific Prolog test module
swipl -g "run_tests(graphql_numeric_serialization)" -t halt src/interactive.pl

# Run a specific test, in a specific module
swipl -g "run_tests(woql:group_by_single_element_list_template)" -t halt src/interactive.pl
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

# 2. Rebuild (clean Rust library if you changed Rust code)
# For Prolog-only changes:
make dev

# For Rust changes:
rm src/rust/librust.{dylib,so}; make dev

# 3. Restart test server
./tests/terminusdb-test-server.sh restart

# 4. Run relevant tests (from repo root!)
npx mocha tests/test/graphql.js --timeout 10000

### Debugging and Logging

#### GitHub Actions integration-test logs

When CI integration tests fail (Docker image tests and the native-build workflow), the job streams the TerminusDB server logs into the GitHub Actions step output and also uploads them as artifacts (`terminusdb-server-logs-<artifact>` for Docker jobs, `terminusdb-server-logs-native-<os>` for native jobs). Download the relevant artifact or expand the "Capture TerminusDB logs" step to inspect crashes or panics that occur before the Mocha tests report their failure.

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

TerminusDB provides built-in logging functions in Rust that integrate with the server's structured JSON logging system.

**Built-in Logging Functions:**

The logging module (`src/rust/terminusdb-community/src/log.rs`) provides five severity levels:

```rust
use crate::log::{log_debug, log_info, log_notice, log_warning, log_error};

// Inside a predicate function with a context parameter
predicates! {
    #[module("$my_module")]
    semidet fn my_predicate(context, input_term, output_term) {
        // Simple logging (pass context and message)
        log_debug!(context, "Starting processing");
        log_info!(context, "Processing item");
        log_warning!(context, "Potential issue detected");
        log_error!(context, "Operation failed");
        
        // Formatted logging (like Rust's format! macro)
        let value = input_term.get::<i64>()?;
        log_debug!(context, "Input value: {}", value);
        log_info!(context, "Processing {} items", count);
        log_warning!(context, "Value {} exceeds threshold", value);
        log_error!(context, "Failed to process {} with error: {:?}", id, error);
        
        output_term.unify("result")
    }
}
```

**How It Works:**

1. The Rust logging functions call Prolog's `json_log:json_log/2` predicate
2. Messages appear in the server log with proper timestamps, severity, and metadata
3. Log output respects `TERMINUSDB_LOG_LEVEL` environment variable
4. Log format respects `TERMINUSDB_LOG_FORMAT` (text or json)

**Logging Levels:**

- `log_debug!` - Development debugging (only shown when `TERMINUSDB_LOG_LEVEL=DEBUG`)
- `log_info!` - General informational messages (default level)
- `log_notice!` - Notable but normal conditions
- `log_warning!` - Warning conditions
- `log_error!` - Error conditions

**Controlling Log Output:**

```bash
# Set log level (DEBUG, INFO, NOTICE, WARNING, ERROR)
export TERMINUSDB_LOG_LEVEL=DEBUG

# Set log format (text or json)
export TERMINUSDB_LOG_FORMAT=text

# Start server with debug logging
TERMINUSDB_LOG_LEVEL=DEBUG ./tests/terminusdb-test-server.sh restart

# View logs
./tests/terminusdb-test-server.sh logs
```

**Example Usage in Context:**

```rust
predicates! {
    #[module("$json_preserve")]
    semidet fn json_read_string_preserving_numbers(context, json_string_term, result_term) {
        log_debug!(context, "json_read_string_preserving_numbers called");
        
        let json_string: PrologText = json_string_term.get()?;
        let json_str = json_string.to_string();
        
        log_debug!(context, "Parsing JSON string of length: {}", json_str.len());
        
        match serde_json::from_str::<Value>(&json_str) {
            Ok(parsed) => {
                log_info!(context, "Successfully parsed JSON");
                // ... process result
                result_term.unify(dict)
            }
            Err(e) => {
                log_error!(context, "JSON parse error: {:?}", e);
                Err(PrologError::Exception)
            }
        }
    }
}
```

**Alternative: File-Based Logging (When Built-in Logging Isn't Available):**

For Rust code that doesn't have access to a Prolog context (e.g., standalone functions), use temporary file logging:

```rust
use std::io::Write;

if let Ok(mut f) = std::fs::OpenOptions::new()
    .create(true)
    .append(true)
    .open("/tmp/debug_output.log")
{
    let _ = writeln!(f, "Debug message: {:?}", some_value);
}

// View output
// tail -f /tmp/debug_output.log
```

**Best Practices:**

- **Always use built-in logging** when you have a `context` parameter
- **Use appropriate severity levels** - avoid `log_error!` for non-errors
- **Include context in messages** - function name, key identifiers
- **Remove debug logging** before committing (or use INFO+ level for permanent logs)
- **File-based logging** should only be used when context isn't available

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

With context (preferred):
```rust
predicates! {
    semidet fn process_data(context, input_term, output_term) {
        let input: Data = input_term.get()?;
        
        log_debug!(context, "Processing input: {:?}", input);
        
        let result = transform(&input)?;
        
        log_debug!(context, "Transform result: {:?}", result);
        
        output_term.unify(result)
    }
}
```

Without context (fallback):
```rust
fn helper_function(input: &Data) -> Result<Output> {
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

✅ **Use built-in logging:** Always prefer `log_debug!`, `log_info!`, etc. macros in Rust when you have a `context` parameter. They integrate seamlessly with the server's logging system.

⚠️ **Log levels:** Set `TERMINUSDB_LOG_LEVEL=DEBUG` to see debug messages. Default is INFO level.

⚠️ **Clean up:** Remove debug logging code before committing unless it's part of permanent monitoring/debugging infrastructure.

⚠️ **Performance:** Excessive logging in hot paths can impact performance. Use INFO+ levels for production code.

## Submitting Changes

Before submitting a change, please run `make && ./terminusdb test` to make sure that all tests pass. Failure should result in a big fail message, and success with a final `true`. API tests will require that the admin password is `root` or that the environment variable `TERMINUSDB_ADMIN_PASS` is set prior to invocation of `terminusdb`.

Please send a [GitHub Pull Request](https://github.com/terminusdb/terminusdb/pull/new/main) to the main branch.

Please write clear log messages with your commits. Small changes can be a one line message, but big changes should have a descriptive paragraph with a newline after the title in the message.

It should preferably look something like this: 

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

### Quick sanity tests

```
make test
make test-int
make lint
make lint-mocha
```

### Defensive Coding

When handling input from external sources (client libraries, user queries, API requests), use explicit type checking rather than wildcards or permissive pattern matching. This prevents type confusion bugs and potential security vulnerabilities.

For example, when matching typed literals, be explicit about the expected type:
```prolog
% Good - explicit type checking
Key = Value^^'xsd:string'

% Bad - accepts any type, potential security issue
Key = Value^^_
```

Always validate assumptions about data types and provide clear error messages when validation fails. This makes bugs easier to diagnose and prevents silent failures that can lead to incorrect results or security issues downstream.

### The perfect PR checklist

1. Leverage TDD where relevant: add failing unit and integration tests first to verify assumptions of how the thing should work
2. Fix the failing tests, and ensure no regressions in other tests 
3. Complete the PR, write an informative PR summary to help the reviewer
4. Run the full unit test suites
5. Run the integration test suites
6. Run the lint tool

```bash
rm src/rust/librust.*
make dev
./tests/terminusdb-test-server.sh restart
make lint && make lint-mocha && make test && make test-int
```

### Veterans of the Tabs / Spaces War

No tabs please! 4 spaces for indentation. This is non-negotiable.
