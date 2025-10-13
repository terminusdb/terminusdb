
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
