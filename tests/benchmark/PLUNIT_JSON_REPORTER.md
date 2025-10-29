# PL-Unit JSON Reporter

## Overview

The `plunit_json_reporter.pl` module extends SWI-Prolog's PL-Unit test framework to output test results in JSON format compatible with Mocha's JSON reporter. This enables unified test result parsing and analysis across JavaScript (Mocha) and Prolog (PL-Unit) tests.

**The reporter is built-in** and automatically available in all TerminusDB entry points (`start.pl`, `interactive.pl`, `bootstrap.pl`). The `run_tests_json/0` predicate is always available, just like `run_tests/0`.

## Features

- ✅ **Mocha-compatible JSON output** - Same structure as Mocha's `--reporter json`
- ✅ **Individual test timings** - Captures duration for every test
- ✅ **Suite grouping** - Organizes tests by their test units/modules
- ✅ **Pass/Fail tracking** - Separate arrays for passed and failed tests
- ✅ **Speed classification** - Tests marked as fast/medium/slow based on duration
- ✅ **ISO timestamps** - Start and end times in ISO 8601 format

## Usage

### Standalone

Run all PL-Unit tests and output JSON:

```bash
# From the repository root
swipl -g run_tests_json -t halt -f src/interactive.pl
```

Or simply use `run_tests_json` from the SWI-Prolog interactive prompt after loading any TerminusDB entry point:

```prolog
?- [src/interactive].
?- run_tests_json.
```

### Programmatic (from Node.js)

```javascript
const { exec } = require('child_process');
const util = require('util');
const execAsync = util.promisify(exec);

async function runPlUnitTests() {
  const cmd = 'swipl -g run_tests_json -t halt -f src/interactive.pl';
  const { stdout } = await execAsync(cmd, { cwd: '/path/to/cleodb' });
  
  // Extract JSON from output (skip warnings/info messages)
  const jsonMatch = stdout.match(/\{[\s\S]*\}/);
  const results = JSON.parse(jsonMatch[0]);
  
  return results;
}
```

## Output Format

The JSON output matches Mocha's structure:

```json
{
  "stats": {
    "suites": 1,
    "tests": 3,
    "passes": 3,
    "pending": 0,
    "failures": 0,
    "start": "2025-10-15T18:00:00+02:00",
    "end": "2025-10-15T18:00:05+02:00",
    "duration": 5000
  },
  "tests": [
    {
      "title": "sample_tests:simple_pass",
      "fullTitle": "sample_tests:simple_pass",
      "duration": 2,
      "currentRetry": 0,
      "speed": "fast",
      "err": {},
      "state": "passed"
    }
  ],
  "pending": [],
  "failures": [],
  "passes": [
    {
      "title": "sample_tests:simple_pass",
      "fullTitle": "sample_tests:simple_pass",
      "duration": 2,
      "currentRetry": 0,
      "speed": "fast",
      "err": {},
      "state": "passed"
    }
  ]
}
```

## Field Descriptions

### stats object
- **suites**: Number of test units (modules)
- **tests**: Total number of tests
- **passes**: Number of passed tests
- **pending**: Number of pending tests (always 0 for PL-Unit)
- **failures**: Number of failed tests
- **start**: ISO 8601 timestamp when tests started
- **end**: ISO 8601 timestamp when tests completed
- **duration**: Total duration in milliseconds

### test object
- **title**: Test name (format: `module:test_name`)
- **fullTitle**: Same as title for PL-Unit
- **duration**: Test duration in milliseconds
- **currentRetry**: Number of retries (always 0)
- **speed**: `fast` (<50ms), `medium` (<200ms), or `slow` (≥200ms)
- **err**: Error object (empty `{}` for passed tests)
- **state**: `passed` or `failed`

## Speed Classification

Tests are classified by duration:
- **fast**: < 50ms
- **medium**: 50-200ms
- **slow**: ≥ 200ms

## Integration with Benchmark Suite

The reporter is automatically used by `benchmark_excel.js`:

```javascript
async function runPlUnitTests(runNumber) {
  const cmd = 'swipl -g "consult(\'tests/plunit_json_reporter.pl\'), consult(\'src/interactive.pl\'), run_tests_json" -t halt'
  const { stdout } = await execAsync(cmd, { cwd: CONFIG.REPO_PATH })
  
  // Parse JSON output
  const jsonMatch = stdout.match(/\{[\s\S]*\}/)
  const jsonResults = JSON.parse(jsonMatch[0])
  
  return jsonResults  // Mocha-compatible format
}
```

This enables:
- Unified data processing for all test types
- Individual test timing in Excel reports
- JSON export with complete test details

## Implementation Details

The reporter works by:
1. Capturing PL-Unit's output stream
2. Parsing test result lines (format: `[N/M] suite:test ... result (X.XXX sec)`)
3. Extracting test names, durations, and pass/fail status
4. Building Mocha-compatible JSON structure
5. Outputting to stdout for consumption by Node.js scripts

## Limitations

- Does not capture individual test error messages (uses generic "Test failed")
- Cannot capture pending/skipped tests (PL-Unit doesn't have this concept)
- Relies on parsing PL-Unit's text output format

## Example

Given this test suite:

```prolog
:- begin_tests(math_tests).

test(addition) :-
    assertion(2 + 2 =:= 4).

test(subtraction) :-
    assertion(5 - 3 =:= 2).

:- end_tests(math_tests).
```

Output:

```json
{
  "stats": {
    "suites": 1,
    "tests": 2,
    "passes": 2,
    "pending": 0,
    "failures": 0,
    "start": "2025-10-15T18:00:00+02:00",
    "end": "2025-10-15T18:00:00+02:00",
    "duration": 10
  },
  "tests": [
    {
      "title": "math_tests:addition",
      "fullTitle": "math_tests:addition",
      "duration": 0,
      "currentRetry": 0,
      "speed": "fast",
      "err": {},
      "state": "passed"
    },
    {
      "title": "math_tests:subtraction",
      "fullTitle": "math_tests:subtraction",
      "duration": 0,
      "currentRetry": 0,
      "speed": "fast",
      "err": {},
      "state": "passed"
    }
  ],
  "pending": [],
  "failures": [],
  "passes": [ /* same as tests array */ ]
}
```

## See Also

- `benchmark_excel.js` - Uses this reporter for unified benchmarking
- `BENCHMARK_README.md` - Documentation for the full benchmark suite
- Mocha JSON Reporter: https://mochajs.org/#json
