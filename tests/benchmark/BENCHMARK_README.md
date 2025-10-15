# TerminusDB Test Performance Benchmark

## Overview

This benchmark suite provides comprehensive performance analysis of the TerminusDB test suites, comparing the current branch (`128-fix-the-mess`) with the main branch.

## What It Measures

The benchmark runs **10 iterations** of each test suite on both branches:

1. **Integration Tests (from repo root)**: `npx mocha tests/test/*.js`
2. **Integration Tests (from tests/ dir)**: `cd tests && npx mocha`
3. **PL-Unit Tests**: `swipl -g run_tests -t halt -f src/interactive.pl`

## Features

- ✅ 10 runs per test suite per branch (60 total test executions)
- ✅ Statistical analysis (mean, median, std dev, min, max)
- ✅ Individual test timing comparison (top 50 slowest tests)
- ✅ **Unified JSON output** format (Mocha-compatible) for all test types
- ✅ Excel report with 3 tabs:
  - **Summary**: High-level comparison with percentage differences
  - **Detailed Runs**: All 10 runs for each test suite
  - **Individual Tests**: Per-test performance comparison
- ✅ JSON data export for further analysis
- ✅ Automatic branch switching and rebuilding
- ✅ Server restart between branches

## Prerequisites

```bash
# Install xlsx package for Excel generation
npm install xlsx
```

## Usage

### Full Benchmark with Excel Report

```bash
cd /Users/hoijnet/Code/terminusdb/cleodb/tests
node benchmark_excel.js
```

This will:
1. Run 10 iterations on current branch (128-fix-the-mess)
2. Switch to main branch, rebuild, and run 10 iterations
3. Switch back to current branch
4. Generate Excel report in `tests/benchmark_results/`

**Expected duration**: 60-90 minutes (depending on system performance)

### CSV-based Benchmark (lighter alternative)

```bash
cd /Users/hoijnet/Code/terminusdb/cleodb/tests
node benchmark_tests.js
```

Generates CSV files instead of Excel (no xlsx dependency required).

## Output

### Excel Report Structure

**Tab 1: Summary**
- High-level statistics for each test suite
- Mean, median, standard deviation, min/max times
- Percentage difference between branches
- Visual comparison

**Tab 2: Detailed Runs**
- Individual timings for all 10 runs
- Timestamp for each run
- Allows tracking consistency/variance

**Tab 3: Individual Tests**
- Top 50 slowest tests
- Per-test comparison between branches
- Categorized as Faster/Slower/Similar
- Shows exact ms difference and percentage

### File Location

```
tests/benchmark_results/
└── benchmark_128-fix-the-mess_vs_main_2025-10-15.xlsx
```

## Interpreting Results

### Performance Categories

- **Faster**: Current branch is >5ms faster (positive result ✓)
- **Slower**: Current branch is >5ms slower (needs investigation ⚠️)
- **Similar**: Difference <5ms (no significant impact)

### Expected Results

The path resolution fixes should show:
- **No significant performance regression** (<2% difference)
- **Consistent timing across both run locations** (root vs tests/)
- **Individual test times remain stable**

### Warning Signs

If you see:
- >5% performance regression consistently
- High standard deviation (>10% of mean)
- Specific tests showing major slowdowns (>50ms difference)

Then further investigation is needed.

## Troubleshooting

### "xlsx package not found"
```bash
npm install xlsx
```

### "Could not find terminusdb.sh"
Make sure you run from the `tests/` directory or repo root.

### Branch switching fails
Ensure working directory is clean:
```bash
git stash
```

### Test server won't start
```bash
./tests/terminusdb-test-server.sh stop
./tests/terminusdb-test-server.sh start
```

## Technical Details

### PL-Unit JSON Reporter

The benchmark uses a built-in JSON reporter (`src/core/util/plunit_json_reporter.pl`) that outputs PL-Unit test results in Mocha-compatible JSON format. This enables:
- Unified data structure across JavaScript (Mocha) and Prolog (PL-Unit) tests
- Individual test timing for every PL-Unit test
- Easy parsing and analysis in JavaScript
- Consistent Excel export format

**The reporter is built-in** and automatically available via the `run_tests_json/0` predicate in all TerminusDB entry points.

Usage standalone:
```bash
swipl -g run_tests_json -t halt -f src/interactive.pl
```

Output format matches Mocha's JSON reporter with `stats`, `tests`, `passes`, and `failures` arrays.

### Statistical Methods

- **Mean**: Average of all 10 runs
- **Median**: Middle value (reduces impact of outliers)
- **Std Dev**: Measure of consistency (lower is better)
- **Min/Max**: Shows range of performance

### Why 10 Runs?

10 runs provides:
- Good statistical confidence
- Outlier detection
- Reasonable execution time
- Variance measurement

### Benchmark Isolation

The script:
- Rebuilds binary after branch switch
- Restarts test server
- Waits 3 seconds for server stabilization
- Uses consistent timeout settings

## Example Output

```
═══════════════════════════════════════════════════════
BENCHMARK COMPLETE - SUMMARY
═══════════════════════════════════════════════════════

Integration (root):
  128-fix-the-mess: 295432ms
  main: 294821ms
  Difference: +611ms (+0.21%)
  Result: ≈ No significant difference

Integration (tests):
  128-fix-the-mess: 293145ms
  main: 293089ms
  Difference: +56ms (+0.02%)
  Result: ≈ No significant difference

PL-Unit:
  128-fix-the-mess: 45231ms
  main: 45189ms
  Difference: +42ms (+0.09%)
  Result: ≈ No significant difference
```

## Notes

- The benchmark modifies git branches - ensure clean working directory
- Server logs are redirected to avoid cluttering output
- Individual test timings only available for integration tests (mocha JSON reporter)
- PL-Unit tests measured as total suite time only
- All timings in milliseconds

## Questions?

See the main CONTRIBUTING.md for test documentation.
