# Branch Comparison Benchmark Tool

Comprehensive tool for comparing performance between two Git branches with statistical analysis.

## Quick Start

```bash
cd tests
bash benchmark_compare.sh <branch1> <branch2> <num_runs>
```

**Example:**
```bash
bash benchmark_compare.sh main feature/optimization 10
```

## Features

✅ **Automated branch switching and building**  
✅ **Multiple test runs with statistics** (average, std dev, min, max)  
✅ **Side-by-side branch comparison** in Excel  
✅ **Easy test suite selection** via configuration arrays  
✅ **Mocha and PL-Unit support**  
✅ **Color-coded performance differences**

## Configuration

Edit the arrays at the top of `benchmark_compare.sh`:

### Mocha Tests

```bash
# Single test file
MOCHA_TESTS=(
    "capabilities.js"
)

# Multiple test files
MOCHA_TESTS=(
    "capabilities.js"
    "auth.js"
    "branch-auth.js"
)

# All test files (discover automatically)
MOCHA_TESTS=($(cd tests/test && ls *.js))
```

### PL-Unit Tests

```bash
# Single suite
PLUNIT_TESTS=(
    "json_read_term"
)

# Multiple suites
PLUNIT_TESTS=(
    "json_read_term"
    "json_read_term_stream"
    "json_read_list_stream"
)

# All tests
PLUNIT_TESTS=(
    "_"
)
```

## Excel Output Structure

### Comparison Tab (Overview)
- Side-by-side statistics for both branches
- Performance difference in ms and %
- Color-coded to show which branch is faster
- Identifies tests unique to each branch

### Test Detail Tabs
- Separate sheets for each branch: `<suite>_b1` and `<suite>_b2`
- Suite and test name columns
- All run durations in individual columns
- Statistics: Average, Std Dev, Min, Max
- Speed classification and status

## Example Output

```
Comparison Tab:
┌────────────────┬───────┬─────────┬─────────┬────────┬─────────┬────────┐
│ Test Suite     │ Tests │ Branch  │ Avg(ms) │ StdDev │ Diff(ms)│ Faster │
├────────────────┼───────┼─────────┼─────────┼────────┼─────────┼────────┤
│ capabilities   │   8   │ main    │  5733   │  68.8  │         │        │
│                │   8   │ feature │  5124   │  45.2  │  -609   │ feature│
└────────────────┴───────┴─────────┴─────────┴────────┴─────────┴────────┘
```

## Usage Examples

### Compare Current Branch vs Main (5 runs)
```bash
bash benchmark_compare.sh main HEAD 5
```

### Compare Two Feature Branches (10 runs)
```bash
bash benchmark_compare.sh feature/caching feature/indexing 10
```

### Quick Comparison (3 runs)
```bash
bash benchmark_compare.sh v1.0 v1.1 3
```

### Full Benchmark (20 runs)
```bash
bash benchmark_compare.sh main develop 20
```

## What Gets Tested

For each branch, the tool:
1. Checks out the branch
2. Runs `make dev` to build
3. Restarts the test server
4. Runs each Mocha test suite N times
5. Runs each PL-Unit test suite N times
6. Captures JSON output with test details and timings

## Output Files

Results are saved in `tests/benchmark_results/`:
- `branch1/` - All runs for first branch
- `branch2/` - All runs for second branch
- `benchmark_comparison_<branch1>_vs_<branch2>_<timestamp>.xlsx` - Comparison report

## Tips

### Selecting Specific Tests

**Find available Mocha tests:**
```bash
ls tests/test/*.js
```

**Find available PL-Unit suites:**
```bash
swipl -g "consult('src/interactive.pl'), forall(current_test_unit(Unit, _), writeln(Unit))" -t halt 2>&1 | grep -v "%" | sort -u
```

### Performance Analysis

- **Low std dev** (< 5% of mean) = Consistent performance
- **High std dev** (> 10% of mean) = Variable performance, may need more runs
- **Difference > 10%** = Significant performance impact worth investigating

### Troubleshooting

**Build fails:**
```bash
# Clean build first
make clean
make dev
```

**Test server won't start:**
```bash
./tests/terminusdb-test-server.sh stop
./tests/terminusdb-test-server.sh start
```

**Missing PL-Unit test:**
```bash
# Verify test exists
swipl -g "consult('src/interactive.pl'), current_test_unit(your_test_name, _)" -t halt
```

## Integration with CI/CD

Example GitHub Actions workflow:

```yaml
name: Performance Comparison
on:
  pull_request:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Run benchmark
        run: |
          cd tests
          bash benchmark_compare.sh main ${{ github.head_ref }} 5
      - name: Upload results
        uses: actions/upload-artifact@v2
        with:
          name: benchmark-results
          path: tests/benchmark_results/*.xlsx
```

## Advanced Configuration

### Custom Timeout

Edit in `benchmark_compare.sh`:
```bash
TIMEOUT=60000  # 60 seconds
```

### Test Discovery

To auto-discover all tests:

```bash
# In benchmark_compare.sh
MOCHA_TESTS=($(cd tests/test && ls *.js | grep -v "disabled"))
PLUNIT_TESTS=("_")  # Underscore means all PL-Unit tests
```

### Filtering Tests

```bash
# Only integration tests
MOCHA_TESTS=($(cd tests/test && ls *-auth.js))

# Only specific PL-Unit modules
PLUNIT_TESTS=($(swipl -g "..." -t halt | grep "^json_"))
```

## See Also

- `benchmark_preview.sh` - Quick preview with single run
- `benchmark_preview_multi.sh` - Multiple runs on current branch
- `BENCHMARK_README.md` - Full benchmark suite documentation
- `PLUNIT_JSON_REPORTER.md` - PL-Unit JSON reporter details
