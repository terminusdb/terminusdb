# TerminusDB Benchmark Suite

Comprehensive benchmarking tools for performance testing and comparison.

## Quick Start

```bash
# From repository root
cd tests/benchmark

# Compare two branches
bash benchmark_compare.sh main feature/optimization 10

# Quick preview of current branch
bash benchmark_preview.sh
```

## Tools

### Branch Comparison
- **`benchmark_compare.sh`** - Compare performance between two Git branches
- **`benchmark_compare_excel.py`** - Generate Excel reports with statistical analysis
- See `BENCHMARK_COMPARISON_README.md` for details

### Quick Testing
- **`benchmark_preview.sh`** - Fast benchmark of current branch
- **`benchmark_preview_multi.sh`** - Multiple benchmark types in one run
- See `BENCHMARK_README.md` for details

### Supporting Files
- **`benchmark_excel.js`** - Node.js benchmark runner with JSON output
- **`benchmark_*.js`** - Various benchmark test suites
- **`PLUNIT_JSON_REPORTER.md`** - Documentation for PL-Unit JSON reporter

## Directory Structure

```
tests/benchmark/
├── README.md                          # This file
├── BENCHMARK_COMPARISON_README.md     # Branch comparison docs
├── BENCHMARK_README.md                # Quick benchmark docs
├── PLUNIT_JSON_REPORTER.md           # JSON reporter docs
├── benchmark_compare.sh               # Main comparison tool
├── benchmark_compare_excel.py         # Excel report generator
├── benchmark_preview*.sh              # Quick preview tools
├── benchmark_*.js                     # Benchmark test suites
└── benchmark_results/                 # Output directory (gitignored)
```

## Requirements

- **SWI-Prolog** with PL-Unit
- **Node.js** with Mocha
- **Python 3** with openpyxl (for Excel reports)

Install Python dependencies:
```bash
pip3 install openpyxl
```

## Usage Examples

### Compare Branches
```bash
# Compare main vs feature branch with 10 runs
bash benchmark_compare.sh main feature/new-index 10

# Generate Excel report
python3 benchmark_compare_excel.py benchmark_results/
```

### Quick Performance Check
```bash
# Single quick run
bash benchmark_preview.sh

# Multiple benchmark types
bash benchmark_preview_multi.sh
```

## Configuration

Edit the arrays in `benchmark_compare.sh` to select test suites:

```bash
# Mocha tests (JavaScript)
MOCHA_TESTS=(
    "capabilities.js"
    "graphql.js"
)

# PL-Unit tests (Prolog)
PLUNIT_TESTS=(
    "api_prefixes"
    "json_read_term"
)
```

## Output

Results are saved to `benchmark_results/` with timestamps:
- `benchmark_results_YYYYMMDD_HHMMSS/` - Comparison results
- JSON files for each test run
- Excel reports with statistical analysis
