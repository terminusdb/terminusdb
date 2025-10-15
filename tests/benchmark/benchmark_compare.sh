#!/bin/bash
# Branch Comparison Benchmark Tool
#
# Usage: bash benchmark_compare.sh <branch1> <branch2> <num_runs>
# Example: bash benchmark_compare.sh main feature/optimization 10

set -e

# ============================================================================
# CONFIGURATION - Edit these to select which test suites to run
# ============================================================================

# Mocha test files to benchmark (relative to tests/test/)
# Examples:
#   - Single file: MOCHA_TESTS=("capabilities.js")
#   - Multiple files: MOCHA_TESTS=("capabilities.js" "auth.js" "branch-auth.js")
#   - All files: MOCHA_TESTS=($(ls tests/test/*.js))
MOCHA_TESTS=(
    "capabilities.js"
)

# PL-Unit test suites to benchmark
# Examples:
#   - Single suite: PLUNIT_TESTS=("json_read_term")
#   - Multiple suites: PLUNIT_TESTS=("json_read_term" "json_read_term_stream")
#   - All tests: PLUNIT_TESTS=("_")  # underscore means all tests
PLUNIT_TESTS=(
    "api_prefixes"
)

# Test timeout in milliseconds
TIMEOUT=30000

# ============================================================================
# Command Line Arguments
# ============================================================================

if [ "$#" -lt 3 ]; then
    echo "Usage: $0 <branch1> <branch2> <num_runs>"
    echo ""
    echo "Examples:"
    echo "  $0 main feature/new-index 10"
    echo "  $0 v1.0 v1.1 5"
    echo ""
    echo "Configuration:"
    echo "  Edit MOCHA_TESTS and PLUNIT_TESTS arrays in this script"
    echo "  to select which test suites to run."
    exit 1
fi

BRANCH1="$1"
BRANCH2="$2"
NUM_RUNS="$3"

cd "$(dirname "$0")/../.."
REPO_ROOT=$(pwd)

echo "═══════════════════════════════════════════════════════════"
echo "BRANCH COMPARISON BENCHMARK"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Configuration:"
echo "  Branch 1: $BRANCH1"
echo "  Branch 2: $BRANCH2"
echo "  Runs per branch: $NUM_RUNS"
echo "  Mocha tests: ${MOCHA_TESTS[*]}"
echo "  PL-Unit tests: ${PLUNIT_TESTS[*]}"
echo ""

# Create results directories
mkdir -p tests/benchmark_results/branch1
mkdir -p tests/benchmark_results/branch2

# ============================================================================
# Function to run benchmarks on a branch
# ============================================================================

run_branch_benchmarks() {
    local BRANCH=$1
    local OUTPUT_DIR=$2
    local BRANCH_LABEL=$3
    
    echo "═══════════════════════════════════════════════════════════"
    echo "BENCHMARKING: $BRANCH"
    echo "═══════════════════════════════════════════════════════════"
    echo ""
    
    # Checkout branch (only if not already on it)
    # Check if in rebase state or get current branch
    if [ -d "$REPO_ROOT/.git/rebase-merge" ] || [ -d "$REPO_ROOT/.git/rebase-apply" ]; then
        # In rebase - read the branch name from rebase state
        if [ -f "$REPO_ROOT/.git/rebase-merge/head-name" ]; then
            CURRENT_BRANCH=$(cat "$REPO_ROOT/.git/rebase-merge/head-name" | sed 's|refs/heads/||')
        else
            CURRENT_BRANCH=$(git symbolic-ref --short HEAD 2>/dev/null || echo "")
        fi
    else
        CURRENT_BRANCH=$(git branch --show-current)
    fi
    
    if [ "$CURRENT_BRANCH" = "$BRANCH" ]; then
        echo "Already on branch: $BRANCH (skipping checkout)"
    else
        echo "Switching to branch: $BRANCH"
        git checkout "$BRANCH" 2>&1 | grep -v "Already on"
    fi
    
    # Build
    echo "Building..."
    make dev > /dev/null 2>&1
    
    # Restart test server
    echo "Restarting test server..."
    ./tests/terminusdb-test-server.sh restart > /dev/null 2>&1
    sleep 3
    
    echo ""
    
    # Run Mocha tests
    echo "Running Mocha tests ($NUM_RUNS runs)..."
    echo "───────────────────────────────────────────────────────────"
    
    for TEST_FILE in "${MOCHA_TESTS[@]}"; do
        TEST_NAME=$(basename "$TEST_FILE" .js)
        echo "  Suite: $TEST_NAME"
        
        for RUN in $(seq 1 $NUM_RUNS); do
            echo -n "    Run $RUN/$NUM_RUNS... "
            
            npx mocha "tests/test/$TEST_FILE" --timeout $TIMEOUT --reporter json \
                > "$OUTPUT_DIR/mocha_${TEST_NAME}_run_${RUN}_raw.txt" 2>&1
            
            # Extract JSON
            python3 -c "
import re
with open('$OUTPUT_DIR/mocha_${TEST_NAME}_run_${RUN}_raw.txt', 'r') as f:
    content = f.read()
match = re.search(r'\{.*\}', content, re.DOTALL)
if match:
    with open('$OUTPUT_DIR/mocha_${TEST_NAME}_run_${RUN}.json', 'w') as out:
        out.write(match.group(0))
"
            
            if [ -s "$OUTPUT_DIR/mocha_${TEST_NAME}_run_${RUN}.json" ]; then
                echo "✓"
            else
                echo "✗ Failed"
            fi
        done
    done
    
    echo ""
    
    # Run PL-Unit tests
    echo "Running PL-Unit tests ($NUM_RUNS runs)..."
    echo "───────────────────────────────────────────────────────────"
    
    for TEST_SUITE in "${PLUNIT_TESTS[@]}"; do
        echo "  Suite: $TEST_SUITE"
        
        for RUN in $(seq 1 $NUM_RUNS); do
            echo -n "    Run $RUN/$NUM_RUNS... "
            
            if [ "$TEST_SUITE" = "_" ]; then
                # Run all tests
                swipl -g "run_tests_json" -t halt -f src/interactive.pl \
                    2>&1 > "$OUTPUT_DIR/plunit_all_run_${RUN}_raw.txt"
            else
                # Run specific suite - interactive.pl loads plunit_json_reporter automatically
                swipl -g "consult('src/interactive.pl'), run_tests_json($TEST_SUITE)" -t halt \
                    2>&1 > "$OUTPUT_DIR/plunit_${TEST_SUITE}_run_${RUN}_raw.txt"
            fi
            
            # Extract JSON
            SUITE_NAME=$([ "$TEST_SUITE" = "_" ] && echo "all" || echo "$TEST_SUITE")
            python3 -c "
import re
with open('$OUTPUT_DIR/plunit_${SUITE_NAME}_run_${RUN}_raw.txt', 'r') as f:
    content = f.read()
match = re.search(r'\{.*\}', content, re.DOTALL)
if match:
    with open('$OUTPUT_DIR/plunit_${SUITE_NAME}_run_${RUN}.json', 'w') as out:
        out.write(match.group(0))
"
            
            if [ -s "$OUTPUT_DIR/plunit_${SUITE_NAME}_run_${RUN}.json" ]; then
                echo "✓"
            else
                echo "✗ Failed"
            fi
        done
    done
    
    echo ""
}

# ============================================================================
# Run benchmarks on both branches
# ============================================================================

run_branch_benchmarks "$BRANCH1" "tests/benchmark_results/branch1" "Branch 1"
run_branch_benchmarks "$BRANCH2" "tests/benchmark_results/branch2" "Branch 2"

# ============================================================================
# Generate comparison Excel
# ============================================================================

echo "═══════════════════════════════════════════════════════════"
echo "GENERATING COMPARISON REPORT"
echo "═══════════════════════════════════════════════════════════"
echo ""

python3 tests/benchmark_compare_excel.py "$BRANCH1" "$BRANCH2" "$NUM_RUNS"

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "✅ BENCHMARK COMPARISON COMPLETE"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Results saved in: tests/benchmark_results/"
echo ""
echo "To modify test selection, edit the MOCHA_TESTS and"
echo "PLUNIT_TESTS arrays at the top of this script."
echo ""
