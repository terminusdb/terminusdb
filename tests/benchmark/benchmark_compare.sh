#!/bin/bash
# Branch Comparison Benchmark Tool
#
# Usage: bash benchmark_compare.sh <branch1> <branch2> <num_runs>
# Example: bash benchmark_compare.sh main feature/optimization 10

set -e

cd "$(dirname "$0")/../.."

# ============================================================================
# CONFIGURATION - Edit these to select which test suites to run
# ============================================================================

# Mocha test files to benchmark (just filenames, not paths)
# Options:
# 1. "auto" - Discovers all .js test files (recommended)
# 2. Specific list - Only run these tests: MOCHA_TESTS=("graphql.js" "woql-noauth.js")
# 3. Empty - Skip Mocha tests: MOCHA_TESTS=()

#MOCHA_TESTS=("auto")  # Auto-discover all test files in tests/test/
MOCHA_TESTS=("woql-noauth.js")  # Minimal test

# PL-Unit test suites to benchmark
# Options:
# 1. "auto" - Auto-discover all test units (finds ~123 suites)
# 2. Curated list - Only stable tests: PLUNIT_TESTS=("typecast" "json" ...)
# 3. Empty - Skip PL-Unit tests: PLUNIT_TESTS=()

#PLUNIT_TESTS=("auto")  # Auto-discover all test units
PLUNIT_TESTS=("typecast")  # Minimal test

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


REPO_ROOT=$(pwd)

# Save the original branch to restore at the end
ORIGINAL_BRANCH=$(git branch --show-current)
if [ -z "$ORIGINAL_BRANCH" ]; then
    # In detached HEAD state, save the commit hash
    ORIGINAL_BRANCH=$(git rev-parse HEAD)
fi

# ============================================================================
# Pre-flight checks: Validate branch switching before running tests
# ============================================================================

echo "==================================================================="
echo "PRE-FLIGHT CHECKS"
echo "==================================================================="
echo ""

# Check for uncommitted changes
echo "Checking for uncommitted changes..."
if ! git diff-index --quiet HEAD -- 2>/dev/null; then
    echo "❌ ERROR: You have uncommitted changes in your working directory."
    echo ""
    echo "Git status:"
    git status --short
    echo ""
    echo "Options:"
    echo "  1. Commit your changes:    git commit -am 'Your message'"
    echo "  2. Stash your changes:     git stash"
    echo "  3. Discard your changes:   git reset --hard"
    echo ""
    exit 1
fi
echo "✓ No uncommitted changes"

# Check if branches exist
echo ""
echo "Validating branches..."
if ! git rev-parse --verify "$BRANCH1" >/dev/null 2>&1; then
    echo "❌ ERROR: Branch '$BRANCH1' does not exist"
    echo ""
    echo "Available branches:"
    git branch -a | head -20
    exit 1
fi
echo "✓ Branch '$BRANCH1' exists"

if ! git rev-parse --verify "$BRANCH2" >/dev/null 2>&1; then
    echo "❌ ERROR: Branch '$BRANCH2' does not exist"
    echo ""
    echo "Available branches:"
    git branch -a | head -20
    exit 1
fi
echo "✓ Branch '$BRANCH2' exists"

# Test branch switching
echo ""
echo "Testing branch switching (dry run)..."

# Try switching to branch1
echo -n "  Switching to $BRANCH1... "
if ! git checkout "$BRANCH1" 2>&1 | grep -q "Switched to\|Already on"; then
    echo "❌ FAILED"
    echo ""
    echo "Cannot switch to branch '$BRANCH1'."
    echo "This may be due to conflicts or other git issues."
    git checkout "$ORIGINAL_BRANCH" 2>/dev/null
    exit 1
fi
echo "✓"

# Try switching to branch2
echo -n "  Switching to $BRANCH2... "
if ! git checkout "$BRANCH2" 2>&1 | grep -q "Switched to\|Already on"; then
    echo "❌ FAILED"
    echo ""
    echo "Cannot switch to branch '$BRANCH2'."
    git checkout "$ORIGINAL_BRANCH" 2>/dev/null
    exit 1
fi
echo "✓"

# Return to original branch
echo -n "  Returning to $ORIGINAL_BRANCH... "
if ! git checkout "$ORIGINAL_BRANCH" 2>&1 | grep -q "Switched to\|Already on"; then
    echo "❌ FAILED"
    echo ""
    echo "Cannot return to original branch."
    exit 1
fi
echo "✓"

echo ""
echo "✅ All pre-flight checks passed!"
echo ""

# ============================================================================
# Auto-discover test suites if requested
# ============================================================================

discover_plunit_suites() {
    echo "Discovering PL-Unit test suites..." >&2
    
    # Verify we're in the repo root
    if [ ! -f "src/interactive.pl" ]; then
        echo "  ⚠ Warning: src/interactive.pl not found, using fallback list" >&2
        echo "typecast json json_read_term json_datatype arithmetic_document employee_documents"
        return
    fi
    
    # Use current_test_unit/2 to query loaded test units
    TEMP_SCRIPT=$(mktemp)
    TEMP_OUTPUT=$(mktemp)
    
    # Get absolute path to interactive.pl
    ABS_INTERACTIVE="$(pwd)/src/interactive.pl"
    
    cat > "$TEMP_SCRIPT" << EOF
:- use_module(library(plunit)).
:- consult('$ABS_INTERACTIVE').

:- initialization(discover_and_exit).

discover_and_exit :-
    findall(Unit, current_test_unit(Unit, _), Units),
    sort(Units, Sorted),
    forall(member(U, Sorted), (atom_string(U, Str), writeln(Str))),
    halt(0).

discover_and_exit :-
    halt(1).
EOF
    
    # Run with a timeout of 30 seconds (macOS doesn't have timeout, use perl)
    perl -e 'alarm shift; exec @ARGV' 30 swipl -f "$TEMP_SCRIPT" > "$TEMP_OUTPUT" 2>&1 || true
    
    # Extract only the test suite names (filter out errors/warnings)
    DISCOVERED=$(grep -v '^%' "$TEMP_OUTPUT" | grep -v '^Warning:' | grep -v '^ERROR:' | grep -v '^Unknown message:' | grep -v '^\s*$' | tr '\n' ' ')
    
    rm -f "$TEMP_SCRIPT" "$TEMP_OUTPUT"
    
    if [ -z "$DISCOVERED" ]; then
        echo "  ⚠ Warning: Could not discover test suites, using fallback list" >&2
        echo "typecast json json_read_term json_datatype arithmetic_document employee_documents"
    else
        SUITE_COUNT=$(echo "$DISCOVERED" | wc -w | tr -d ' ')
        echo "  Found $SUITE_COUNT test suites" >&2
        echo "$DISCOVERED"
    fi
}

# Expand "auto" for Mocha tests
if [ "${#MOCHA_TESTS[@]}" -eq 1 ] && [ "${MOCHA_TESTS[0]}" = "auto" ]; then
    echo "Discovering Mocha test files..."
    if [ -d "tests/test" ]; then
        MOCHA_TESTS=($(cd tests/test && ls *.js 2>/dev/null || echo ""))
        echo "  Discovered ${#MOCHA_TESTS[@]} Mocha test files"
    else
        echo "  ⚠ Warning: tests/test directory not found"
        MOCHA_TESTS=()
    fi
    echo ""
fi

# Expand "auto" to discovered suites
if [ "${#PLUNIT_TESTS[@]}" -eq 1 ] && [ "${PLUNIT_TESTS[0]}" = "auto" ]; then
    PLUNIT_TESTS=($(discover_plunit_suites))
    echo "  Discovered ${#PLUNIT_TESTS[@]} PL-Unit test suites"
    echo ""
fi

echo "================================================================="
echo "BRANCH COMPARISON BENCHMARK"
echo "================================================================="
echo ""
echo "Configuration:"
echo "  Branch 1: $BRANCH1"
echo "  Branch 2: $BRANCH2"
echo "  Runs per branch: $NUM_RUNS"
echo "  Mocha tests: ${#MOCHA_TESTS[@]} files"
echo "  PL-Unit tests: ${#PLUNIT_TESTS[@]} suites"
echo ""

# Create results directories
mkdir -p tests/benchmark/benchmark_results/branch1
mkdir -p tests/benchmark/benchmark_results/branch2

# ============================================================================
# Function to run benchmarks on a branch
# ============================================================================

run_branch_benchmarks() {
    local BRANCH=$1
    local OUTPUT_DIR=$2
    local BRANCH_LABEL=$3
    
    echo "================================================================="
    echo "BENCHMARKING: $BRANCH"
    echo "================================================================="
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
    
    # Run PL-Unit tests (faster, so run first)
    echo "Running PL-Unit tests ($NUM_RUNS runs)..."
    echo "───────────────────────────────────────────────────────────"
    
    PLUNIT_TOTAL=${#PLUNIT_TESTS[@]}
    PLUNIT_COUNT=0
    
    for TEST_SUITE in "${PLUNIT_TESTS[@]}"; do
        PLUNIT_COUNT=$((PLUNIT_COUNT + 1))
        echo "  Suite: $TEST_SUITE ($PLUNIT_COUNT/$PLUNIT_TOTAL)"
        
        for RUN in $(seq 1 $NUM_RUNS); do
            echo -n "    Run $RUN/$NUM_RUNS... "
            
            if [ "$TEST_SUITE" = "_" ]; then
                # Run all tests
                swipl -g "run_tests_json" -t halt -f src/interactive.pl \
                    2>&1 > "$OUTPUT_DIR/plunit_all_run_${RUN}_raw.txt"
            else
                # Run specific suite - interactive.pl loads plunit_json_reporter automatically
                swipl -g "consult('src/interactive.pl'), run_tests_json('$TEST_SUITE')" -t halt \
                    2>&1 > "$OUTPUT_DIR/plunit_${TEST_SUITE}_run_${RUN}_raw.txt"
            fi
            
            # Extract JSON
            SUITE_NAME=$([ "$TEST_SUITE" = "_" ] && echo "all" || echo "$TEST_SUITE")
            python3 -c "
import re
import json
with open('$OUTPUT_DIR/plunit_${SUITE_NAME}_run_${RUN}_raw.txt', 'r') as f:
    content = f.read()
match = re.search(r'\{.*\}', content, re.DOTALL)
if match:
    with open('$OUTPUT_DIR/plunit_${SUITE_NAME}_run_${RUN}.json', 'w') as out:
        out.write(match.group(0))
"
            
            # Check if test exists (has tests > 0) or if file was created
            if [ -s "$OUTPUT_DIR/plunit_${SUITE_NAME}_run_${RUN}.json" ]; then
                # Check if the test suite actually ran (stats.tests > 0 or no error)
                HAS_TESTS=$(python3 -c "
import json
try:
    with open('$OUTPUT_DIR/plunit_${SUITE_NAME}_run_${RUN}.json', 'r') as f:
        data = json.load(f)
        print(1 if data.get('stats', {}).get('tests', 0) > 0 else 0)
except:
    print(0)
" 2>/dev/null || echo 0)
                
                if [ "$HAS_TESTS" -eq 1 ]; then
                    echo "✓"
                else
                    echo "⊘ (suite not found in this branch)"
                fi
            else
                echo "✗ Failed"
            fi
        done
    done
    
    echo ""
    
    # Run Mocha tests
    echo "Running Mocha tests ($NUM_RUNS runs)..."
    echo "───────────────────────────────────────────────────────────"
    
    MOCHA_TOTAL=${#MOCHA_TESTS[@]}
    MOCHA_COUNT=0
    
    for TEST_FILE in "${MOCHA_TESTS[@]}"; do
        MOCHA_COUNT=$((MOCHA_COUNT + 1))
        TEST_NAME=$(basename "$TEST_FILE" .js)
        
        # Skip if test doesn't exist in this branch
        if [ ! -f "tests/test/$TEST_FILE" ]; then
            echo "  Suite: $TEST_NAME ($MOCHA_COUNT/$MOCHA_TOTAL) (skipped - not in this branch)"
            continue
        fi
        
        echo "  Suite: $TEST_NAME ($MOCHA_COUNT/$MOCHA_TOTAL)"
        
        for RUN in $(seq 1 $NUM_RUNS); do
            echo -n "    Run $RUN/$NUM_RUNS... "
            
            # Capture exit code
            set +e
            npx mocha "tests/test/$TEST_FILE" --timeout $TIMEOUT --reporter json \
                > "$OUTPUT_DIR/mocha_${TEST_NAME}_run_${RUN}_raw.txt" 2>&1
            EXIT_CODE=$?
            set -e
            
            # Extract JSON and check for failures
            RESULT=$(python3 -c "
import re
import json
import sys
try:
    with open('$OUTPUT_DIR/mocha_${TEST_NAME}_run_${RUN}_raw.txt', 'r') as f:
        content = f.read()
    match = re.search(r'\{.*\}', content, re.DOTALL)
    if match:
        with open('$OUTPUT_DIR/mocha_${TEST_NAME}_run_${RUN}.json', 'w') as out:
            out.write(match.group(0))
        data = json.loads(match.group(0))
        failures = data.get('stats', {}).get('failures', 0)
        tests = data.get('stats', {}).get('tests', 0)
        if tests == 0:
            print('no_tests')
        elif failures > 0:
            print('failed')
        else:
            print('passed')
    else:
        print('no_json')
except Exception as e:
    print('error')
" 2>/dev/null || echo "error")
            
            if [ "$RESULT" = "passed" ]; then
                echo "✓"
            elif [ "$RESULT" = "failed" ]; then
                echo "✗ (test failures)"
            elif [ "$RESULT" = "no_tests" ]; then
                echo "⊘ (no tests found)"
            else
                echo "✗ Failed to parse"
            fi
        done
    done
    
    echo ""
}

# ============================================================================
# Run benchmarks on both branches
# ============================================================================

run_branch_benchmarks "$BRANCH1" "tests/benchmark/benchmark_results/branch1" "Branch 1"
run_branch_benchmarks "$BRANCH2" "tests/benchmark/benchmark_results/branch2" "Branch 2"

# ============================================================================
# Generate comparison Excel
# ============================================================================

echo "================================================================="
echo "GENERATING COMPARISON REPORT"
echo "================================================================="
echo ""

python3 tests/benchmark/benchmark_compare_excel.py "$BRANCH1" "$BRANCH2" "$NUM_RUNS"

echo ""
echo "================================================================="
echo "✅ BENCHMARK COMPARISON COMPLETE"
echo "================================================================="
echo ""
echo "Results saved in: tests/benchmark/benchmark_results/"
echo ""
echo "To modify test selection, edit the MOCHA_TESTS and"
echo "PLUNIT_TESTS arrays at the top of this script."
echo ""

# Restore the original branch
echo "Restoring original branch: $ORIGINAL_BRANCH"
git checkout "$ORIGINAL_BRANCH" > /dev/null 2>&1
