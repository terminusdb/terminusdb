#!/bin/bash
# Branch Comparison Benchmark Tool
#
# Usage: bash benchmark_compare.sh <branch1> <branch2> <num_runs>
# Example: bash benchmark_compare.sh main feature/optimization 10

set -e

# ============================================================================
# CONFIGURATION - Edit .env file or these defaults to select test suites
# ============================================================================

# Load configuration from .env file if it exists
ENV_FILE="$(dirname "$0")/.env"
if [ -f "$ENV_FILE" ]; then
    echo "Loading configuration from .env file..."
    source "$ENV_FILE"
    echo "✓ Configuration loaded"
    echo ""
fi

# Default Mocha test files (used if not set in .env)
# Space-separated list or "auto" to discover all
if [ -z "${MOCHA_TESTS_CONFIG+x}" ]; then
    MOCHA_TESTS_CONFIG="db-auth"
fi

# Default PL-Unit test suites (used if not set in .env)
# Space-separated list or "auto" to discover all
if [ -z "${PLUNIT_TESTS_CONFIG+x}" ]; then
    PLUNIT_TESTS_CONFIG="typecast"
fi

# Convert space-separated strings to arrays
if [ "$MOCHA_TESTS_CONFIG" = "auto" ]; then
    MOCHA_TESTS=("auto")
else
    read -ra MOCHA_TESTS <<< "$MOCHA_TESTS_CONFIG"
fi

if [ "$PLUNIT_TESTS_CONFIG" = "auto" ]; then
    PLUNIT_TESTS=("auto")
else
    read -ra PLUNIT_TESTS <<< "$PLUNIT_TESTS_CONFIG"
fi

# Test timeout in milliseconds
TIMEOUT=30000

# ============================================================================
# Command Line Arguments
# ============================================================================

# Check for --allow-dirty flag (can be anywhere in arguments)
ALLOW_DIRTY=""
for arg in "$@"; do
    if [ "$arg" = "--allow-dirty" ]; then
        ALLOW_DIRTY="--allow-dirty"
    fi
done

# Filter out --allow-dirty from positional arguments
ARGS=()
for arg in "$@"; do
    if [ "$arg" != "--allow-dirty" ]; then
        ARGS+=("$arg")
    fi
done

if [ "${#ARGS[@]}" -lt 3 ]; then
    echo "Usage: $0 [--allow-dirty] <branch1> <branch2> <num_runs>"
    echo ""
    echo "Examples:"
    echo "  $0 main feature/new-index 10"
    echo "  $0 v1.0 v1.1 5"
    echo "  $0 --allow-dirty main develop 5    # Skip git clean check"
    echo "  $0 main develop 5 --allow-dirty    # Flag can be anywhere"
    echo ""
    echo "Options:"
    echo "  --allow-dirty    Skip check for uncommitted changes (for development)"
    echo ""
    echo "Configuration:"
    echo "  Create a .env file in tests/benchmark/ to configure test suites."
    echo "  See .env.example for available options."
    exit 1
fi

BRANCH1="${ARGS[0]}"
BRANCH2="${ARGS[1]}"
NUM_RUNS="${ARGS[2]}"

cd "$(dirname "$0")/../.."
REPO_ROOT=$(pwd)

# Check for uncommitted changes before starting (unless --allow-dirty)
if [ "$ALLOW_DIRTY" != "--allow-dirty" ]; then
    if ! git diff-index --quiet HEAD -- 2>/dev/null; then
        echo "❌ ERROR: You have uncommitted changes."
        echo ""
        echo "Git status:"
        git status --short
        echo ""
        echo "Please commit or stash your changes before running the benchmark:"
        echo "  git commit -am 'Your message'"
        echo "  OR"
        echo "  git stash"
        echo "  OR"
        echo "  bash $0 $BRANCH1 $BRANCH2 $NUM_RUNS --allow-dirty    # Skip this check"
        echo ""
        exit 1
    fi
else
    echo "⚠️  WARNING: Running with uncommitted changes (--allow-dirty)"
    echo ""
fi

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

# Expand "auto" for PL-Unit tests
if [ "${#PLUNIT_TESTS[@]}" -eq 1 ] && [ "${PLUNIT_TESTS[0]}" = "auto" ]; then
    PLUNIT_TESTS=($(discover_plunit_suites))
    echo ""
fi

echo "═══════════════════════════════════════════════════════════"
echo "BRANCH COMPARISON BENCHMARK"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Configuration:"
echo "  Branch 1: $BRANCH1"
echo "  Branch 2: $BRANCH2"
echo "  Runs per branch: $NUM_RUNS"
echo "  Mocha tests: ${#MOCHA_TESTS[@]} files"
echo "  PL-Unit tests: ${#PLUNIT_TESTS[@]} suites"
echo ""

# Clean and create results directories
echo "Cleaning previous benchmark results..."
rm -rf tests/benchmark_results/branch1
rm -rf tests/benchmark_results/branch2
mkdir -p tests/benchmark_results/branch1
mkdir -p tests/benchmark_results/branch2
echo "✓ Clean slate ready"
echo ""

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
    
    # Build (try 'make dev' first, fallback to 'make' for older branches)
    echo "Building..."
    if make -n dev > /dev/null 2>&1; then
        make clean && make dev > /dev/null 2>&1
    else
        make clean && make > /dev/null 2>&1
    fi
    
    # Restart test server (handle both old and new script names)
    echo "Restarting test server..."
    if [ -f "./tests/terminusdb-test-server.sh" ]; then
        ./tests/terminusdb-test-server.sh restart > /dev/null 2>&1
    elif [ -f "./tests/terminusdb.sh" ]; then
        ./tests/terminusdb.sh restart > /dev/null 2>&1
    fi
    sleep 3
    
    echo ""
    
    # Run PL-Unit tests (faster, so run first)
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
                # Quotes around $TEST_SUITE are required for Prolog
                swipl -f src/interactive.pl -g "run_tests_json('$TEST_SUITE')" -t halt \
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
    
    for TEST_FILE in "${MOCHA_TESTS[@]}"; do
        # Ensure .js extension (support both "db-auth" and "db-auth.js")
        if [[ "$TEST_FILE" != *.js ]]; then
            TEST_FILE="${TEST_FILE}.js"
        fi
        
        TEST_NAME=$(basename "$TEST_FILE" .js)
        
        # Skip if test doesn't exist in this branch
        if [ ! -f "tests/test/$TEST_FILE" ]; then
            echo "  Suite: $TEST_NAME (skipped - not in this branch)"
            continue
        fi
        
        echo "  Suite: $TEST_NAME"
        
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

run_branch_benchmarks "$BRANCH1" "tests/benchmark_results/branch1" "Branch 1"
run_branch_benchmarks "$BRANCH2" "tests/benchmark_results/branch2" "Branch 2"

# ============================================================================
# Switch back to BRANCH1 before Excel generation (Excel script may differ between branches)
# ============================================================================

echo ""
echo "Switching back to $BRANCH1 for report generation..."
git checkout "$BRANCH1" > /dev/null 2>&1
echo ""

# ============================================================================
# Generate comparison Excel
# ============================================================================

echo "═══════════════════════════════════════════════════════════"
echo "GENERATING COMPARISON REPORT"
echo "═══════════════════════════════════════════════════════════"
echo ""

python3 tests/benchmark/benchmark_compare_excel.py "$BRANCH1" "$BRANCH2" "$NUM_RUNS"

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "✅ BENCHMARK COMPARISON COMPLETE"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Results saved in: tests/benchmark_results/"
echo ""
echo "To modify test selection, create/edit tests/benchmark/.env"
echo "See tests/benchmark/.env.example for configuration options."
echo ""
