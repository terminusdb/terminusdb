#!/bin/bash
# Run 5 iterations of the preview benchmark

RUNS=5
cd "$(dirname "$0")/.."

echo "═══════════════════════════════════════════════════════════"
echo "BENCHMARK PREVIEW - 5 Iterations"
echo "═══════════════════════════════════════════════════════════"
echo ""

mkdir -p tests/benchmark_results/runs

# Run Mocha tests 5 times
echo "Running Mocha tests ($RUNS iterations)..."
echo "───────────────────────────────────────────────────────────"
for i in $(seq 1 $RUNS); do
    echo -n "  Run $i/$RUNS... "
    
    npx mocha tests/test/capabilities.js --timeout 30000 --reporter json > tests/benchmark_results/runs/mocha_run_${i}_raw.txt 2>&1
    
    # Extract JSON
    python3 -c "
import re
with open('tests/benchmark_results/runs/mocha_run_${i}_raw.txt', 'r') as f:
    content = f.read()
match = re.search(r'\{.*\}', content, re.DOTALL)
if match:
    with open('tests/benchmark_results/runs/mocha_run_${i}.json', 'w') as out:
        out.write(match.group(0))
"
    
    if [ -s tests/benchmark_results/runs/mocha_run_${i}.json ]; then
        echo "✓"
    else
        echo "✗ Failed"
    fi
done

echo ""

# Run PL-Unit tests 5 times
echo "Running PL-Unit tests ($RUNS iterations)..."
echo "───────────────────────────────────────────────────────────"
for i in $(seq 1 $RUNS); do
    echo -n "  Run $i/$RUNS... "
    
    swipl -g "use_module(library(plunit)), use_module('src/core/util/plunit_json_reporter'), consult('tests/test_json_reporter.pl'), run_tests_json(sample_tests)" -t halt 2>&1 > tests/benchmark_results/runs/plunit_run_${i}_raw.txt
    
    # Extract JSON
    python3 -c "
import re
with open('tests/benchmark_results/runs/plunit_run_${i}_raw.txt', 'r') as f:
    content = f.read()
match = re.search(r'\{.*\}', content, re.DOTALL)
if match:
    with open('tests/benchmark_results/runs/plunit_run_${i}.json', 'w') as out:
        out.write(match.group(0))
"
    
    if [ -s tests/benchmark_results/runs/plunit_run_${i}.json ]; then
        echo "✓"
    else
        echo "✗ Failed"
    fi
done

echo ""
echo "───────────────────────────────────────────────────────────"
echo "✅ All runs complete. Generating Excel report..."
echo ""

# Generate Excel with statistics
python3 tests/benchmark_preview_3_excel_multi.py

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "✅ COMPLETE"
echo "═══════════════════════════════════════════════════════════"
