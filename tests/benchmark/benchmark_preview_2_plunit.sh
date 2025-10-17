#!/bin/bash
# Step 2: Run PL-Unit test and save JSON

cd "$(dirname "$0")/.."

echo "Running PL-Unit test (sample_tests)..."
time swipl -g "use_module(library(plunit)), use_module('src/core/util/plunit_json_reporter'), consult('tests/test_json_reporter.pl'), run_tests_json(sample_tests)" -t halt 2>&1 > tests/benchmark_results/preview_plunit_raw.txt

# Extract JSON
python3 -c "
import re
with open('tests/benchmark_results/preview_plunit_raw.txt', 'r') as f:
    content = f.read()
match = re.search(r'\{.*\}', content, re.DOTALL)
if match:
    with open('tests/benchmark_results/preview_plunit.json', 'w') as out:
        out.write(match.group(0))
"

if [ -s tests/benchmark_results/preview_plunit.json ]; then
    echo "✅ PL-Unit test completed"
    cat tests/benchmark_results/preview_plunit.json | python3 -c "
import sys, json
data = json.load(sys.stdin)
print(f'   Tests: {data[\"stats\"][\"tests\"]}')
print(f'   Passes: {data[\"stats\"][\"passes\"]}')
print(f'   Duration: {data[\"stats\"][\"duration\"]}ms')
"
else
    echo "❌ PL-Unit test failed - no JSON output"
    echo "Raw output saved in: tests/benchmark_results/preview_plunit_raw.txt"
    exit 1
fi
