#!/bin/bash
# Step 1: Run Mocha test and save JSON

cd "$(dirname "$0")/.."

echo "Running Mocha test (capabilities.js)..."

# Run mocha and capture output to temp file
time npx mocha tests/test/capabilities.js --timeout 30000 --reporter json > tests/benchmark_results/preview_mocha_raw.txt 2>&1

# Extract just the JSON part (between first { and last })
python3 -c "
import re
with open('tests/benchmark_results/preview_mocha_raw.txt', 'r') as f:
    content = f.read()
# Find the JSON object
match = re.search(r'\{.*\}', content, re.DOTALL)
if match:
    with open('tests/benchmark_results/preview_mocha.json', 'w') as out:
        out.write(match.group(0))
"

if [ $? -eq 0 ] && [ -s tests/benchmark_results/preview_mocha.json ]; then
    echo "✅ Mocha test completed"
    cat tests/benchmark_results/preview_mocha.json | python3 -c "
import sys, json
data = json.load(sys.stdin)
print(f'   Tests: {data[\"stats\"][\"tests\"]}')
print(f'   Passes: {data[\"stats\"][\"passes\"]}')
print(f'   Duration: {data[\"stats\"][\"duration\"]}ms')
"
else
    echo "❌ Mocha test failed"
    exit 1
fi
