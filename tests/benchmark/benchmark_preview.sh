#!/bin/bash
# Master script: Run all benchmark preview steps

set -e  # Exit on error

cd "$(dirname "$0")"

echo "═══════════════════════════════════════════════════════════"
echo "BENCHMARK PREVIEW - Single Suite from Each System"
echo "═══════════════════════════════════════════════════════════"
echo ""

# Create results directory
mkdir -p benchmark_results

# Step 1: Run Mocha test
echo "Step 1/3: Running Mocha tests..."
echo "───────────────────────────────────────────────────────────"
bash benchmark_preview_1_mocha.sh
echo ""

# Step 2: Run PL-Unit test
echo "Step 2/3: Running PL-Unit tests..."
echo "───────────────────────────────────────────────────────────"
bash benchmark_preview_2_plunit.sh
echo ""

# Step 3: Generate Excel
echo "Step 3/3: Generating Excel report..."
echo "───────────────────────────────────────────────────────────"
python3 benchmark_preview_3_excel.py
echo ""

echo "═══════════════════════════════════════════════════════════"
echo "✅ PREVIEW COMPLETE"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Next steps:"
echo "  1. Review the Excel file in benchmark_results/"
echo "  2. Verify the format looks correct"
echo "  3. If satisfied, run the full benchmark with:"
echo "     node benchmark_excel.js"
echo ""
