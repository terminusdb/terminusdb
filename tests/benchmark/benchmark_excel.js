#!/usr/bin/env node

/**
 * TerminusDB Test Performance Benchmark with Excel Export
 *
 * This script:
 * 1. Runs integration tests 10 times on current branch (from root and tests/)
 * 2. Runs PL-Unit tests 10 times on current branch
 * 3. Switches to main branch and repeats
 * 4. Generates detailed Excel report with comparative analysis
 */

const { exec } = require('child_process')
const { promisify } = require('util')
const fs = require('fs/promises')
const path = require('path')
const XLSX = require('xlsx')

const execAsync = promisify(exec)

// Get repository root path dynamically
const REPO_PATH = path.resolve(__dirname, '..')

const CONFIG = {
  RUNS: 10,
  CURRENT_BRANCH: '128-fix-the-mess',
  COMPARE_BRANCH: 'main',
  TIMEOUT: 30000,
  REPO_PATH,
}

const results = {
  current: { runs: [], individualTests: {}, summary: {} },
  compare: { runs: [], individualTests: {}, summary: {} },
}

async function runIntegrationTests (location, runNumber) {
  const label = location === 'root' ? 'Integration (root)' : 'Integration (tests)'
  process.stdout.write(`    ${label}... `)

  const cmd = location === 'root'
    ? `npx mocha tests/test/*.js --timeout ${CONFIG.TIMEOUT} --reporter json`
    : `cd tests && npx mocha --timeout ${CONFIG.TIMEOUT} --reporter json`

  const startTime = Date.now()
  const { stdout } = await execAsync(cmd, {
    maxBuffer: 50 * 1024 * 1024,
    cwd: CONFIG.REPO_PATH,
  })
  const totalDuration = Date.now() - startTime

  const jsonResults = JSON.parse(stdout)

  // Update duration with actual wall-clock time
  jsonResults.stats.duration = totalDuration

  console.log(`✓ ${totalDuration}ms (${jsonResults.stats.tests} tests, ${jsonResults.stats.suites} suites)`)

  // Return Mocha JSON structure as-is (already in correct format)
  return jsonResults
}

async function runPlUnitTests (runNumber) {
  process.stdout.write('    PL-Unit... ')

  const startTime = Date.now()

  // Run tests with JSON reporter (auto-loaded in all entry points)
  const cmd = 'swipl -g run_tests_json -t halt -f src/interactive.pl'

  const { stdout } = await execAsync(cmd, {
    maxBuffer: 10 * 1024 * 1024,
    cwd: CONFIG.REPO_PATH,
  })

  const totalDuration = Date.now() - startTime

  // Parse JSON output (Mocha-compatible format)
  let jsonResults
  try {
    // Find JSON in output (skip any warnings/info messages)
    const jsonMatch = stdout.match(/\{[\s\S]*\}/)
    if (!jsonMatch) {
      throw new Error('No JSON output found in PL-Unit results')
    }
    jsonResults = JSON.parse(jsonMatch[0])
  } catch (error) {
    console.error(`\n✗ Failed to parse PL-Unit JSON: ${error.message}`)
    console.error('Output:', stdout.substring(0, 500))
    throw error
  }

  // Update duration with actual wall-clock time
  jsonResults.stats.duration = totalDuration

  console.log(`✓ ${totalDuration}ms (${jsonResults.stats.tests} tests, ${jsonResults.stats.suites} suites)`)

  // Return Mocha-compatible JSON structure
  return jsonResults
}

async function switchBranch (branch) {
  console.log(`\n${'─'.repeat(60)}`)
  console.log(`Switching to branch: ${branch}`)
  console.log('─'.repeat(60))

  await execAsync(`git checkout ${branch}`, { cwd: CONFIG.REPO_PATH })

  process.stdout.write('  Building binary... ')
  await execAsync('make dev > /dev/null 2>&1', { cwd: CONFIG.REPO_PATH })
  console.log('✓')

  process.stdout.write('  Restarting test server... ')
  await execAsync('./tests/terminusdb-test-server.sh restart > /dev/null 2>&1', {
    cwd: CONFIG.REPO_PATH,
  })
  await new Promise(resolve => setTimeout(resolve, 3000))
  console.log('✓')
}

async function runBenchmark (branchName, storageKey) {
  console.log(`\n${'═'.repeat(60)}`)
  console.log(`BENCHMARKING: ${branchName}`)
  console.log('═'.repeat(60))

  for (let run = 1; run <= CONFIG.RUNS; run++) {
    console.log(`\n  Run ${run}/${CONFIG.RUNS}:`)

    try {
      const rootTests = await runIntegrationTests('root', run)
      const testsTests = await runIntegrationTests('tests', run)
      const plTests = await runPlUnitTests(run)

      results[storageKey].runs.push({
        run,
        integration_root: rootTests,
        integration_tests: testsTests,
        plunit: plTests,
        timestamp: new Date().toISOString(),
      })

      // Track individual test timings for all tests (using fullTitle as key)
      for (const test of [...rootTests.tests, ...testsTests.tests, ...plTests.tests]) {
        const testKey = test.fullTitle
        if (!results[storageKey].individualTests[testKey]) {
          results[storageKey].individualTests[testKey] = []
        }
        results[storageKey].individualTests[testKey].push(test.duration)
      }
    } catch (error) {
      console.error(`    ✗ Failed: ${error.message}`)
      results[storageKey].runs.push({
        run,
        error: error.message,
        timestamp: new Date().toISOString(),
      })
    }
  }
}

function calculateStats (values) {
  if (values.length === 0) return { mean: 0, median: 0, stddev: 0, min: 0, max: 0 }

  const sorted = values.slice().sort((a, b) => a - b)
  const mean = values.reduce((a, b) => a + b, 0) / values.length
  const median = values.length % 2 === 0
    ? (sorted[values.length / 2 - 1] + sorted[values.length / 2]) / 2
    : sorted[Math.floor(values.length / 2)]

  const variance = values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) / values.length
  const stddev = Math.sqrt(variance)

  return {
    mean: parseFloat(mean.toFixed(2)),
    median: parseFloat(median.toFixed(2)),
    stddev: parseFloat(stddev.toFixed(2)),
    min: Math.min(...values),
    max: Math.max(...values),
  }
}

function aggregateResults () {
  console.log(`\n${'═'.repeat(60)}`)
  console.log('AGGREGATING RESULTS')
  console.log('═'.repeat(60))

  for (const [key, data] of Object.entries(results)) {
    const branchName = key === 'current' ? CONFIG.CURRENT_BRANCH : CONFIG.COMPARE_BRANCH
    console.log(`\nBranch: ${branchName}`)

    const validRuns = data.runs.filter(r => !r.error)

    if (validRuns.length === 0) {
      console.log('  No valid runs!')
      continue
    }

    const rootTimes = validRuns.map(r => r.integration_root.stats.duration)
    const testsTimes = validRuns.map(r => r.integration_tests.stats.duration)
    const plunitTimes = validRuns.map(r => r.plunit.stats.duration)

    data.summary = {
      integration_root: calculateStats(rootTimes),
      integration_tests: calculateStats(testsTimes),
      plunit: calculateStats(plunitTimes),
      validRuns: validRuns.length,
      failedRuns: data.runs.length - validRuns.length,
    }

    console.log(`  Valid runs: ${validRuns.length}/${data.runs.length}`)
    console.log(`  Integration (root) mean: ${data.summary.integration_root.mean}ms ± ${data.summary.integration_root.stddev}ms`)
    console.log(`  Integration (tests) mean: ${data.summary.integration_tests.mean}ms ± ${data.summary.integration_tests.stddev}ms`)
    console.log(`  PL-Unit mean: ${data.summary.plunit.mean}ms ± ${data.summary.plunit.stddev}ms`)
  }
}

async function generateExcelReport () {
  console.log(`\n${'═'.repeat(60)}`)
  console.log('GENERATING EXCEL REPORT')
  console.log('═'.repeat(60))

  const wb = XLSX.utils.book_new()
  const timestamp = new Date().toISOString().replace(/[:.]/g, '-').split('T')[0]

  // Tab 1: Summary Comparison
  const summaryData = [
    ['TerminusDB Test Performance Benchmark', '', '', '', '', '', ''],
    [`Generated: ${new Date().toLocaleString()}`, '', '', '', '', '', ''],
    ['', '', '', '', '', '', ''],
    ['Test Suite', 'Branch', 'Mean (ms)', 'Median (ms)', 'Std Dev', 'Min (ms)', 'Max (ms)', 'Runs'],
  ]

  const suites = ['Integration (root)', 'Integration (tests)', 'PL-Unit']
  const keys = ['integration_root', 'integration_tests', 'plunit']

  for (let i = 0; i < suites.length; i++) {
    const suite = suites[i]
    const key = keys[i]

    const currentStats = results.current.summary[key]
    const compareStats = results.compare.summary[key]

    summaryData.push([
      suite,
      CONFIG.CURRENT_BRANCH,
      currentStats.mean,
      currentStats.median,
      currentStats.stddev,
      currentStats.min,
      currentStats.max,
      results.current.summary.validRuns,
    ])

    summaryData.push([
      '',
      CONFIG.COMPARE_BRANCH,
      compareStats.mean,
      compareStats.median,
      compareStats.stddev,
      compareStats.min,
      compareStats.max,
      results.compare.summary.validRuns,
    ])

    const diff = currentStats.mean - compareStats.mean
    const diffPct = ((diff / compareStats.mean) * 100).toFixed(2)

    summaryData.push([
      '',
      'Difference',
      `${diff > 0 ? '+' : ''}${diff.toFixed(2)}`,
      '',
      '',
      '',
      '',
      `${diff > 0 ? '+' : ''}${diffPct}%`,
    ])

    summaryData.push(['', '', '', '', '', '', '', ''])
  }

  const ws1 = XLSX.utils.aoa_to_sheet(summaryData)
  ws1['!cols'] = [
    { wch: 25 }, { wch: 20 }, { wch: 12 }, { wch: 12 },
    { wch: 10 }, { wch: 10 }, { wch: 10 }, { wch: 10 },
  ]
  XLSX.utils.book_append_sheet(wb, ws1, 'Summary')

  // Tab 2: Detailed Runs
  const runsData = [
    ['Detailed Run Results', '', '', '', '', ''],
    ['', '', '', '', '', ''],
    ['Branch', 'Run', 'Integration Root (ms)', 'Integration Tests (ms)', 'PL-Unit (ms)', 'Timestamp'],
  ]

  for (const [key, data] of Object.entries(results)) {
    const branchName = key === 'current' ? CONFIG.CURRENT_BRANCH : CONFIG.COMPARE_BRANCH

    for (const run of data.runs) {
      if (!run.error) {
        runsData.push([
          branchName,
          run.run,
          run.integration_root.summary.total,
          run.integration_tests.summary.total,
          run.plunit.summary.total,
          run.timestamp,
        ])
      }
    }
  }

  const ws2 = XLSX.utils.aoa_to_sheet(runsData)
  ws2['!cols'] = [{ wch: 20 }, { wch: 8 }, { wch: 20 }, { wch: 20 }, { wch: 15 }, { wch: 25 }]
  XLSX.utils.book_append_sheet(wb, ws2, 'Detailed Runs')

  // Tab 3: Individual Test Comparison (Top 50 slowest tests)
  const testCompData = [
    ['Individual Test Performance Comparison (Top 50 Slowest)', '', '', '', '', ''],
    ['', '', '', '', '', ''],
    ['Test Name', `${CONFIG.CURRENT_BRANCH} Mean (ms)`, `${CONFIG.COMPARE_BRANCH} Mean (ms)`, 'Difference (ms)', 'Difference (%)', 'Category'],
  ]

  const allTestNames = new Set([
    ...Object.keys(results.current.individualTests),
    ...Object.keys(results.compare.individualTests),
  ])

  const testComparisons = []
  for (const testName of allTestNames) {
    const currentTimings = results.current.individualTests[testName] || []
    const compareTimings = results.compare.individualTests[testName] || []

    if (currentTimings.length === 0 || compareTimings.length === 0) continue

    const currentMean = currentTimings.reduce((a, b) => a + b, 0) / currentTimings.length
    const compareMean = compareTimings.reduce((a, b) => a + b, 0) / compareTimings.length
    const diff = currentMean - compareMean
    const diffPct = ((diff / compareMean) * 100).toFixed(2)

    testComparisons.push({
      name: testName,
      currentMean: parseFloat(currentMean.toFixed(2)),
      compareMean: parseFloat(compareMean.toFixed(2)),
      diff: parseFloat(diff.toFixed(2)),
      diffPct: parseFloat(diffPct),
      category: diff > 5 ? 'Slower' : diff < -5 ? 'Faster' : 'Similar',
    })
  }

  // Sort by current branch mean time (slowest first) and take top 50
  testComparisons.sort((a, b) => b.currentMean - a.currentMean)
  const top50 = testComparisons.slice(0, 50)

  for (const test of top50) {
    testCompData.push([
      test.name,
      test.currentMean,
      test.compareMean,
      test.diff,
      `${test.diffPct}%`,
      test.category,
    ])
  }

  const ws3 = XLSX.utils.aoa_to_sheet(testCompData)
  ws3['!cols'] = [{ wch: 60 }, { wch: 15 }, { wch: 15 }, { wch: 15 }, { wch: 15 }, { wch: 12 }]
  XLSX.utils.book_append_sheet(wb, ws3, 'Individual Tests')

  // Save Excel file
  const outputDir = path.join(CONFIG.REPO_PATH, 'tests', 'benchmark_results')
  await fs.mkdir(outputDir, { recursive: true })

  const excelPath = path.join(outputDir, `benchmark_${CONFIG.CURRENT_BRANCH}_vs_${CONFIG.COMPARE_BRANCH}_${timestamp}.xlsx`)
  XLSX.writeFile(wb, excelPath)

  console.log(`  ✓ Excel report: ${excelPath}`)

  // Save detailed JSON for long-term tracking
  const jsonPath = path.join(outputDir, `benchmark_${CONFIG.CURRENT_BRANCH}_vs_${CONFIG.COMPARE_BRANCH}_${timestamp}.json`)
  const jsonData = {
    metadata: {
      generated: timestamp,
      current_branch: CONFIG.CURRENT_BRANCH,
      compare_branch: CONFIG.COMPARE_BRANCH,
      runs_per_branch: CONFIG.RUNS,
      repo_path: CONFIG.REPO_PATH,
    },
    summary: {
      current: results.current.summary,
      compare: results.compare.summary,
    },
    detailed_runs: {
      current: results.current.runs,
      compare: results.compare.runs,
    },
    individual_tests: {
      current: results.current.individualTests,
      compare: results.compare.individualTests,
    },
  }

  await fs.writeFile(jsonPath, JSON.stringify(jsonData, null, 2))
  console.log(`  ✓ JSON data: ${jsonPath}`)

  return { excelPath, jsonPath }
}

function printSummary () {
  console.log(`\n${'═'.repeat(60)}`)
  console.log('BENCHMARK COMPLETE - SUMMARY')
  console.log('═'.repeat(60))

  const comparisons = [
    { name: 'Integration (root)', key: 'integration_root' },
    { name: 'Integration (tests)', key: 'integration_tests' },
    { name: 'PL-Unit', key: 'plunit' },
  ]

  for (const { name, key } of comparisons) {
    const current = results.current.summary[key].mean
    const compare = results.compare.summary[key].mean
    const diff = current - compare
    const diffPct = ((diff / compare) * 100).toFixed(2)

    console.log(`\n${name}:`)
    console.log(`  ${CONFIG.CURRENT_BRANCH}: ${current}ms`)
    console.log(`  ${CONFIG.COMPARE_BRANCH}: ${compare}ms`)
    console.log(`  Difference: ${diff > 0 ? '+' : ''}${diff.toFixed(2)}ms (${diff > 0 ? '+' : ''}${diffPct}%)`)

    if (Math.abs(parseFloat(diffPct)) < 2) {
      console.log('  Result: ≈ No significant difference')
    } else if (diff > 0) {
      console.log('  Result: ⚠️  Current branch is slower')
    } else {
      console.log('  Result: ✓ Current branch is faster')
    }
  }
}

async function main () {
  console.log('\n' + '╔' + '═'.repeat(68) + '╗')
  console.log('║' + ' '.repeat(15) + 'TERMINUSDB TEST PERFORMANCE BENCHMARK' + ' '.repeat(16) + '║')
  console.log('╚' + '═'.repeat(68) + '╝\n')
  console.log(`Current Branch:  ${CONFIG.CURRENT_BRANCH}`)
  console.log(`Compare Branch:  ${CONFIG.COMPARE_BRANCH}`)
  console.log(`Runs per branch: ${CONFIG.RUNS}`)
  console.log(`Total executions: ${CONFIG.RUNS * 2 * 3} (2 branches × ${CONFIG.RUNS} runs × 3 suites)\n`)

  const startTime = Date.now()

  try {
    // Benchmark current branch (already on it)
    await runBenchmark(CONFIG.CURRENT_BRANCH, 'current')

    // Switch and benchmark compare branch
    await switchBranch(CONFIG.COMPARE_BRANCH)
    await runBenchmark(CONFIG.COMPARE_BRANCH, 'compare')

    // Switch back to current branch
    await switchBranch(CONFIG.CURRENT_BRANCH)

    // Aggregate results
    aggregateResults()

    // Generate Excel report
    const excelPath = await generateExcelReport()

    // Print summary
    printSummary()

    const totalTime = ((Date.now() - startTime) / 1000 / 60).toFixed(2)

    console.log(`\n${'═'.repeat(60)}`)
    console.log(`Total benchmark time: ${totalTime} minutes`)
    console.log(`Excel report: ${excelPath}`)
    console.log('═'.repeat(60) + '\n')
  } catch (error) {
    console.error('\n✗ Benchmark failed:', error)
    console.error(error.stack)
    process.exit(1)
  }
}

// Check if xlsx is installed
try {
  require.resolve('xlsx')
  main()
} catch (e) {
  console.error('\n✗ Error: xlsx package not found')
  console.error('Please install it with: npm install xlsx')
  console.error('Then run this script again.\n')
  process.exit(1)
}
