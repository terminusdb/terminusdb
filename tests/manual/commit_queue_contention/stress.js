const { Agent } = require('../../lib/agent')
const fs = require('node:fs')
const path = require('node:path')

/**
 * Manual stress test for commit-queue / auto-optimize contention.
 *
 * Interleaves document inserts, branch creation, rebase, push, and explicit
 * optimize calls on the same database while the auto-optimize plugin is
 * enabled. The test itself does not assert behavior; it only reports the number
 * of successful/failed operations and scans the server log for 500 errors and
 * optimization failures.
 *
 * Run with:
 *   TERMINUSDB_PLUGINS_PATH="/path/to/docker/plugins" \
 *     /path/to/tests/terminusdb-test-server.sh start --clean
 *   cd /path/to/tests
 *   node manual/commit_queue_contention/stress.js
 */

const BASE_URL = process.env.TERMINUSDB_BASE_URL || 'http://127.0.0.1:6363'
const LOG_FILE = process.env.TERMINUSDB_LOG ||
      path.resolve(__dirname, '../../.terminusdb-test.log')

const DURATION_SECONDS = Number(process.env.STRESS_DURATION_SECONDS || 60)
const CONCURRENCY = Number(process.env.STRESS_CONCURRENCY || 4)
const REQUEST_TIMEOUT_MS = Number(process.env.STRESS_REQUEST_TIMEOUT_MS || 120000)
const SHUTDOWN_TIMEOUT_MS = Number(process.env.STRESS_SHUTDOWN_TIMEOUT_MS || 60000)
const ERROR_LOG_FILE = process.env.STRESS_ERROR_LOG ||
      path.resolve(__dirname, 'stress-errors.log')

const agent = new Agent({ baseUrl: BASE_URL }).auth({})

let inFlight = 0

let nextDocCounter = 0
const existingBranches = new Set(['main'])
const stats = {
  insert: { ok: 0, fail: 0, skip: 0 },
  branch: { ok: 0, fail: 0, skip: 0 },
  rebase: { ok: 0, fail: 0, skip: 0 },
  optimize: { ok: 0, fail: 0, skip: 0 },
}

async function sleep (ms) {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

async function retry (fn, retries = 3) {
  let lastError
  for (let i = 0; i < retries; i++) {
    try {
      return await fn()
    } catch (err) {
      lastError = err
      await sleep(50 * (i + 1))
    }
  }
  throw lastError
}

function dbPath (operation) {
  return `/api/${operation}/${agent.orgName}/${agent.dbName}`
}

async function ensureDb () {
  agent.dbName = 'stress-' + Date.now()

  const createResponse = await agent.post(dbPath('db')).timeout(REQUEST_TIMEOUT_MS).send({
    label: 'stress test database',
    comment: '',
  })
  if (createResponse.status >= 400 && createResponse.status !== 409) {
    throw new Error(`Database creation failed: ${createResponse.status} ${JSON.stringify(createResponse.body)}`)
  }

  await retry(async () => {
    const response = await agent.post(dbPath('document')).query({
      graph_type: 'schema',
      author: 'stress',
      message: 'stress schema',
      merge_repeats: false,
    }).timeout(REQUEST_TIMEOUT_MS).send({
      '@type': 'Class',
      '@id': 'StressDoc',
      '@key': { '@type': 'Lexical', '@fields': ['counter'] },
      counter: 'xsd:integer',
      label: 'xsd:string',
    })
    if (response.status >= 400) {
      throw new Error(`Schema insert failed: ${response.status} ${JSON.stringify(response.body)}`)
    }
  })

  // Pre-create the feature branches so workers do not race on branch creation.
  for (let i = 0; i < 4; i++) {
    const branchName = `feature-${i}`
    const response = await createBranch(branchName, 'main')
    if (response.status === 200 || response.status === 409) {
      existingBranches.add(branchName)
    }
  }
}

async function insertDoc (branchName = 'main') {
  const n = ++nextDocCounter
  return agent.post(dbPath('document')).query({
    graph_type: 'instance',
    author: 'stress',
    message: `insert ${n}`,
    merge_repeats: false,
  }).timeout(REQUEST_TIMEOUT_MS).send({
    '@type': 'StressDoc',
    counter: n,
    label: `doc-${n}`,
  })
}

async function createBranch (branchName, origin) {
  const response = await agent.post(dbPath('branch') + `/local/branch/${branchName}`).timeout(REQUEST_TIMEOUT_MS).send({
    origin: `${agent.orgName}/${agent.dbName}/local/branch/${origin}`,
  })
  if (response.status === 200 || response.status === 409) {
    existingBranches.add(branchName)
  }
  return response
}

async function rebaseBranch (branchName) {
  if (!existingBranches.has(branchName)) {
    const err = new Error('Branch does not exist')
    err.skip = true
    throw err
  }
  return agent.post(dbPath('rebase')).timeout(REQUEST_TIMEOUT_MS).send({
    rebase_from: `${agent.orgName}/${agent.dbName}/local/branch/${branchName}`,
    author: 'stress',
  })
}

async function explicitOptimize (branchName) {
  const descriptor = `${agent.orgName}/${agent.dbName}/local/branch/${branchName}`
  return agent.post(`/api/optimize/${descriptor}`).timeout(REQUEST_TIMEOUT_MS).send({})
}

async function runWithCheck (operation, promise) {
  inFlight += 1
  let response
  try {
    response = await promise
  } catch (err) {
    if (err.status >= 500) {
      response = err.response
    } else {
      throw err
    }
  } finally {
    inFlight -= 1
  }

  if (response.status >= 500) {
    const body = JSON.stringify(response.body || response.text)
    const line = `${new Date().toISOString()} ${operation} ${response.status} ${body}\n`
    fs.appendFileSync(ERROR_LOG_FILE, line)
    const err = new Error(`Server error ${response.status}: ${body}`)
    err.status = response.status
    err.responseBody = body
    throw err
  }

  return response
}

function isExpectedSetupError (operation, err) {
  return Boolean(err.skip) ||
    (operation === 'rebase' && /source_branch_not_found/.test(err.message)) ||
    (operation === 'insert' && /can_not_insert_existing_object_with_id/.test(err.message))
}

const OPERATIONS = [
  {
    name: 'insert',
    threshold: 0.5,
    run: async (branchName) => runWithCheck('insert', insertDoc(Math.random() < 0.7 ? 'main' : branchName)),
  },
  {
    name: 'branch',
    threshold: 0.6,
    run: async (branchName) => {
      try {
        await runWithCheck('branch', createBranch(branchName, 'main'))
      } catch (err) {
        // Branch may already exist; count as success to keep the loop moving.
      }
    },
  },
  {
    name: 'rebase',
    threshold: 0.7,
    run: async (branchName) => runWithCheck('rebase', rebaseBranch(branchName)),
  },
  {
    name: 'optimize',
    threshold: 1.0,
    run: async (branchName) => runWithCheck('optimize', explicitOptimize(branchName)),
  },
]

async function randomOperation () {
  const branchName = `feature-${Math.floor(Math.random() * 4)}`
  const roll = Math.random()
  const operation = OPERATIONS.find((op) => roll < op.threshold)

  try {
    await operation.run(branchName)
    stats[operation.name].ok += 1
  } catch (err) {
    if (isExpectedSetupError(operation.name, err)) {
      // Expected when the stress test races ahead of its own branch setup.
      // Do not count as a server failure.
      stats[operation.name].skip = (stats[operation.name].skip || 0) + 1
    } else {
      stats[operation.name].fail += 1
    }
  }
}

async function workerLoop (stop) {
  while (!stop.value) {
    await randomOperation()
    await sleep(20 + Math.random() * 80)
  }
}

async function run () {
  // Clear the error log from any previous run.
  try { fs.unlinkSync(ERROR_LOG_FILE) } catch (_) { /* ignore */ }

  await ensureDb()

  const stop = { value: false }
  const workers = []
  for (let i = 0; i < CONCURRENCY; i++) {
    workers.push(workerLoop(stop))
  }

  const startTime = new Date()
  console.log(`[${startTime.toISOString()}] Starting stress test for ${DURATION_SECONDS}s with concurrency ${CONCURRENCY} against ${BASE_URL}`)
  console.log(`Database: ${agent.dbName}`)
  console.log(`Log file: ${LOG_FILE}`)
  console.log(`Request timeout: ${REQUEST_TIMEOUT_MS}ms, shutdown timeout: ${SHUTDOWN_TIMEOUT_MS}ms`)
  console.log('')

  await sleep(DURATION_SECONDS * 1000)
  stop.value = true
  const stopTime = new Date()
  const runDuration = stopTime - startTime
  console.log(`\n[${stopTime.toISOString()}] Stopping workers... ${inFlight} request(s) in flight (ran for ${runDuration}ms)`)
  const shutdown = await Promise.race([
    Promise.all(workers),
    sleep(SHUTDOWN_TIMEOUT_MS).then(() => 'timeout'),
  ])

  const exitTime = new Date()
  const shutdownDuration = exitTime - stopTime

  if (shutdown === 'timeout') {
    console.log(`\n[${exitTime.toISOString()}] WARNING: ${inFlight} request(s) still in flight after shutdown timeout (${shutdownDuration}ms) - workers are stuck`)
    console.log('Result: FAIL (in-flight requests did not terminate cleanly)')
    process.exit(1)
  }

  console.log(`\n[${exitTime.toISOString()}] All workers stopped cleanly (shutdown took ${shutdownDuration}ms)`)
  console.log('Operation counts:')
  console.log(JSON.stringify(stats, null, 2))

  const log = fs.existsSync(LOG_FILE) ? fs.readFileSync(LOG_FILE, 'utf8') : ''
  const logRelevant = log.includes(agent.dbName)
  const errors500 = logRelevant ? (log.match(/\(500\)/g) || []).length : 0
  const optimizeFailed = logRelevant ? (log.match(/Optimization of .* failed/gi) || []).length : 0
  const builderCommitted = logRelevant ? (log.match(/builder has already been committed/g) || []).length : 0
  const unexpectedCommit = logRelevant ? (log.match(/unexpected_commit_failure/g) || []).length : 0

  console.log('')
  console.log('Log scan:')
  if (!logRelevant) {
    console.log(`  Skipped: ${LOG_FILE} does not contain entries for ${agent.dbName}`)
  } else {
    console.log(`  HTTP 500 responses: ${errors500}`)
    console.log(`  Optimization failed: ${optimizeFailed}`)
    console.log(`  "builder has already been committed": ${builderCommitted}`)
    console.log(`  "unexpected_commit_failure": ${unexpectedCommit}`)
  }
  console.log('')

  if (errors500 + optimizeFailed + builderCommitted + unexpectedCommit > 0) {
    console.log('Result: FAIL (contention artifacts detected)')
    process.exit(1)
  } else {
    console.log('Result: PASS (no contention artifacts detected in log)')
  }
}

run().catch((err) => {
  console.error(err)
  process.exit(1)
})
