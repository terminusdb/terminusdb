/**
 * Per-suite resource profiler — mocha root hook plugin.
 *
 * Loaded via `mocha --require ./lib/metrics-profiler.js` and active only when
 * TERMINUSDB_PROFILE_OUT is set; normal test runs are untouched.
 *
 * At every top-level suite boundary the hook runs POST /api/optimize/_system
 * (flush the shared system commit graph so residue measurement reflects what
 * is truly retained) and samples GET /api/metrics, appending one NDJSON record
 * to $TERMINUSDB_PROFILE_OUT. Each record carries the suite that just finished,
 * so consecutive-record deltas attribute residue to the suite that made it.
 *
 * Boundary detection: the sample for suite N is taken inside the beforeEach of
 * suite N+1's first test — after N's work has fully completed and before N+1
 * mutates anything. A baseline record precedes the first suite.
 */

const fs = require('fs')

const OUT = process.env.TERMINUSDB_PROFILE_OUT
const BASE = (process.env.TERMINUSDB_BASE_URL || 'http://localhost:6363').replace(/\/$/, '')
const AUTH = 'Basic ' + Buffer.from(`admin:${process.env.TERMINUSDB_ADMIN_PASS || 'root'}`).toString('base64')
const WORKER = process.env.MOCHA_WORKER_ID || 'main'

let seq = 0
let lastSuite = null
let metricsWarned = false

function topSuite (test) {
  let s = test.parent
  while (s && s.parent && s.parent.title !== '') s = s.parent
  return (s && s.title) || '(root)'
}

function parseMetrics (text) {
  const metrics = {}
  for (const line of text.split('\n')) {
    const m = line.match(/^(\w+(?:\{[^}]*\})?) (\S+)$/)
    if (m) metrics[m[1]] = Number(m[2])
  }
  return metrics
}

async function sample (label) {
  try {
    await fetch(`${BASE}/api/optimize/_system`, {
      method: 'POST',
      headers: { authorization: AUTH },
      signal: AbortSignal.timeout(60000),
    })
  } catch { /* optimize failure must not break the test run */ }
  let metrics = {}
  try {
    const res = await fetch(`${BASE}/api/metrics`, { signal: AbortSignal.timeout(10000) })
    if (!res.ok && !metricsWarned) {
      metricsWarned = true
      console.error(`metrics-profiler: /api/metrics returned ${res.status} on ${BASE} — records will carry empty metrics (endpoint is enterprise-only)`)
    }
    metrics = res.ok ? parseMetrics(await res.text()) : {}
  } catch { /* metrics failure must not break the test run */ }
  fs.appendFileSync(OUT, JSON.stringify({ seq: seq++, worker: WORKER, ts: Date.now(), suite: label, metrics }) + '\n')
}

exports.mochaHooks = OUT
  ? {
      async beforeEach () {
        const suite = topSuite(this.currentTest)
        if (lastSuite === null) {
          lastSuite = suite
          await sample('__baseline__')
          return
        }
        if (suite !== lastSuite) {
          await sample(lastSuite)
          lastSuite = suite
        }
      },
      async afterAll () {
        if (lastSuite !== null) await sample(lastSuite)
      },
    }
  : {}
