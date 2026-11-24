const { expect } = require('chai')
const { spawn, execFile } = require('node:child_process')
const crypto = require('node:crypto')
const fs = require('node:fs')
const http = require('node:http')
const net = require('node:net')
const os = require('node:os')
const path = require('node:path')
const { util } = require('../lib')

// Indexing must occupy the dedicated indexer worker, never a request
// worker: with WORKER_POOL_SIZE=1 a held embedding response keeps the
// indexing dispatch busy, and an ordinary document request issued
// mid-hold must still complete. Spawns a sibling server process, so it
// needs a local binary — skipped on Docker CI.
const SKIP_INDEXER_INDEPENDENCE_TESTS =
  process.env.SKIP_INDEXER_INDEPENDENCE_TESTS === 'true' ||
  !!process.env.TERMINUSDB_DOCKER_CONTAINER

const EMBED_DIM = 8
const PASSWORD = 'root'
const ORG = 'admin'
const DB = 'lane'
const BRANCH_PATH = `${ORG}/${DB}/local/branch/main`

// Embedding metadata without store_indices → no auto-index on write;
// the run starts deterministically on the explicit POST below.
const SCHEMA = [
  {
    '@type': '@context',
    '@base': 'http://example.com/data/',
    '@schema': 'http://example.com/schema#',
  },
  {
    '@id': 'Animal',
    '@type': 'Class',
    '@key': { '@type': 'Lexical', '@fields': ['name'] },
    name: 'xsd:string',
    '@metadata': {
      embedding: {
        query: 'query ($id: ID) { Animal(id: $id) { name } }',
        template: 'The animal is named {{name}}.',
      },
    },
  },
]

function getFreePort () {
  return new Promise((resolve, reject) => {
    const srv = net.createServer()
    srv.once('error', reject)
    srv.listen(0, '127.0.0.1', () => {
      const { port } = srv.address()
      srv.close(() => resolve(port))
    })
  })
}

// OpenAI-compatible embeddings endpoint whose responses can be held
// until released — the deterministic way to keep indexing occupied.
function startGatedEmbedServer () {
  const state = { calls: 0, held: [], open: true }
  const server = http.createServer((req, res) => {
    let reqPath = req.url
    while (reqPath.endsWith('/')) reqPath = reqPath.slice(0, -1)
    if (req.method !== 'POST' || reqPath !== '/v1/embeddings') {
      res.writeHead(404).end()
      return
    }
    const chunks = []
    req.on('data', c => chunks.push(c))
    req.on('end', () => {
      state.calls += 1
      const body = JSON.parse(Buffer.concat(chunks).toString() || '{}')
      const answer = () => {
        let input = body.input || []
        if (typeof input === 'string') input = [input]
        const data = input.map((text, index) => {
          const digest = crypto.createHash('sha256').update(String(text)).digest()
          const embedding = []
          for (let i = 0; i < EMBED_DIM; i++) {
            embedding.push((digest[i % digest.length] / 255) * 2 - 1)
          }
          return { embedding, index, object: 'embedding' }
        })
        const out = JSON.stringify({ object: 'list', data, model: body.model || 'fake-embed' })
        res.writeHead(200, { 'Content-Type': 'application/json' }).end(out)
      }
      if (state.open) answer()
      else state.held.push(answer)
    })
  })
  const release = () => {
    state.open = true
    state.held.splice(0).forEach(fn => fn())
  }
  return new Promise((resolve, reject) => {
    server.once('error', reject)
    server.listen(0, '127.0.0.1', () => resolve({ server, state, release }))
  })
}

function httpRequest (port, method, requestPath, body = null, timeoutMs = 10000) {
  return new Promise((resolve, reject) => {
    const req = http.request({
      hostname: '127.0.0.1',
      port,
      path: requestPath,
      method,
      headers: {
        Authorization: 'Basic ' + Buffer.from('admin:' + PASSWORD).toString('base64'),
        ...(body !== null ? { 'Content-Type': 'application/json' } : {}),
      },
    }, (res) => {
      const chunks = []
      res.on('data', c => chunks.push(c))
      res.on('end', () => {
        const raw = Buffer.concat(chunks).toString()
        let json = null
        try { json = JSON.parse(raw) } catch (e) { /* plain text or empty */ }
        resolve({ status: res.statusCode, body: json, text: raw })
      })
    })
    req.on('error', reject)
    req.setTimeout(timeoutMs, () => { req.destroy(new Error('request timeout')) })
    if (body !== null) req.write(JSON.stringify(body))
    req.end()
  })
}

async function poll (fn, timeoutMs, intervalMs = 250) {
  const deadline = Date.now() + timeoutMs
  let last
  while (Date.now() < deadline) {
    last = await fn()
    if (last) return last
    await new Promise(resolve => setTimeout(resolve, intervalMs)) // NOSONAR — polling is serialized by definition
  }
  throw new Error(`poll timed out; last=${JSON.stringify(last)}`)
}

async function waitForServer (port, timeoutMs = 30000) {
  await poll(async () => {
    try {
      const res = await new Promise((resolve, reject) => {
        const req = http.get(`http://127.0.0.1:${port}/api/ok`, resolve)
        req.on('error', reject)
        req.setTimeout(1000, () => { req.destroy(); reject(new Error('timeout')) })
      })
      res.resume()
      return res.statusCode === 200
    } catch (e) { // NOSONAR — connection refused is expected while the server starts
      return false
    }
  }, timeoutMs, 250)
}

function stopInstance (proc) {
  return new Promise((resolve) => {
    if (proc?.exitCode !== null || proc?.signalCode !== null) return resolve()
    proc.once('exit', () => resolve())
    proc.kill('SIGTERM')
    setTimeout(() => { try { proc.kill('SIGKILL') } catch (e) {} resolve() }, 10000).unref()
  })
}

describe('Indexer lane independence', function () {
  if (SKIP_INDEXER_INDEPENDENCE_TESTS) {
    // eslint-disable-next-line mocha/no-pending-tests
    it.skip('skipped: requires a local terminusdb binary (set SKIP_INDEXER_INDEPENDENCE_TESTS=true to force)', function () {})
    return
  }
  this.timeout(300000)

  let sharedBase
  let embed
  let embedPort
  let port
  let proc

  function serverEnv () {
    const projectRoot = path.resolve(__dirname, '..', '..')
    return {
      ...process.env,
      TERMINUSDB_SERVER_NAME: '127.0.0.1',
      TERMINUSDB_ADMIN_PASS: PASSWORD,
      TERMINUSDB_JWT_ENABLED: 'true',
      TERMINUSDB_ENABLE_OPTIMIZER: '0',
      TERMINUSDB_ADDON_PATH: projectRoot,
      TERMINUSDB_PLUGINS_PATH: path.join(projectRoot, 'plugins'),
      TERMINUSDB_BUILTIN_PLUGINS_PATH: path.join(projectRoot, 'builtin_plugins'),
      TERMINUSDB_SERVER_PORT: String(port),
      TERMINUSDB_SERVER_BACKEND: 'rust',
      TERMINUSDB_INDEXER_BACKEND: 'vectorlink',
      TERMINUSDB_SERVER_DB_PATH: path.join(sharedBase, 'db'),
      VECTORLINK_DATA_DIR: path.join(sharedBase, 'vectorlink'),
      VECTORLINK_EMBED_PROVIDER: 'openai_compatible',
      VECTORLINK_EMBED_URL: `http://127.0.0.1:${embedPort}`,
      VECTORLINK_MODEL: 'fake-embed',
      VECTORLINK_DIM: String(EMBED_DIM),
      VECTORLINK_EMBED_CACHE_SIZE: '3000000',
      TERMINUSDB_WORKER_POOL_SIZE: '1',
      TERMINUSDB_COMMIT_QUEUE_TIMEOUT: '5',
      // Loopback-only traffic — never let an inherited proxy env route
      // the embed calls through a dead proxy.
      NO_PROXY: '127.0.0.1,localhost',
      no_proxy: '127.0.0.1,localhost',
    }
  }

  function initStore () {
    return new Promise((resolve, reject) => {
      execFile(util.terminusdbScript(), ['store', 'init', '--key', 'root'],
        { env: serverEnv() }, (err, stdout, stderr) => {
          if (err) return reject(new Error(`store init failed: ${stderr || stdout || err.message}`))
          resolve()
        })
    })
  }

  before(async function () {
    sharedBase = fs.mkdtempSync(path.join(os.tmpdir(), 'tdb-lane-'))
    embed = await startGatedEmbedServer()
    embedPort = embed.server.address().port
    port = await getFreePort()

    await initStore()

    const logPath = path.join(sharedBase, 'lane.log')
    const logFd = fs.openSync(logPath, 'a')
    proc = spawn(util.terminusdbScript(), ['serve'], {
      env: serverEnv(),
      stdio: ['ignore', logFd, logFd],
    })
    fs.closeSync(logFd)
    proc.on('error', err => process.stderr.write(`[lane] spawn error: ${err}\n`))
    proc.on('exit', (code, signal) => process.stderr.write(`[lane] exited code=${code} signal=${signal}\n`))
    await waitForServer(port)

    let res = await httpRequest(port, 'POST', `/api/db/${ORG}/${DB}`, { comment: '', label: DB })
    expect(res.status).to.equal(200)
    res = await httpRequest(port, 'POST', `/api/document/${ORG}/${DB}?graph_type=schema&full_replace=true&author=test&message=schema`, SCHEMA)
    expect(res.status).to.equal(200)

    // Sized so the commit's NDJSON exceeds the pipe buffer — the
    // indexing worker stays blocked writing while the gate holds.
    const docs = []
    for (let i = 0; i < 3000; i++) {
      docs.push({ '@type': 'Animal', name: `Animal number ${i} with a reasonably long name to keep the layer output large` })
    }
    res = await httpRequest(port, 'POST', `/api/document/${ORG}/${DB}?author=test&message=docs`, docs, 120000)
    expect(res.status).to.equal(200)
  })

  after(async function () {
    embed.release()
    await stopInstance(proc)
    if (embed) await new Promise(resolve => embed.server.close(resolve))
    if (sharedBase) {
      // On failure the spawned server's log is the only record of what
      // the indexer worker did — echo it before the temp dir is deleted.
      if (this.currentTest?.state === 'failed') {
        try {
          const log = fs.readFileSync(path.join(sharedBase, 'lane.log'), 'utf8')
          process.stderr.write(`\n[lane.log]\n${log}\n[/lane.log]\n`)
        } catch (e) {
          process.stderr.write(`[lane] could not read lane.log: ${e.message}\n`)
        }
      }
      fs.rmSync(sharedBase, { recursive: true, force: true })
    }
  })

  it('serves ordinary requests while indexing occupies its own worker', async function () {
    embed.state.open = false

    const trigger = await httpRequest(port, 'POST', `/api/index/${ORG}/${DB}`, {})
    expect(trigger.status).to.equal(200)
    expect(trigger.body['api:target_commit']).to.be.a('string').that.is.not.empty

    // The engine is now parked on the held embed call and the indexing
    // worker keeps writing into a backing-up pipe. If the embed endpoint
    // is unreachable the task errors instead — surface that reason
    // immediately rather than waiting out the full poll.
    let lastStatus = null
    await poll(async () => {
      if (embed.state.calls >= 1) return true
      const res = await httpRequest(
        port, 'GET',
        `/api/index/${ORG}/${DB}?path=${BRANCH_PATH}&branch=main`,
      )
      lastStatus = `${res.status} ${JSON.stringify(res.body ?? res.text)}`
      if (res.status === 200 && res.body?.status === 'error') {
        throw new Error(`index task failed before any embed call: ${res.body.error}`)
      }
      return false
    }, 60000).catch(e => {
      throw new Error(`embed endpoint never received a call (calls=${embed.state.calls}, last status=${lastStatus}): ${e.message}`)
    })

    // 5s is far below the remaining indexing time but generous for a
    // free worker — before the dedicated lane this queued behind the
    // blocked indexing dispatch and timed out.
    const res = await httpRequest(
      port, 'POST',
      `/api/document/${ORG}/${DB}?author=test&message=concurrent`,
      [{ '@type': 'Animal', name: 'Concurrent request animal' }],
      5000,
    )
    expect(res.status, res.text).to.equal(200)

    embed.release()
    const status = await poll(async () => {
      try {
        const s = await httpRequest(
          port, 'GET',
          `/api/index/${ORG}/${DB}?path=${BRANCH_PATH}&branch=main`,
        )
        return (s.status === 200 && s.body?.status === 'completed') ? s.body : false
      } catch (e) {
        return false
      }
    }, 120000)
    expect(status.engine?.searchable_documents).to.be.at.least(3000)
  })
})
