const { expect } = require('chai')
const { spawn, execFile } = require('node:child_process')
const crypto = require('node:crypto')
const fs = require('node:fs')
const http = require('node:http')
const net = require('node:net')
const os = require('node:os')
const path = require('node:path')
const { util } = require('../lib')

// Two TerminusDB instances share one storage and vectorlink directory. The
// first acquires the embed-cache write lock and indexes; the second opens the
// same data read-only and must hand index requests to the lock holder through
// the on-disk index_tasks queue.
// Skipped on Docker-based CI (spawned sibling processes need a local binary)
// or when SKIP_INDEX_HANDOFF_TESTS is set.
const SKIP_INDEX_HANDOFF_TESTS =
  process.env.SKIP_INDEX_HANDOFF_TESTS === 'true' ||
  !!process.env.TERMINUSDB_DOCKER_CONTAINER

const EMBED_DIM = 8
const PASSWORD = 'root'
const ORG = 'admin'
const DB = 'handoff'
const BRANCH_PATH = `${ORG}/${DB}/local/branch/main`

const SCHEMA = [
  {
    '@type': '@context',
    '@base': 'http://example.com/data/',
    '@schema': 'http://example.com/schema#',
    '@metadata': { terminusdb: { options: ['store_indices'] } },
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

// Deterministic OpenAI-compatible embeddings endpoint: vectors are derived
// from the input text hash so repeated runs embed identically.
function startEmbedServer () {
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
      const body = JSON.parse(Buffer.concat(chunks).toString() || '{}')
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
    })
  })
  return new Promise((resolve, reject) => {
    server.once('error', reject)
    server.listen(0, '127.0.0.1', () => resolve(server))
  })
}

function httpRequest (port, method, requestPath, body = null) {
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
    req.setTimeout(10000, () => { req.destroy(new Error('request timeout')) })
    if (body !== null) req.write(JSON.stringify(body))
    req.end()
  })
}

async function poll (fn, timeoutMs, intervalMs = 500) {
  const deadline = Date.now() + timeoutMs
  let last
  while (Date.now() < deadline) {
    last = await fn()
    if (last) return last
    await new Promise(resolve => setTimeout(resolve, intervalMs)) // NOSONAR — polling is serialized by definition
  }
  throw new Error('poll timed out')
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
      // not up yet — keep polling until the deadline
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

async function indexStatus (port) {
  const res = await httpRequest(
    port, 'GET',
    `/api/index/${ORG}/${DB}?path=${BRANCH_PATH}&branch=main`,
  )
  expect(res.status).to.equal(200)
  return res.body
}

async function tryIndexStatus (port) {
  try {
    return await indexStatus(port)
  } catch (e) {
    // transient startup/readiness failure — callers poll until non-null
    return null
  }
}

describe('Read-only index handoff', function () {
  if (SKIP_INDEX_HANDOFF_TESTS) {
    // eslint-disable-next-line mocha/no-pending-tests
    it.skip('skipped: requires a local terminusdb binary (set SKIP_INDEX_HANDOFF_TESTS=true to force)', function () {})
    return
  }
  this.timeout(180000)

  let sharedBase
  let taskDir
  let embedServer
  let embedPort
  let portA
  let portB
  let procA
  let procB

  function serverEnv (port) {
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
      TERMINUSDB_INDEXER_HANDOFF_POLL_SECS: '1',
      TERMINUSDB_WORKER_POOL_SIZE: '30',
      TERMINUSDB_COMMIT_QUEUE_TIMEOUT: '5',
    }
  }

  function spawnInstance (name, port) {
    const logPath = path.join(sharedBase, `${name}.log`)
    const logFd = fs.openSync(logPath, 'a')
    const proc = spawn(util.terminusdbScript(), ['serve'], {
      env: serverEnv(port),
      stdio: ['ignore', logFd, logFd],
    })
    fs.closeSync(logFd)
    proc.on('error', err => process.stderr.write(`[${name}] spawn error: ${err}\n`))
    proc.on('exit', (code, signal) => process.stderr.write(`[${name}] exited code=${code} signal=${signal}\n`))
    return proc
  }

  function taskFiles () {
    try {
      return fs.readdirSync(taskDir).filter(f => f.endsWith('.json'))
    } catch (e) {
      return []
    }
  }

  function initStore (port) {
    return new Promise((resolve, reject) => {
      execFile(util.terminusdbScript(), ['store', 'init', '--key', 'root'],
        { env: serverEnv(port) }, (err, stdout, stderr) => {
          if (err) return reject(new Error(`store init failed: ${stderr || stdout || err.message}`))
          resolve()
        })
    })
  }

  before(async function () {
    sharedBase = fs.mkdtempSync(path.join(os.tmpdir(), 'tdb-handoff-'))
    taskDir = path.join(sharedBase, 'vectorlink', 'index_tasks')
    embedServer = await startEmbedServer()
    embedPort = embedServer.address().port
    portA = await getFreePort()
    portB = await getFreePort()

    await initStore(portA)

    // Instance A acquires the embed-cache lock and owns index writes.
    procA = spawnInstance('handoff-a', portA)
    await waitForServer(portA)

    let res = await httpRequest(portA, 'POST', `/api/db/${ORG}/${DB}`, { comment: '', label: DB })
    expect(res.status).to.equal(200)
    res = await httpRequest(portA, 'POST', `/api/document/${ORG}/${DB}?graph_type=schema&full_replace=true&author=test&message=schema`, SCHEMA)
    expect(res.status).to.equal(200)
    res = await httpRequest(portA, 'POST', `/api/document/${ORG}/${DB}?author=test&message=docs`, [
      { '@type': 'Animal', name: 'Plato' },
      { '@type': 'Animal', name: 'Socrates' },
    ])
    expect(res.status).to.equal(200)
    res = await httpRequest(portA, 'POST', `/api/index/${ORG}/${DB}`, {})
    expect(res.status).to.equal(200)

    const status = await poll(async () => {
      const s = await tryIndexStatus(portA)
      return s?.engine?.searchable_documents >= 2 ? s : false
    }, 60000)
    expect(status.status).to.equal('completed')
    expect(status.read_only).to.equal(false)

    // Instance B opens the same directories read-only.
    procB = spawnInstance('handoff-b', portB)
    await waitForServer(portB)
    await poll(async () => {
      const s = await tryIndexStatus(portB)
      return s?.read_only === true ? s : false
    }, 30000)
  })

  after(async function () {
    await stopInstance(procB)
    await stopInstance(procA)
    if (embedServer) await new Promise(resolve => embedServer.close(resolve))
    if (sharedBase) fs.rmSync(sharedBase, { recursive: true, force: true })
  })

  it('hands off a reindex request from the read-only instance', async function () {
    const res = await httpRequest(portB, 'POST', `/api/index/${ORG}/${DB}`, {})
    expect(res.status).to.equal(200)
    expect(res.body['api:message']).to.match(/handed off/i)
    const files = await poll(async () => {
      const found = taskFiles()
      return found.length > 0 ? found : false
    }, 10000, 200)
    expect(files).to.have.length(1)
    expect(files[0]).to.match(/main\.reindex\.json$/)
  })

  it('collapses duplicate read-only requests into one task file', async function () {
    const res = await httpRequest(portB, 'POST', `/api/index/${ORG}/${DB}`, {})
    expect(res.status).to.equal(200)
    // Pending or already underway, the queue still holds a single file.
    expect(taskFiles().length).to.be.at.most(1)
    const status = await indexStatus(portB)
    expect(status.read_only).to.equal(true)
  })

  it('reports handoff progress and completion on the read-only instance', async function () {
    // While the file exists the requester sees pending or underway; once the
    // writer finishes, the file is deleted and the status returns to no
    // handoff with the indexed commit recorded.
    await poll(async () => taskFiles().length === 0, 90000)
    const status = await indexStatus(portB)
    expect(status.read_only).to.equal(true)
    expect(status.handoff).to.equal(null)
    expect(status.status).to.equal('completed')
    expect(status.last_indexed_commit).to.be.a('string')
  })

  it('keeps search available on the read-only instance', async function () {
    const res = await httpRequest(portB, 'POST', `/api/search/${ORG}/${DB}`, { q: 'Plato' })
    expect(res.status).to.equal(200)
    const hits = res.body && (res.body.results || res.body.hits || res.body)
    expect(hits).to.satisfy(h => Array.isArray(h) ? h.length > 0 : Object.keys(h).length > 0)
  })

  it('hands off a domain-index delete from the read-only instance', async function () {
    const res = await httpRequest(portB, 'DELETE', `/api/index/${BRANCH_PATH}`)
    expect(res.status, res.text).to.equal(200)
    expect(res.body['api:message']).to.match(/handed off/i)
    const files = await poll(async () => {
      const found = taskFiles().filter(f => f.endsWith('.delete.json'))
      return found.length > 0 ? found : false
    }, 10000, 200)
    expect(files[0]).to.equal(`${ORG}%2F${DB}.delete.json`)
    // The writer executes the domain delete and removes the task file.
    await poll(async () => taskFiles().filter(f => f.endsWith('.delete.json')).length === 0, 60000)
    const status = await poll(async () => {
      const s = await tryIndexStatus(portB)
      return s?.engine?.searchable_documents === 0 ? s : false
    }, 30000)
    expect(status.read_only).to.equal(true)
  })
})
