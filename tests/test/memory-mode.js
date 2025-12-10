const { expect } = require('chai')
const { spawn } = require('child_process')
const http = require('http')
const path = require('path')

describe('In-Memory Mode', function () {
  this.timeout(30000)

  const PORT = 9393
  const PASSWORD = 'test_password_123'
  let serverProcess = null
  async function waitForServer(maxRetries = 40) {
    for (let i = 0; i < maxRetries; i++) {
      try {
        await new Promise((resolve, reject) => {
          const req = http.get(`http://127.0.0.1:${PORT}/api/`, (res) => {
            resolve(res)
          })
          req.on('error', reject)
          req.setTimeout(1000, () => {
            req.destroy()
            reject(new Error('timeout'))
          })
        })
        return true
      } catch (e) {
        await new Promise(resolve => setTimeout(resolve, 1000))
      }
    }
    throw new Error('Server did not start in time')
  }

  async function httpRequest(options, body = null) {
    return new Promise((resolve, reject) => {
      const req = http.request(options, (res) => {
        let data = ''
        res.on('data', chunk => { data += chunk })
        res.on('end', () => resolve({ status: res.statusCode, body: data }))
      })
      req.on('error', reject)
      if (body) req.write(body)
      req.end()
    })
  }

  function stopServer(done) {
    if (serverProcess) {
      serverProcess.kill('SIGTERM')
      serverProcess = null
      // Wait a bit for the port to be released
      setTimeout(done, 1000)
    } else {
      done()
    }
  }

  describe('server startup tests', function () {
    afterEach(function (done) {
      stopServer(done)
    })

    it('should start server in memory mode with custom password', async function () {
      const terminusdbPath = path.join(__dirname, '../../terminusdb')

      serverProcess = spawn(terminusdbPath, ['serve', '--memory', PASSWORD], {
        env: { ...process.env, TERMINUSDB_SERVER_PORT: PORT.toString() },
        stdio: ['ignore', 'pipe', 'pipe'],
      })

      await waitForServer()

      // Verify we can authenticate with the custom password
      const auth = Buffer.from(`admin:${PASSWORD}`).toString('base64')
      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: '/api/',
        method: 'GET',
        headers: { Authorization: `Basic ${auth}` },
      })

      expect(response.status).to.equal(200)
    })

    it('should start server in memory mode with default password', async function () {
      const terminusdbPath = path.join(__dirname, '../../terminusdb')

      serverProcess = spawn(terminusdbPath, ['serve', '-m'], {
        env: { ...process.env, TERMINUSDB_SERVER_PORT: PORT.toString() },
        stdio: ['ignore', 'pipe', 'pipe'],
      })

      await waitForServer()

      // Verify we can authenticate with the default password 'root'
      const auth = Buffer.from('admin:root').toString('base64')
      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: '/api/',
        method: 'GET',
        headers: { Authorization: `Basic ${auth}` },
      })

      expect(response.status).to.equal(200)
    })

    it('should allow database creation in memory mode', async function () {
      const terminusdbPath = path.join(__dirname, '../../terminusdb')

      serverProcess = spawn(terminusdbPath, ['serve', '--memory', PASSWORD], {
        env: { ...process.env, TERMINUSDB_SERVER_PORT: PORT.toString() },
        stdio: ['ignore', 'pipe', 'pipe'],
      })

      await waitForServer()

      const auth = Buffer.from(`admin:${PASSWORD}`).toString('base64')
      const body = JSON.stringify({ label: 'Memory Test DB' })

      // Create a database
      const createResponse = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: '/api/db/admin/memory_test_db',
        method: 'POST',
        headers: {
          Authorization: `Basic ${auth}`,
          'Content-Type': 'application/json',
          'Content-Length': Buffer.byteLength(body),
        },
      }, body)

      expect(createResponse.status).to.equal(200)
      const result = JSON.parse(createResponse.body)
      expect(result['api:status']).to.equal('api:success')
    })

    it('should return info endpoint with version information', async function () {
      const terminusdbPath = path.join(__dirname, '../../terminusdb')

      serverProcess = spawn(terminusdbPath, ['serve', '--memory', PASSWORD], {
        env: { ...process.env, TERMINUSDB_SERVER_PORT: PORT.toString() },
        stdio: ['ignore', 'pipe', 'pipe'],
      })

      await waitForServer()

      const auth = Buffer.from(`admin:${PASSWORD}`).toString('base64')
      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: '/api/info',
        method: 'GET',
        headers: { Authorization: `Basic ${auth}` },
      })

      const info = JSON.parse(response.body)
      expect(response.status, JSON.stringify(info)).to.equal(200)

      // Verify response structure
      expect(info['@type']).to.equal('api:InfoResponse')
      expect(info['api:status']).to.equal('api:success')

      // Verify api:info contains key properties
      expect(info['api:info']).to.be.an('object')
      expect(info['api:info']).to.have.property('authority')
      expect(info['api:info']).to.have.property('storage')
      expect(info['api:info']).to.have.property('terminusdb')
      expect(info['api:info']).to.have.property('terminusdb_store')

      // Verify terminusdb version info
      expect(info['api:info'].terminusdb).to.have.property('version').that.is.a('string')
      expect(info['api:info'].terminusdb).to.have.property('git_hash').that.is.a('string')

      // Verify storage info
      expect(info['api:info'].storage).to.have.property('version')

      // Verify terminusdb_store version
      expect(info['api:info'].terminusdb_store).to.have.property('version').that.is.a('string')
    })
  })

  describe('full CRUD lifecycle in memory mode', function () {
    const DB_NAME = 'memory_crud_test'
    const DB_PATH = `/api/document/admin/${DB_NAME}`
    let auth

    before(async function () {
      const terminusdbPath = path.join(__dirname, '../../terminusdb')

      serverProcess = spawn(terminusdbPath, ['serve', '--memory', PASSWORD], {
        env: { ...process.env, TERMINUSDB_SERVER_PORT: PORT.toString() },
        stdio: ['ignore', 'pipe', 'pipe'],
      })

      await waitForServer()
      auth = Buffer.from(`admin:${PASSWORD}`).toString('base64')

      // Create database for CRUD tests
      const body = JSON.stringify({ label: 'CRUD Test DB' })
      const createResponse = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `/api/db/admin/${DB_NAME}`,
        method: 'POST',
        headers: {
          Authorization: `Basic ${auth}`,
          'Content-Type': 'application/json',
          'Content-Length': Buffer.byteLength(body),
        },
      }, body)
      expect(createResponse.status).to.equal(200)
    })

    after(function (done) {
      stopServer(done)
    })

    it('should create schema', async function () {
      const schema = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
        age: 'xsd:integer',
      }
      const body = JSON.stringify(schema)

      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `${DB_PATH}?graph_type=schema&author=test&message=Add%20Person%20schema`,
        method: 'POST',
        headers: {
          Authorization: `Basic ${auth}`,
          'Content-Type': 'application/json',
          'Content-Length': Buffer.byteLength(body),
        },
      }, body)

      expect(response.status).to.equal(200)
    })

    it('should add document', async function () {
      const doc = {
        '@type': 'Person',
        '@id': 'Person/john',
        name: 'John Doe',
        age: 30,
      }
      const body = JSON.stringify(doc)

      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `${DB_PATH}?author=test&message=Add%20John`,
        method: 'POST',
        headers: {
          Authorization: `Basic ${auth}`,
          'Content-Type': 'application/json',
          'Content-Length': Buffer.byteLength(body),
        },
      }, body)

      expect(response.status).to.equal(200)
    })

    it('should retrieve document', async function () {
      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `${DB_PATH}?id=Person/john`,
        method: 'GET',
        headers: {
          Authorization: `Basic ${auth}`,
        },
      })

      expect(response.status).to.equal(200)
      const doc = JSON.parse(response.body)
      expect(doc['@id']).to.equal('Person/john')
      expect(doc.name).to.equal('John Doe')
      expect(doc.age).to.equal(30)
    })

    it('should delete document', async function () {
      const body = JSON.stringify('Person/john')

      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `${DB_PATH}?author=test&message=Delete%20John`,
        method: 'DELETE',
        headers: {
          Authorization: `Basic ${auth}`,
          'Content-Type': 'application/json',
          'Content-Length': Buffer.byteLength(body),
        },
      }, body)

      // 204 No Content indicates success for DELETE
      expect(response.status).to.equal(204)
    })

    it('should confirm document is deleted', async function () {
      // Query all Person documents to verify john is gone
      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `${DB_PATH}?type=Person&as_list=true`,
        method: 'GET',
        headers: {
          Authorization: `Basic ${auth}`,
        },
      })

      expect(response.status).to.equal(200)
      const docs = JSON.parse(response.body)
      expect(docs).to.be.an('array')
      expect(docs).to.have.lengthOf(0)
    })

    it('should delete database', async function () {
      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `/api/db/admin/${DB_NAME}`,
        method: 'DELETE',
        headers: {
          Authorization: `Basic ${auth}`,
        },
      })

      expect(response.status).to.equal(200)
      const result = JSON.parse(response.body)
      expect(result['api:status']).to.equal('api:success')
    })

    it('should confirm database is deleted', async function () {
      const response = await httpRequest({
        hostname: '127.0.0.1',
        port: PORT,
        path: `/api/db/admin/${DB_NAME}`,
        method: 'GET',
        headers: {
          Authorization: `Basic ${auth}`,
        },
      })

      expect(response.status).to.equal(404)
    })
  })
})
