const { expect } = require('chai')
const { Agent, util, document } = require('../lib')

describe('log', function () {
  let agent
  let dbName

  before(function () {
    dbName = util.randomString()
    agent = new Agent({ dbName }).auth()
  })

  describe('logging with new db', function () {
    beforeEach(async function () {
      await agent.post(`/api/db/admin/${dbName}`).send({ label: 'Hello' })
    })

    afterEach(async function () {
      await agent.delete(`/api/db/admin/${dbName}`)
    })

    it('gets a log from changes', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
      const result1 = await document.insert(agent, { instance: instance1 })

      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]
      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }

      const result2 = await document.insert(agent, { instance: instance2 })
      const version2 = result2.headers['terminusdb-data-version'].split('branch:')[1]

      const logRequest = await agent.get(`/api/log/admin/${dbName}`)
      const log = logRequest.body

      expect(log).to.have.lengthOf(3)
      expect(log[0].author).to.equal('default_author')
      expect(log[0].identifier).to.equal(version2)
      expect(log[1].identifier).to.equal(version1)
    })

    it('log entries include user field with authenticated user IRI', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id, a: 'xsd:string' }
      await document.insert(agent, { schema })
      const instance = { '@type': id, '@id': `terminusdb:///data/${id}/0`, a: 'val' }
      await document.insert(agent, { instance })

      const logRequest = await agent.get(`/api/log/admin/${dbName}`)
      const log = logRequest.body

      // Log entries are full commit documents returned by get_document.
      // The user field is the authenticated user's IRI stored as xsd:anyURI.
      expect(log.length).to.be.greaterThan(0)
      for (const entry of log) {
        expect(entry).to.have.property('user', 'terminusdb://system/data/User/admin')
        expect(entry).to.have.property('author')
        expect(entry).to.have.property('message')
        expect(entry).to.have.property('identifier')
        expect(entry).to.have.property('timestamp')
      }
    })

    it('gets a log from changes by commit', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
      const result1 = await document.insert(agent, { instance: instance1 })

      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]
      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }

      const result2 = await document.insert(agent, { instance: instance2 })
      const version2 = result2.headers['terminusdb-data-version'].split('branch:')[1]

      const logRequest = await agent.get(`/api/log/admin/${dbName}/local/commit/${version2}`)
      const log = logRequest.body

      expect(log).to.have.lengthOf(3)
      expect(log[0].author).to.equal('default_author')
      expect(log[0].identifier).to.equal(version2)
      expect(log[1].identifier).to.equal(version1)
    })

    it('gets a log with count', async function () {
      const id = util.randomString()
      const schema = { '@type': 'Class', '@id': id }
      await document.insert(agent, { schema })
      const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
      const result1 = await document.insert(agent, { instance: instance1 })

      const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]
      const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }

      await document.insert(agent, { instance: instance2 })

      const logRequest = await agent.get(`/api/log/admin/${dbName}?start=1&count=1`)
      const log = logRequest.body

      expect(log).to.have.lengthOf(1)
      expect(log[0].author).to.equal('default_author')
      expect(log[0].identifier).to.equal(version1)
      // expect(log[1].identifier).to.equal(version1)
    })
  })

  it('gets an error from _system', async function () {
    const response = await agent.get('/api/log/_system')

    expect(response.status).to.equal(400)
    expect(response.body['api:error']['@type']).to.equal('api:ResourceHasNoHistory')
  })

  it('gets an error from somedb/_meta', async function () {
    const response = await agent.get('/api/log/someorg/somedb/_meta')

    expect(response.status).to.equal(400)
    expect(response.body['api:error']['@type']).to.equal('api:ResourceHasNoHistory')
  })

  it('gets an error from somedb/local/_commits', async function () {
    const response = await agent.get('/api/log/someorg/somedb/local/_commits')

    expect(response.status).to.equal(400)
    expect(response.body['api:error']['@type']).to.equal('api:ResourceHasNoHistory')
  })
})
