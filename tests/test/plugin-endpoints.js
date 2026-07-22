const { expect } = require('chai')
const { Agent, db } = require('../lib')

describe('plugin-endpoints', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('tdb-search plugin routes', function () {
    it('GET /api/search/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/search/${agent.orgName}/${agent.dbName}`)
        .query({ query: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })

    it('POST /api/search/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post(`/api/search/${agent.orgName}/${agent.dbName}`)
        .send({ query: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })

    it('POST /api/similar/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post(`/api/similar/${agent.orgName}/${agent.dbName}`)
        .send({ id: 'test', query: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })

    it('GET /api/duplicates/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/duplicates/${agent.orgName}/${agent.dbName}`)
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })

    it('POST /api/plugin/search-resolve/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post(`/api/plugin/search-resolve/${agent.orgName}/${agent.dbName}`)
        .send({ source: 'test', target: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })

    it('GET /api/index/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/index/${agent.orgName}/${agent.dbName}`)
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })

    it('POST /api/compare returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .post('/api/compare')
        .send({ method: 'cosine', source: 'test', target: 'test' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })
  })

  describe('index endpoint response format', function () {
    it('GET /api/index/{path} returns 400 when endpoint is not configured', async function () {
      const r = await agent
        .get(`/api/index/${agent.orgName}/${agent.dbName}`)
        .query({ commit_id: 'placeholder' })
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:TdbSearchEndpointNotConfigured')
    })

    it('POST /api/index/{path} returns valid JSON (not Prolog json{} term)', async function () {
      const r = await agent
        .post(`/api/index/${agent.orgName}/${agent.dbName}/local/branch/main`)
      // Regression: handler used write(json{...}) which produced invalid JSON
      // like "json{@type:api:IndexResponse,api:status:api:success}" instead of
      // proper JSON. The response must be parseable JSON in all cases
      // (success, error, or endpoint-not-configured).
      expect(r.text).to.not.match(/^json\{/)
      expect(r.body).to.be.an('object')
      expect(r.body).to.have.property('@type')
      expect(r.body).to.have.property('api:status')
    })

    it('DELETE /api/index/{path} returns valid JSON (not Prolog json{} term)', async function () {
      const r = await agent
        .delete(`/api/index/${agent.orgName}/${agent.dbName}/local/branch/main`)
      // Regression: handler used write(json{...}) which produced invalid JSON.
      // The response must be parseable JSON in all cases.
      expect(r.text).to.not.match(/^json\{/)
      expect(r.body).to.be.an('object')
      expect(r.body).to.have.property('@type')
      expect(r.body).to.have.property('api:status')
    })
  })
})
