const { expect } = require('chai')
const { Agent, util } = require('../lib')

describe('common-http-errors', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('reports missing path', async function () {
    const paths = [
      '/api',
      '/api/abc',
      '/api/branch',
      '/api/clone',
      '/api/document',
      '/api/fetch',
      '/api/optimize',
      '/api/pack',
      '/api/prefixes',
      '/api/pull',
      '/api/push',
      '/api/rebase',
      '/api/reset',
      '/api/schema',
      '/api/triples',
      '/api/unpack',
    ]
    for (const path of paths) {
      const r = await agent.get(path)
      expect(r.status).to.equal(404)
      expect(r.body['api:status']).to.equal('api:not_found')
      expect(r.body['api:path']).to.equal(path)
    }
  })

  it('reports method not allowed', async function () {
    const requestPairs = [
      ['post', '/api/ok'],
      ['put', '/api/ok'],
      ['put', '/api/db/' + util.randomString() + '/' + util.randomString()],
      ['get', '/api/optimize/'],
      ['put', '/api/optimize/'],
      ['get', '/api/reset/'],
      ['put', '/api/reset/'],
      ['get', '/api/woql'],
      ['put', '/api/woql'],
    ]
    for (const [method, path] of requestPairs) {
      const r = await agent[method](path)
      expect(r.status).to.equal(405)
    }
  })

  it('reports missing content type', async function () {
    const requests = [
      agent.post('/api/db/' + util.randomString() + '/' + util.randomString()),
      agent.post('/api/document/' + util.randomString() + '/' + util.randomString()),
      agent.put('/api/document/' + util.randomString() + '/' + util.randomString()),
      agent.post('/api/rebase/' + util.randomString()),
      agent.post('/api/woql'),
      agent.post('/api/woql').attach('x', Buffer.from('forcing multipart/form-data')),
    ]
    for (const request of requests) {
      const r = await request
      expect(r.status).to.equal(400)
      expect(r.body['api:status']).to.equal('api:failure')
      expect(r.body['@type']).to.equal('api:MissingContentTypeErrorResponse')
    }
  })

  it('reports invalid JSON', async function () {
    const requestPairs = [
      ['post', '/api/db/' + util.randomString() + '/' + util.randomString()],
      ['post', '/api/rebase/' + util.randomString()],
      ['post', '/api/woql'],
    ]
    for (const [method, path] of requestPairs) {
      const body = '{{'
      const r = await agent[method](path).type('application/json').send(body)
      expect(r.status).to.equal(400)
      expect(r.body['api:status']).to.equal('api:failure')
      expect(r.body['system:object']).to.equal(body)
    }
  })
})
