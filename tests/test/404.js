const { expect } = require('chai')
const { Agent } = require('../lib')

describe('404', function () {
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
      '/api/db',
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
})
