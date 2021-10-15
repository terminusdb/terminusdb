const { expect } = require('chai')
const { Agent } = require('../lib')

describe('405', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('reports method not allowed', async function () {
    const requestPairs = [
      ['post', '/api/ok'],
      ['put', '/api/ok'],
      ['get', '/api/db/u/d'],
      ['put', '/api/db/u/d'],
      ['get', '/api/optimize/'],
      ['get', '/api/reset/'],
    ]
    for (const [method, path] of requestPairs) {
      const r = await agent[method](path)
      expect(r.status).to.equal(405)
    }
  })
})
