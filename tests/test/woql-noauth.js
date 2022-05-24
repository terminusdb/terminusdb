const { expect } = require('chai')
const { Agent, endpoint, util, woql } = require('../lib')

describe('woql-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails with missing query', async function () {
    const { path } = endpoint.woqlResource(agent.defaults())
    const requests = [
      agent.post(path).send({}),
      woql.multipart(agent, path, {}),
    ]
    for (const request of requests) {
      const r = await request.then(woql.verifyGetFailure)
      expect(r.body['api:error']['@type']).to.equal('api:MissingParameter')
      expect(r.body['api:error']['api:parameter']).to.equal('query')
    }
  })

  it('reports resource not found', async function () {
    const resource = util.randomString() + '/' + util.randomString()
    const r = await woql
      .post(agent, '/api/woql/' + resource, { query: {} })
    expect(r.status).to.equal(404)
    expect(r.body['api:error']['@type']).to.equal('api:UnresolvableAbsoluteDescriptor')
    expect(r.body['api:error']['api:absolute_descriptor']).to.equal(resource + '/local/branch/main')
  })
})
