const { Agent, api, util, woql } = require('../lib')

describe('woql-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails with missing query', async function () {
    const requests = [
      woql.post(agent, undefined, { bodyString: '{}' }),
      woql.multipart(agent, undefined, { bodyString: '{}' }),
    ]
    for (const request of requests) {
      await request.fails(api.error.missingParameter('query'))
    }
  })

  it('reports resource not found', async function () {
    const resource = util.randomString() + '/' + util.randomString()
    await woql
      .post(agent, {}, { path: '/api/woql/' + resource })
      .notFound(api.error.unresolvableDescriptor(resource + '/local/branch/main'))
  })
})
