const { Agent, api, remote, util } = require('../lib')

describe('remote-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails on unknown descriptor', async function () {
    const descriptor = 'desc-' + util.randomString()
    await remote
      .get(agent, { path: `/api/remote/${descriptor}` })
      .fails(api.error.badDescriptorPath(descriptor))
  })

  it('fails on unknown database', async function () {
    await remote
      .get(agent)
      .notFound(api.error.unknownDatabase(agent.orgName, agent.dbName))
  })
})
