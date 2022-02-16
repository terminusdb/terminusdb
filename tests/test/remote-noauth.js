const { expect } = require('chai')
const { Agent, endpoint, remote, util } = require('../lib')

describe('remote-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails on unknown descriptor', async function () {
    const descriptor = 'desc-' + util.randomString()
    const r = await agent.get(`/api/remote/${descriptor}`).then(remote.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BadAbsoluteDescriptor')
    expect(r.body['api:error']['api:absolute_descriptor']).to.equal(descriptor)
  })

  it('fails on unknown database', async function () {
    const { path, orgName, dbName } = endpoint.remote(agent.defaults())
    const r = await agent.get(path).then(remote.verifyNotFound)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownDatabase')
    expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    expect(r.body['api:error']['api:database_name']).to.equal(dbName)
  })
})
