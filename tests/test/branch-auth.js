const { expect } = require('chai')
const { Agent, branch, endpoint, util } = require('../lib')

describe('branch', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('fails on bad origin descriptor', async function () {
    const { path } = endpoint.branch(agent.defaults())
    const originDescriptor = 'desc-' + util.randomString()
    const r = await agent
      .post(path)
      .send({ origin: originDescriptor })
      .then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BadOriginAbsoluteDescriptor')
    expect(r.body['api:error']['api:absolute_descriptor']).to.equal(originDescriptor)
  })

  it('fails on unknown origin database', async function () {
    const { path, orgName, dbName } = endpoint.branch(agent.defaults())
    const originDbName = 'origin-' + dbName
    const r = await agent
      .post(path)
      .send({ origin: `${orgName}/${originDbName}` })
      .then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownOriginDatabase')
    expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    expect(r.body['api:error']['api:database_name']).to.equal(originDbName)
  })
})
