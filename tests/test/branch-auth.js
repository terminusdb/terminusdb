const { expect } = require('chai')
const { Agent, branch, db, endpoint, util } = require('../lib')

describe('branch', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    const { path } = endpoint.db(agent.defaults())
    await db.createAfterDel(agent, path)
  })

  after(async function () {
    const { path } = endpoint.db(agent.defaults())
    await db.del(agent, path)
  })

  it('fails create with bad origin descriptor', async function () {
    const { path } = endpoint.branch(agent.defaults())
    const origin = 'desc-' + util.randomString()
    const r = await agent.post(path).send({ origin }).then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BadOriginAbsoluteDescriptor')
    expect(r.body['api:error']['api:absolute_descriptor']).to.equal(origin)
  })

  it('fails create with _commits descriptor', async function () {
    const { orgName, dbName, path } = endpoint.branch(agent.defaults())
    const origin = `${orgName}/${dbName}/local/_commits`
    const r = await agent.post(path).send({ origin }).then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:NotASourceBranchDescriptorError')
    expect(r.body['api:error']['api:absolute_descriptor']).to.equal(origin)
  })

  it('passes create and delete', async function () {
    const { path, origin } = endpoint.branchNew(agent.defaults(), util.randomString())
    await agent.post(path).send({ origin }).then(branch.verifySuccess)
    await agent.delete(path).then(branch.verifySuccess)
  })

  it('passes create and delete with prefixes', async function () {
    const { path, origin } = endpoint.branchNew(agent.defaults(), util.randomString())
    const prefixes = {
      '@base': 'https://terminushub.com/document',
      '@schema': 'https://terminushub.com/schema',
    }
    await agent.post(path).send({ origin, prefixes }).then(branch.verifySuccess)
    await agent.delete(path).then(branch.verifySuccess)
  })

  it('fails create with existing branch', async function () {
    const newBranchName = util.randomString()
    const { path, origin } = endpoint.branchNew(agent.defaults(), newBranchName)
    await agent.post(path).send({ origin }).then(branch.verifySuccess)
    const r = await agent.post(path).send({ origin }).then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BranchExistsError')
    expect(r.body['api:error']['api:branch_name']).to.equal(newBranchName)
  })

  it('fails create with unknown origin database', async function () {
    const params = agent.defaults()
    params.dbName = util.randomString()
    const { path, dbName, orgName, origin } = endpoint.branchNew(params, util.randomString())
    const r = await agent.post(path).send({ origin }).then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownOriginDatabase')
    expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    expect(r.body['api:error']['api:database_name']).to.equal(dbName)
  })
})
