const { expect } = require('chai')
const { Agent, branch, db, endpoint, util } = require('../lib')

describe('branch', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    const dbDefaults = endpoint.db(agent.defaults())
    const dbPath = dbDefaults.path
    await db.createAfterDel(agent, dbPath)
  })

  after(async function () {
    const dbDefaults = endpoint.db(agent.defaults())
    const dbPath = dbDefaults.path
    await db.del(agent, dbPath)
  })

  it('fails on bad origin descriptor', async function () {
    const { path } = endpoint.branch(agent.defaults())
    const origin = 'desc-' + util.randomString()
    const r = await agent.post(path).send({ origin }).then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BadOriginAbsoluteDescriptor')
    expect(r.body['api:error']['api:absolute_descriptor']).to.equal(origin)
  })

  it('succeeds creating a totally empty branch', async function () {
    // First create a schemaless DB to make sure it is totally empty
    const defaults = agent.defaults()
    defaults.dbName = util.randomString()
    const dbDefaults = endpoint.db(defaults)
    const dbPath = dbDefaults.path
    await db.createAfterDel(agent, dbPath, { schema: false })
    // And then create the branch!
    const { path, origin } = endpoint.branchNew(defaults, util.randomString())
    await agent.post(path).send({ origin }).then(branch.verifySuccess)
    await agent.delete(path).then(branch.verifySuccess)
    await db.del(agent, dbPath)
  })

  it('succeeds creating and deleting a branch', async function () {
    const { path, origin } = endpoint.branchNew(agent.defaults(), util.randomString())
    await agent.post(path).send({ origin }).then(branch.verifySuccess)
    await agent.delete(path).then(branch.verifySuccess)
  })

  it('succeeds creating and deleting a branch with prefixes', async function () {
    const { path, origin } = endpoint.branchNew(agent.defaults(), util.randomString())
    const prefixes = {
      '@base': 'https://terminushub.com/document',
      '@schema': 'https://terminushub.com/schema',
    }
    await agent.post(path).send({ origin, prefixes }).then(branch.verifySuccess)
    await agent.delete(path).then(branch.verifySuccess)
  })

  it('fails creating a branch that already exists', async function () {
    const newBranchName = util.randomString()
    const { path, origin } = endpoint.branchNew(agent.defaults(), newBranchName)
    await agent.post(path).send({ origin }).then(branch.verifySuccess)
    const r = await agent.post(path).send({ origin }).then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BranchExistsError')
    expect(r.body['api:error']['api:branch_name']).to.equal(newBranchName)
  })

  it('fails on unknown origin database', async function () {
    const params = agent.defaults()
    params.dbName = util.randomString()
    const { path, dbName, orgName, origin } = endpoint.branchNew(params, util.randomString())
    const r = await agent.post(path).send({ origin }).then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownOriginDatabase')
    expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    expect(r.body['api:error']['api:database_name']).to.equal(dbName)
  })
})
