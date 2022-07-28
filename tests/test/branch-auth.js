const { Agent, api, branch, db, util } = require('../lib')
const { expect } = require('chai')

describe('branch', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('fails create with bad origin descriptor', async function () {
    const origin = util.randomString()
    const result = await branch
      .create(agent, util.randomString(), { origin }).unverified()
    expect(result.body['api:error']['@type']).to.equal('api:OriginBranchDoesNotExist')
  })

  it('fails create with _commits descriptor', async function () {
    const origin = `${agent.orgName}/${agent.dbName}/local/_commits`
    await branch
      .create(agent, util.randomString(), { origin })
      .fails(api.error.notASourceBranchDescriptor(origin))
  })

  it('passes create and delete', async function () {
    const branchName = util.randomString()
    await branch.create(agent, branchName)
    await branch.delete(agent, branchName)
  })

  it('passes create and delete with prefixes', async function () {
    const branchName = util.randomString()
    const prefixes = {
      '@base': 'https://terminushub.com/document',
      '@schema': 'https://terminushub.com/schema',
    }
    await branch.create(agent, branchName, { prefixes })
    await branch.delete(agent, branchName)
  })

  it('fails create with existing branch', async function () {
    await branch.create(agent, 'main').fails(api.error.branchExists('main'))
  })

  it('fails create with unknown origin database', async function () {
    const dbName = util.randomString()
    const origin = api.path.branchOrigin(agent, { dbName })
    await branch
      .create(agent, util.randomString(), { origin })
      .fails(api.error.unknownOriginDatabase(agent.orgName, dbName))
  })
})
