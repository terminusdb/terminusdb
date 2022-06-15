const { Agent, api, db, util } = require('../lib')

describe('db-auth', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  beforeEach(function () {
    // Use a unique database for each test.
    agent.dbName = util.randomString()
  })

  it('passes exists', async function () {
    await db.create(agent)
    await db.exists(agent)
    await db.delete(agent)
  })

  it('passes create with no comment', async function () {
    const bodyString = '{"label":"hello"}'
    await db.create(agent, { bodyString })
    await db.delete(agent)
  })

  it('fails delete with unknown database', async function () {
    await db.delete(agent).notFound(api.error.unknownDatabase(agent.orgName, agent.dbName))
  })

  it('fails delete after create with unknown organization', async function () {
    // Create a database
    await db.create(agent)
    // Delete the database but with unknown organization
    const orgName = agent.orgName
    agent.orgName = util.randomString()
    await db.delete(agent).notFound(api.error.unknownOrganization(agent.orgName))
    // Delete the created database
    agent.orgName = orgName
    await db.delete(agent)
  })
})
