const { expect } = require('chai')
const { Agent, db, endpoint, util } = require('../lib')

describe('db-auth', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('passes exists', async function () {
    const { path } = endpoint.db(agent.defaults())
    // Create a database
    await db.create(agent, path).then(db.verifyCreateSuccess)
    // Check for database existence
    const r = await agent.head(path).query({ exists: true })
    expect(r.status).to.equal(200)
    expect(r.text).to.be.undefined
    // Delete the database
    await db.del(agent, path).then(db.verifyDeleteSuccess)
  })

  it('fails delete with unknown database', async function () {
    const { path, orgName, dbName } = endpoint.db(agent.defaults())
    const r = await db.del(agent, path).then(db.verifyDeleteNotFound)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownDatabase')
    expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    expect(r.body['api:error']['api:database_name']).to.equal(dbName)
  })

  it('fails delete after create with unknown organization', async function () {
    const params = agent.defaults()
    const pathWithKnownOrg = endpoint.db(params).path
    params.orgName = util.randomString()
    const pathWithUnknownOrg = endpoint.db(params).path
    // Create a database
    await db.create(agent, pathWithKnownOrg).then(db.verifyCreateSuccess)
    // Delete the database but with unknown organization
    const r = await db.del(agent, pathWithUnknownOrg).then(db.verifyDeleteNotFound)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownOrganizationName')
    expect(r.body['api:error']['api:organization_name']).to.equal(params.orgName)
    // Delete the created database
    await db.del(agent, pathWithKnownOrg).then(db.verifyDeleteSuccess)
  })
})
