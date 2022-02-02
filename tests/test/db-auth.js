const { expect } = require('chai')
const { Agent, db, endpoint, util } = require('../lib')

describe('db-auth', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('succeeds when creating a new database', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await db.create(agent, path)
    db.verifyCreateSuccess(r)
  })

  it('succeeds when removing a new database', async function () {
    const dbName = util.randomString()
    const { path } = endpoint.db({ orgName: agent.userName, dbName })
    const r = await db.create(agent, path)
    db.verifyCreateSuccess(r)
    const removeRequest = await db.del(agent, path)
    db.verifyDeleteSuccess(removeRequest)
  })

  it('fails when creating a new database in non-existing organization', async function () {
    const { dbName } = endpoint.db(agent.defaults())
    const r = await db.create(agent, endpoint.db({ orgName: 'NonExistingOrganization', dbName }).path)
    db.verifyCreateFailure(r)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownOrganization')
  })

  it('fails when creating a new database with bad prefixes', async function () {
    const { path } = endpoint.db(agent.defaults())
    const prefixes = [
      { '@base': 'x' },
      { '@schema': 'x' },
    ]
    prefixes.forEach(async function (prefix) {
      const r = await db.create(agent, path, { prefix })
      db.verifyCreateFailure(r)
    })
  })

  it('fails creating a db when required parameters are not present', async function () {
    const { path } = endpoint.db(agent.defaults())
    const dbName = util.randomString()
    const r = await agent
      .post(path)
      .send({
      })
    expect(r.body['@type']).to.equal('api:BadAPIDocumentErrorResponse')
    const requestOnlyLabel = await agent
      .post(endpoint.db({ orgName: agent.userName, dbName }).path)
      .send({
        label: 'Test Label',
      })
    expect(requestOnlyLabel.body['@type']).to.equal('api:BadAPIDocumentErrorResponse')
  })

  it('fails on database exists with not found', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent
      .head(path)
      .query({ exists: true })
    expect(r.status).to.equal(404)
    expect(r.text).to.be.undefined
  })

  it('fails on deleting existing db in non-existing org', async function () {
    const { path, dbName } = endpoint.db(agent.defaults())

    // Create db
    await db.create(agent, path)

    // Delete the same database with different org
    const deletePath = endpoint.db({ path, orgName: 'NonExistingOrg', dbName }).path
    const r = await db.del(agent, deletePath)
    expect(r.status).to.equal(400)
    expect(r.body['api:status']).to.equal('api:failure')
  })

  it('fails on deleting nonexistent database', async function () {
    const { path, orgName, dbName } = endpoint.db(agent.defaults())

    // Delete a database but don't verify.
    await db.delUnverified(agent, path)

    // Delete the same database.
    const r = await db
      .del(agent, path)
      .then(db.verifyDeleteNotFound)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownDatabase')
    expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    expect(r.body['api:error']['api:database_name']).to.equal(dbName)
    expect(r.body['api:status']).to.equal('api:not_found')
    expect(r.status).to.equal(404)
  })
})
