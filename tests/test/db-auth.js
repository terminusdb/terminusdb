const { expect } = require('chai')
const { Agent, db, endpoint } = require('../lib')

describe('db', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
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
  })
})
