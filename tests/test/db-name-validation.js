const { expect } = require('chai')
const { Agent, db, endpoint, organization, util } = require('../lib')

describe('db-name-validation', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('fails creation on an invalid database name', async function () {
    const { path } = endpoint.db({
      orgName: 'no_n_existent',
      dbName: 'it|has|pipes',
    })
    const r = await db.create(agent, path)
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:InvalidDatabaseName')
    expect(r.body['api:error']['api:database_name']).to.equal('it|has|pipes')
  })

  it('fails creation on an empty database name', async function () {
    const { path } = endpoint.db({
      orgName: 'no_n_existent',
      dbName: '',
    })
    const r = await db.create(agent, path)
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:InvalidDatabaseName')
    expect(r.body['api:error']['api:database_name']).to.equal('')
  })

  it('fails creation on an empty organization name', async function () {
    const { path } = endpoint.db({
      orgName: '',
      dbName: 'foo',
    })
    const r = await db.create(agent, path)
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:InvalidOrganizationName')
    expect(r.body['api:error']['api:organization_name']).to.equal('')
  })

  describe('in an organization with a pipe', function () {
    let orgName
    let path
    before(async function () {
      orgName = util.randomString() + '|pipe'
      path = endpoint.db({ orgName, dbName: 'foo' }).path

      await organization
        .add(agent, {
          organization_name: orgName,
        })
        .then(organization.verifyAddSuccess)
    })

    after(async function () {
      await db.del(agent, path)
        .then(db.verifyDelSuccess)

      // deleting organizations is unfortunately broken right now.
      /*
      const r = await organization
        .del(agent, {
          organization_name: orgName
        })
        .then(organization.verifyDelSuccess)
      */
    })

    it('succeeds creation of a database', async function () {
      await db.create(agent, path)
        .then(db.verifyCreateSuccess)
    })
  })

  it('fails deletion on an invalid database name', async function () {
    const { path } = endpoint.db({
      orgName: 'no_n_existent',
      dbName: 'it|has|pipes',
    })
    const r = await db.del(agent, path)
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:InvalidDatabaseName')
    expect(r.body['api:error']['api:database_name']).to.equal('it|has|pipes')
  })

  it('fails deletion on an empty database name', async function () {
    const { path } = endpoint.db({
      orgName: 'no_n_existent',
      dbName: '',
    })
    const r = await db.del(agent, path)
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:InvalidDatabaseName')
    expect(r.body['api:error']['api:database_name']).to.equal('')
  })

  it('fails deletion on an empty organization name', async function () {
    const { path } = endpoint.db({
      orgName: '',
      dbName: 'foo',
    })
    const r = await db.del(agent, path)
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:InvalidOrganizationName')
    expect(r.body['api:error']['api:organization_name']).to.equal('')
  })
})
