const { expect } = require('chai')
const { Agent, util, db } = require('../lib')

describe('capabilities', function () {
  it('passes grant and revoke', async function () {
    const agent = new Agent().auth()
    const orgName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    await agent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })
    // org
    await agent.post(`/api/organizations/${orgName}`)
    // role
    await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access', 'meta_write_access'],
      })

    const result1 = await agent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgName,
        user: userName,
        roles: [roleName],
      })

    expect(result1.body).to.deep.equal({ '@type': 'api:CapabilityResponse', 'api:status': 'api:success' })
    expect(result1.status).to.equal(200)

    const result2 = await agent
      .post('/api/capabilities')
      .send({
        operation: 'revoke',
        scope: orgName,
        user: userName,
        roles: [roleName],
      })

    expect(result2.body).to.deep.equal({ '@type': 'api:CapabilityResponse', 'api:status': 'api:success' })
    expect(result2.status).to.equal(200)
  })

  it('lists organization users', async function () {
    const agent = new Agent().auth()
    const orgName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    const result1 = await agent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })
    const userIdLong = result1.body
    const userIdList = userIdLong.split('terminusdb://system/data/')
    const userId = userIdList[userIdList.length - 1]

    // org
    await agent.post(`/api/organizations/${orgName}`)
    // role
    await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access', 'meta_write_access',
          'instance_read_access', 'instance_write_access',
          'schema_read_access', 'schema_write_access',
          'create_database', 'delete_database'],
      })

    await agent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgName,
        user: userName,
        roles: [roleName],
      })

    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName: orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)
    const bodyString = '{"label":"hello"}'
    await db.create(userAgent, { bodyString })

    // organization users
    const resultUsers = await agent.get(`/api/organizations/${orgName}/users`)
    const users = resultUsers.body
    expect(users[0]['@id']).to.equal(userId)

    // organization users databases
    const resultDatabases = await agent
      .get(`/api/organizations/${orgName}/users/${userName}/databases`)
    const databases = resultDatabases.body
    expect(databases[0].label).to.equal('hello')

    // cleanup
    await db.delete(userAgent)
  })
})
