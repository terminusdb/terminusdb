const { expect } = require('chai')
const { Agent, util, db } = require('../lib')

describe('capabilities', function () {
  it('passes grant and revoke', async function () {
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
    const result2 = await agent.post(`/api/organizations/${orgName}`)
    const orgIdLong = result2.body
    const orgIdList = orgIdLong.split('terminusdb://system/data/')
    const orgId = orgIdList[orgIdList.length - 1]

    // role
    const result3 = await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access', 'meta_write_access',
          'instance_read_access', 'instance_write_access',
          'schema_read_access', 'schema_write_access',
          'create_database', 'delete_database'],
      })
    const roleIdLong = result3.body
    const roleIdList = roleIdLong.split('terminusdb://system/data/')
    const roleId = roleIdList[roleIdList.length - 1]

    const result4 = await agent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgId,
        user: userId,
        roles: [roleId],
      })

    expect(result4.body).to.deep.equal({ '@type': 'api:CapabilityResponse', 'api:status': 'api:success' })
    expect(result4.status).to.equal(200)

    const result5 = await agent
      .post('/api/capabilities')
      .send({
        operation: 'revoke',
        scope: orgId,
        user: userId,
        roles: [roleId],
      })

    expect(result5.body).to.deep.equal({ '@type': 'api:CapabilityResponse', 'api:status': 'api:success' })
    expect(result5.status).to.equal(200)
  })

  it('blocks unauthorized organization users', async function () {
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
    const result2 = await agent.post(`/api/organizations/${orgName}`)
    const orgIdLong = result2.body
    const orgIdList = orgIdLong.split('terminusdb://system/data/')
    const orgId = orgIdList[orgIdList.length - 1]

    // role
    const result3 = await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access', 'meta_write_access',
                 'instance_read_access', 'instance_write_access',
                 'schema_read_access', 'schema_write_access',
                 'create_database', 'delete_database'],
      })
    const roleIdLong = result3.body
    const roleIdList = roleIdLong.split('terminusdb://system/data/')
    const roleId = roleIdList[roleIdList.length - 1]

    await agent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgId,
        user: userId,
        roles: [roleId],
      })

    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // organization users
    const resultUsers = await userAgent.get(`/api/organizations/${orgName}/users`)
    expect(resultUsers.status).to.equal(403)
  })

  it('auth allows authorized organization users', async function () {
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
    const result2 = await agent.post(`/api/organizations/${orgName}`)
    const orgIdLong = result2.body
    const orgIdList = orgIdLong.split('terminusdb://system/data/')
    const orgId = orgIdList[orgIdList.length - 1]

    // role
    const result3 = await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access', 'meta_write_access',
                 'instance_read_access', 'instance_write_access',
                 'schema_read_access', 'schema_write_access',
                 'manage_capabilities', 'create_database',
                 'delete_database'],
      })
    const roleIdLong = result3.body
    const roleIdList = roleIdLong.split('terminusdb://system/data/')
    const roleId = roleIdList[roleIdList.length - 1]

    await agent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgId,
        user: userId,
        roles: [roleId],
      })

    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // organization users
    const resultUsers = await userAgent.get(`/api/organizations/${orgName}/users`)
    expect(resultUsers.status).to.equal(200)
    const users = resultUsers.body
    expect(users[0].name).to.equal(userName)
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
    const result2 = await agent.post(`/api/organizations/${orgName}`)
    const orgIdLong = result2.body
    const orgIdList = orgIdLong.split('terminusdb://system/data/')
    const orgId = orgIdList[orgIdList.length - 1]

    // role
    const result3 = await agent
      .post('/api/roles')
      .send({
        name: roleName,
        action: ['meta_read_access', 'meta_write_access',
                 'instance_read_access', 'instance_write_access',
                 'schema_read_access', 'schema_write_access',
                 'create_database', 'delete_database'],
      })
    const roleIdLong = result3.body
    const roleIdList = roleIdLong.split('terminusdb://system/data/')
    const roleId = roleIdList[roleIdList.length - 1]

    await agent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgId,
        user: userId,
        roles: [roleId],
      })

    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)
    const bodyString = '{"label":"hello"}'
    await db.create(userAgent, { bodyString })

    // organization users
    const resultUsers = await agent.get(`/api/organizations/${orgName}/users`)
    const users = resultUsers.body
    expect(users[0]['@id']).to.equal(userId)
    expect(users[0]).to.not.have.property('key_hash')
    expect(users[0].capability[0]).to.have.property('role')
    expect(users[0].capability[0].role[0].action)
      .to.have.members(['create_database',
                        'delete_database',
                        'instance_read_access',
                        'instance_write_access',
                        'meta_read_access',
                        'meta_write_access',
                        'schema_read_access',
                        'schema_write_access'
                       ])
    // organization users databases
    const resultDatabases = await agent
      .get(`/api/organizations/${orgName}/users/${userName}/databases`)
    const databases = resultDatabases.body
    expect(databases[0].label).to.equal('hello')

    // cleanup
    await db.delete(userAgent)
  })
})
