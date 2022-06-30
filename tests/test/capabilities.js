const { expect } = require('chai')
const { Agent, util } = require('../lib')

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
})
