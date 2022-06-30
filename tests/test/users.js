const { expect } = require('chai')
const { Agent, util } = require('../lib')

describe('users', function () {
  it('passes add', async function () {
    const agent = new Agent().auth()
    const userName = util.randomString()
    const result = await agent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    expect(result.body).to.equal(`terminusdb://system/data/User/${userName}`)
    expect(result.status).to.equal(200)
  })

  it('passes get', async function () {
    const agent = new Agent().auth()
    const userName = util.randomString()
    await agent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    const result = await agent.get(`/api/users/${userName}`)
    expect(result.body.name).to.equal(userName)
    expect(result.status).to.equal(200)
  })

  it('passes delete', async function () {
    const agent = new Agent().auth()
    const userName = util.randomString()
    await agent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    const result = await agent.delete(`/api/users/${userName}`)
    expect(result.body).to.deep.equal({ '@type': 'api:UsersResponse', 'api:status': 'api:success' })
    expect(result.status).to.equal(200)
  })

  it('passes password change', async function () {
    const agent = new Agent().auth()
    const userName = util.randomString()
    await agent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    const newPassword = 'a different password'
    const result = await agent
      .put('/api/users')
      .send({
        name: userName,
        password: newPassword,
      })
    expect(result.status).to.equal(200)

    const userPass = Buffer.from(`${userName}:${newPassword}`).toString('base64')
    agent.set('Authorization', `Basic ${userPass}`)

    const connectResult = await agent.get('/api/info')
    expect(connectResult.body['@type']).to.equal('api:InfoResponse')
  })

  it('denies auth on bad password after change', async function () {
    const agent = new Agent().auth()
    const userName = util.randomString()
    await agent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    const newPassword = 'a different password'
    const result = await agent
      .put('/api/users')
      .send({
        name: userName,
        password: newPassword,
      })
    expect(result.status).to.equal(200)

    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    agent.set('Authorization', `Basic ${userPass}`)

    const connectResult = await agent.get('/api/info')
    expect(connectResult.status).to.equal(401)
  })
})
