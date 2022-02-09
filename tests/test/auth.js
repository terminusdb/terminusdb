const { expect } = require('chai')
const { Agent, db, endpoint } = require('../lib')

describe('auth', function () {
  let agent

  before(function () {
    agent = new Agent()
    const userNamePass = Buffer.from('nonExistingUser:somePassword').toString('base64')
    agent.set('Authorization', `Basic ${userNamePass}`)
  })

  it('fails connect on non existing user', async function () {
    const r = await agent.get('/api/')
    expect(r.status).to.equal(401)
  })

  it('fails db create on non existing user', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await db.create(agent, path)
    expect(r.status).to.equal(401)
  })
})
