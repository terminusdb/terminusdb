const { Agent, api, db, util } = require('../lib')

let agent

async function connect () {
  await agent.get('/api/').then(api.response.verify(api.response.incorrectAuthentication))
}

async function create () {
  await db
    .create(agent)
    .unverified()
    .then(api.response.verify(api.response.incorrectAuthentication))
}

describe('auth', function () {
  describe('basic: unknown user', function () {
    before(function () {
      const user = util.randomString()
      const pass = util.randomString()
      const userPass = Buffer.from(`${user}:${pass}`).toString('base64')
      agent = new Agent()
      agent.set('Authorization', `Basic ${userPass}`)
    })

    it('fails connect', connect)

    it('fails create', create)
  })

  describe('basic: unknown pass', function () {
    before(function () {
      const pass = util.randomString()
      const userPass = Buffer.from(`${agent.user}:${pass}`).toString('base64')
      agent = new Agent()
      agent.set('Authorization', `Basic ${userPass}`)
    })

    it('fails connect', connect)

    it('fails create', create)
  })

  describe('bearer: unknown token', function () {
    before(function () {
      const token = util.randomString()
      agent = new Agent()
      agent.set('Authorization', `Bearer ${token}`)
    })

    it('fails connect', connect)

    it('fails create', create)
  })
})
