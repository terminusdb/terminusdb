const { expect } = require('chai')
const { Agent, db, endpoint, util } = require('../lib')

let agent

async function connect () {
  const r = await agent.get('/api/')
  expect(r.status).to.equal(401)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:ErrorResponse')
  expect(r.body['api:error']['@type']).to.equal('api:IncorrectAuthenticationError')
}

async function create () {
  const { path } = endpoint.db(agent.defaults())
  const r = await db.create(agent, path)
  expect(r.status).to.equal(401)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:ErrorResponse')
  expect(r.body['api:error']['@type']).to.equal('api:IncorrectAuthenticationError')
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
