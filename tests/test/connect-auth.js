const { expect } = require('chai')
const { Agent } = require('../lib')

describe('connect-auth', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('responds using the insecure header', async function () {
    const r = await agent.get('/api/')
    expect(r.status).to.equal(200)
    expect(r.body).to.be.an('array').that.has.lengthOf(0)
  })
})
