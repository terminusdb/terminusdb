const { expect } = require('chai')
const { Agent } = require('../lib')

describe('connect', function () {
  it('passes with no auth', async function () {
    const agent = new Agent()
    const r = await agent.get('/api/')
    expect(r.status).to.equal(200)
    expect(r.body).to.be.an('array').that.has.lengthOf(0)
  })

  it('passes with auth', async function () {
    const agent = new Agent().auth()
    const r = await agent.get('/api/')
    expect(r.status).to.equal(200)
  })
})
