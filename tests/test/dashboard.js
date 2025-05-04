const { expect } = require('chai')
const { Agent } = require('../lib')

describe('dashboard', function () {
  it.skip('dashboard redirected from root', async function () {
    const agent = new Agent()
    const r = await agent.get('/')
    expect(r.request._redirects).to.equal(1)
    expect(r.body)
  })

  it.skip('dashboard accessible', async function () {
    const agent = new Agent()
    const r = await agent.get('/dashboard/')
    expect(r.status).to.equal(200)
    expect(r.body)
  })
})
