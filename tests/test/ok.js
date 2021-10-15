const { expect } = require('chai')
const { Agent } = require('../lib')

describe('ok', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('responds with success', async function () {
    const r = await agent.get('/api/ok')
    expect(r.status).to.equal(200)
    expect(r.header['content-length']).to.equal('0')
  })
})
