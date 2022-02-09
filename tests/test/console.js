const { expect } = require('chai')
const { Agent } = require('../lib')

describe('console', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('responds with HTML page', async function () {
    const triedUrls = [
      '',
      '/',
      '/db/gavin/baseball/',
      '/home/somewhere',
    ]
    triedUrls.forEach(async function (url) {
      const r = await agent.get(url)
      expect(r.status).to.equal(200)
      expect(r.headers['content-type']).to.equal('text/html')
    })
  })
})
