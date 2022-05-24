const { expect } = require('chai')
const { Agent } = require('../lib')

describe('root', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('responds with HTML', async function () {
    const paths = [
      '',
      '/',
      '/db/baseball/',
      '/home/somewhere',
    ]
    paths.forEach(async function (path) {
      const r = await agent.get(path)
      expect(r.status).to.equal(200)
      expect(r.header['content-type']).to.equal('text/html')
    })
  })
})
