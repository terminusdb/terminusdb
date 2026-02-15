const { expect } = require('chai')
const { Agent } = require('../lib')
const { optimizeSystem } = require('../lib/optimize')

describe('connect', function () {
  before(async function () {
    // Optimize system for consistent test performance
    await optimizeSystem(new Agent().auth())
    console.log('🔧 System optimized\n')
  })

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
