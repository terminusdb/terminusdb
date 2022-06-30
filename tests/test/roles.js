const { expect } = require('chai')
const { Agent, util } = require('../lib')

describe('roles', function () {
  it('passes add', async function () {
    const agent = new Agent().auth()
    const roleName = util.randomString()
    const result = await agent.post(`/api/roles/${roleName}`)
    expect(result.body).to.equal(`terminusdb://system/data/Role/${roleName}`)
    expect(result.status).to.equal(200)
  })

  it('passes delete', async function () {
    const agent = new Agent().auth()
    const roleName = util.randomString()
    await agent.post(`/api/role/${roleName}`)

    const result = await agent.delete(`/api/roles/${roleName}`)
    console.log(result.body)
    expect(result.body).to.equal(`terminusdb://system/data/Role/${roleName}`)
    expect(result.status).to.equal(200)
  })
})
