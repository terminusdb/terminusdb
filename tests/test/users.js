const { expect } = require('chai')
const { Agent, util } = require('../lib')

describe('users', function () {
  it('passes add', async function () {
    const agent = new Agent().auth()
    const userName = util.randomString()
    const result = await agent.post(`/api/users/${userName}`)
    expect(result.body).to.equal(`terminusdb://system/data/User/${userName}`)
    expect(result.status).to.equal(200)
  })

  it('passes delete', async function () {
    const agent = new Agent().auth()
    const userName = util.randomString()
    await agent.post(`/api/users/${userName}`)

    const result = await agent.delete(`/api/users/${userName}`)
    console.log(result.body)
    expect(result.body).to.equal(`terminusdb://system/data/User/${userName}`)
    expect(result.status).to.equal(200)
  })
})
