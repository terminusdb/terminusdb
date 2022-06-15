const { Agent, api, branch } = require('../lib')

describe('branch-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails on unknown descriptor', async function () {
    const desc = 'unknowndesc'
    const path = `/api/branch/${desc}`
    await branch
      .create(agent, undefined, { path })
      .fails(api.error.badTargetDescriptor(desc))
  })
})
