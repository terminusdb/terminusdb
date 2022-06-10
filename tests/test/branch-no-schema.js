const { Agent, branch, db, util } = require('../lib')

describe('branch-no-schema', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { schema: false })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('passes create and delete', async function () {
    const branchName = util.randomString()
    await branch.create(agent, branchName)
    await branch.delete(agent, branchName)
  })
})
