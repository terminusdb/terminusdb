const { Agent, branch, db, endpoint, util } = require('../lib')

describe('branch-no-schema', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    const { path } = endpoint.db(agent.defaults())
    await db.createAfterDel(agent, path, { schema: false })
  })

  after(async function () {
    const { path } = endpoint.db(agent.defaults())
    await db.del(agent, path)
  })

  it('passes create and delete', async function () {
    const { path, origin } = endpoint.branchNew(agent.defaults(), util.randomString())
    await agent.post(path).send({ origin }).then(branch.verifySuccess)
    await agent.delete(path).then(branch.verifySuccess)
  })
})
