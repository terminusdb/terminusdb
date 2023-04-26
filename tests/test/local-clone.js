const { expect } = require('chai')
const { Agent, db, util } = require('../lib')

describe('api local clone', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('1 database, shared', function () {
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('can clone locally', async function () {
      const randomDb = util.randomString()
      const remote = `${agent.orgName}/${agent.dbName}`
      const res = await agent.post(`/api/clone/${agent.orgName}/${randomDb}`)
        .send({
          comment: 'Foo',
          label: 'bar',
          remote_url: remote,
        })
      expect(res.body).to.deep.equal({ '@type': 'api:CloneResponse', 'api:status': 'api:success' })
      await agent.delete(`/api/db/${agent.orgName}/${randomDb}`).send()
    })
  })
})
