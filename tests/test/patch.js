const { expect } = require('chai')
const { Agent, api, document, db, util } = require('../lib')

describe('patch', function () {
  let agent
  let ty1
  let ids
  let id1

  before(function () {
    agent = new Agent().auth()
  })

  describe('1 database, shared', function () {
    before(async function () {
      await db.create(agent)
      ty1 = util.randomString()
      const schema = [
        {
          '@type': 'Class',
          '@id': ty1,
          name: 'xsd:string',
        },
      ]
      const instance = [
        {
          '@type': ty1,
          name: 'foo',
        },
      ]
      await document.insert(agent, { schema })
      const response = await document.insert(agent, { instance })
      ids = response.body
      id1 = ids[0]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('applies patch to db', async function () {
      const path = api.path.patchDb(agent)
      const patch = { '@id': id1, name: { '@op' : "SwapValue", '@before': 'foo', '@after': 'bar' } }
      const author = 'me'
      const message = 'yo'
      const res = await agent.post(path).send({ patch, author, message })
      expect(res.body).to.deep.equal([id1])
    })
  })
})
