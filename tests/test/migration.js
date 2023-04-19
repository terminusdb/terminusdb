const { expect } = require('chai')
const { Agent, document, db, util } = require('../lib')

describe('patch', function () {
  let agent
  let ty1
  let ty2
  let ty3

  before(function () {
    agent = new Agent().auth()
  })

  describe('migration api', function () {
    beforeEach(async function () {
      await db.create(agent)
      ty1 = util.randomString()
      ty2 = util.randomString()
      ty3 = util.randomString()
      const schema = [
        {
          '@type': 'Class',
          '@id': ty1,
          name: 'xsd:string',
        },
        {
          '@type': 'Enum',
          '@id': ty2,
          '@value': ['A', 'B', 'C'],
        },
        {
          '@type': 'Class',
          '@id': ty3,
          enum: ty2,
        },
      ]
      const instance = [
        {
          '@type': ty1,
          name: 'foo',
        },
        {
          '@type': ty1,
          name: 'bar',
        },
        {
          '@type': ty3,
          enum: 'A',
        },
      ]
      await document.insert(agent, { schema })
      await document.insert(agent, { instance }).unverified()
    })

    afterEach(async function () {
      await db.delete(agent)
    })

    it('migrate enum', async function () {
      const schemaResult = await agent.post(`/api/migration/admin/${agent.dbName}`)
        .send({
          author: 'me',
          message: 'migration',
          verbose: true,
          operations: [{
            '@type': 'ExpandEnum',
            enum: ty2,
            values: ['D'],
          }],
        })
      let enumClass
      for (const i in schemaResult.body.schema) {
        const cls = schemaResult.body.schema[i]
        if (cls['@type'] === 'Enum') {
          enumClass = cls
        }
      }
      expect(enumClass['@value']).to.have.members(['A', 'B', 'C', 'D'])

      const instance = { enum: 'D' }
      const r = await document.insert(agent, { instance }).unverified()

      const [idD] = r.body

      const res = await document.get(agent, { query: { id: idD, as_list: true } })
      expect(res.body[0].enum).to.equal('D')
    })
  })
})
