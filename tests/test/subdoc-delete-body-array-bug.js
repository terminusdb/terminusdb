const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('subdoc-delete-body-array-bug', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  const schema = [
    {
      '@type': 'Class',
      '@key': { '@type': 'Lexical', '@fields': ['name'] },
      '@id': 'Parent',
      '@base': 'parent/',
      name: 'xsd:string',
      child: { '@class': 'Child', '@type': 'Optional' },
    },
    {
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      '@id': 'Child',
      '@base': 'child/',
      '@subdocument': [],
      value: { '@class': 'xsd:string', '@type': 'Optional' },
      nested: { '@class': 'Nested', '@type': 'Optional' },
    },
    {
      '@type': 'Class',
      '@key': { '@type': 'Random' },
      '@id': 'Nested',
      '@base': 'nested/',
      '@subdocument': [],
      data: { '@class': 'xsd:string', '@type': 'Optional' },
    },
  ]

  const testDoc = {
    '@type': 'Parent',
    name: 'test-doc',
    child: {
      '@type': 'Child',
      value: 'child-value',
      nested: { '@type': 'Nested', data: 'nested-data' },
    },
  }

  async function countTriples (agent) {
    const tripleQuery = {
      '@type': 'Triple',
      subject: { '@type': 'NodeValue', variable: 'Subject' },
      predicate: { '@type': 'NodeValue', variable: 'Predicate' },
      object: { '@type': 'Value', variable: 'Object' },
    }
    const result = await woql.post(agent, tripleQuery)
    return result.body.bindings
  }

  describe('DELETE with query.id parameter (WORKS)', function () {
    const dbName = 'delete_query_id_works'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)
      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('properly cleans up all subdocuments', async function () {
      await document.insert(agent, {
        instance: testDoc,
        author: 'test',
        message: 'insert doc',
      })

      await document.delete(agent, { query: { id: 'parent/test-doc' } })

      const triplesAfter = await countTriples(agent)

      expect(triplesAfter).to.have.lengthOf(0,
        'DELETE with query.id should leave 0 orphaned triples')
    })
  })

  describe('DELETE with body array', function () {
    const dbName = 'delete_body_array_bug'

    before(async function () {
      agent.dbName = dbName
      await db.create(agent)
      await document.insert(agent, { schema })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('cleans up all subdocuments', async function () {
      await document.insert(agent, {
        instance: testDoc,
        author: 'test',
        message: 'insert doc',
      })

      await document.delete(agent, { body: ['parent/test-doc'] })

      const triplesAfter = await countTriples(agent)

      expect(triplesAfter).to.have.lengthOf(0,
        `DELETE with body array left ${triplesAfter.length} orphaned triples`)
    })
  })

  describe('both delete methods produce same result', function () {
    const dbName1 = 'compare_query_id'
    const dbName2 = 'compare_body_array'

    it('query.id method cleans up properly', async function () {
      agent.dbName = dbName1
      await db.create(agent)
      await document.insert(agent, { schema })
      await document.insert(agent, {
        instance: testDoc,
        author: 'test',
        message: 'insert',
      })

      await document.delete(agent, { query: { id: 'parent/test-doc' } })

      const triples = await countTriples(agent)

      await db.delete(agent)
      expect(triples).to.have.lengthOf(0)
    })

    it('body array method cleans up properly', async function () {
      agent.dbName = dbName2
      await db.create(agent)
      await document.insert(agent, { schema })
      await document.insert(agent, {
        instance: testDoc,
        author: 'test',
        message: 'insert',
      })

      await document.delete(agent, { body: ['parent/test-doc'] })

      const triples = await countTriples(agent)

      await db.delete(agent)
      expect(triples).to.have.lengthOf(0)
    })
  })
})
