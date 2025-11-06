const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('woql-group-by-simple', function () {
  let agent

  const personSchema = {
    '@id': 'Person',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
  }

  const people = [
    { '@type': 'Person', name: 'Alice' },
    { '@type': 'Person', name: 'Bob' },
    { '@type': 'Person', name: 'Charlie' },
  ]

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema: personSchema })
    await document.insert(agent, { instance: people })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('group_by with single-element list template produces flat array', async function () {
    // Query with single-element LIST template
    const query = {
      '@type': 'GroupBy',
      group_by: [],
      template: {
        '@type': 'Value',
        list: [{ '@type': 'Value', variable: 'Name' }],
      },
      grouped: { '@type': 'Value', variable: 'Grouped' },
      query: {
        '@type': 'Triple',
        subject: { '@type': 'Value', variable: 'Subject' },
        predicate: { '@type': 'NodeValue', node: 'name' },
        object: { '@type': 'Value', variable: 'Name' },
      },
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.be.an('array')
    expect(r.body.bindings.length).to.equal(1)

    const binding = r.body.bindings[0]
    const grouped = binding.Grouped

    // After fix: Should be flat array of typed literals (not nested arrays!)
    expect(grouped).to.be.an('array')
    expect(grouped).to.have.lengthOf(3)

    // Elements should be typed literal objects, not arrays
    expect(grouped[0]).to.be.an('object', 'First element should be an object, not an array')
    expect(grouped[0]).to.not.be.an('array', 'No extra array nesting for single values')
    expect(grouped[0]['@type']).to.equal('xsd:string')
    expect(grouped[0]['@value']).to.be.a('string')

    // Check all values are present
    const values = grouped.map(item => item['@value'])
    expect(values).to.include('Alice')
    expect(values).to.include('Bob')
    expect(values).to.include('Charlie')
  })
})
