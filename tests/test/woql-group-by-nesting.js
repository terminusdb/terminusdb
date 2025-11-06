const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('woql-group-by-nesting', function () {
  let agent

  const personSchema = {
    '@id': 'Person',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    knows: {
      '@type': 'Set',
      '@class': 'Person',
    },
  }

  const people = [
    {
      '@type': 'Person',
      name: 'Alice',
      knows: ['Person/Bob', 'Person/Ada'],
    },
    {
      '@type': 'Person',
      name: 'Bob',
      knows: ['Person/Charlie', 'Person/Ada'],
    },
    {
      '@type': 'Person',
      name: 'Ada',
      knows: ['Person/Charlie'],
    },
    {
      '@type': 'Person',
      name: 'Charlie',
      knows: [],
    },
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

  it('group_by should not add extra array nesting to collected values', async function () {
    // Simple query to test group_by with single-element list template
    const query = {
      '@type': 'GroupBy',
      group_by: [], // Group all results together (no grouping variable)
      template: {
        '@type': 'Value',
        list: [{ '@type': 'Value', variable: 'name' }],
      }, // Single-element list template
      grouped: { '@type': 'Value', variable: 'grouped_names' },
      query: {
        '@type': 'Triple',
        subject: { '@type': 'Value', variable: 'person' },
        predicate: { '@type': 'NodeValue', node: 'name' },
        object: { '@type': 'Value', variable: 'name' },
      },
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.be.an('array')
    expect(r.body.bindings.length).to.equal(1)

    const binding = r.body.bindings[0]
    const groupedNames = binding.grouped_names

    // Expected: [{"@type": "xsd:string", "@value": "Alice"}, ...]
    // Before fix (BUG): [[{"@type": "xsd:string", "@value": "Alice"}], ...]

    expect(groupedNames).to.be.an('array')
    expect(groupedNames.length).to.equal(4) // Alice, Bob, Ada, Charlie

    // The key assertion - after fix, first element should NOT be an array
    expect(groupedNames[0]).to.not.be.an('array',
      'First element should not be an array - group_by should not add extra nesting')

    // After the fix, elements should be typed literal objects
    expect(groupedNames[0]).to.be.an('object')
    expect(groupedNames[0]['@type']).to.equal('xsd:string')
    expect(groupedNames[0]['@value']).to.be.a('string')

    // Verify all four names are present
    const values = groupedNames.map(item => item['@value'])
    expect(values).to.include('Alice')
    expect(values).to.include('Bob')
    expect(values).to.include('Ada')
    expect(values).to.include('Charlie')
  })

  it('group_by with multiple variables produces tuples', async function () {
    // Query with TWO variables in template - should produce array of tuples
    const query = {
      '@type': 'GroupBy',
      group_by: [],
      template: {
        '@type': 'Value',
        list: [
          { '@type': 'Value', variable: 'name' },
          { '@type': 'Value', variable: 'person' },
        ],
      }, // Two-element list template
      grouped: { '@type': 'Value', variable: 'grouped_tuples' },
      query: {
        '@type': 'Triple',
        subject: { '@type': 'Value', variable: 'person' },
        predicate: { '@type': 'NodeValue', node: 'name' },
        object: { '@type': 'Value', variable: 'name' },
      },
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.be.an('array')
    expect(r.body.bindings.length).to.equal(1)

    const binding = r.body.bindings[0]
    const groupedTuples = binding.grouped_tuples

    // Expected with multi-variable template: [[name1, person1], [name2, person2], ...]
    expect(groupedTuples).to.be.an('array')
    expect(groupedTuples.length).to.equal(4) // Alice, Bob, Ada, Charlie

    // Each element SHOULD be an array (tuple) with two elements
    expect(groupedTuples[0]).to.be.an('array', 'Multi-variable template should produce tuples')
    expect(groupedTuples[0].length).to.equal(2, 'Each tuple should have 2 elements')

    // First element of tuple is the name
    expect(groupedTuples[0][0]['@type']).to.equal('xsd:string')
    expect(groupedTuples[0][0]['@value']).to.be.a('string')

    // Second element of tuple is the person node
    expect(groupedTuples[0][1]).to.be.a('string') // Node value
  })
})
