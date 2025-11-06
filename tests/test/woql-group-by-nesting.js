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
    // Query that groups all path edge subjects together - should produce flat array
    const query = {
      '@type': 'GroupBy',
      group_by: [], // Group all results together (no grouping variable)
      template: {
        '@type': 'Value',
        list: [{ '@type': 'Value', variable: 'extracted_subject' }],
      }, // Single-element list template
      grouped: { '@type': 'Value', variable: 'grouped_subjects' },
      query: {
        '@type': 'And',
        and: [
          {
            '@type': 'Path',
            subject: { '@type': 'NodeValue', node: 'Person/Alice' },
            pattern: {
              '@type': 'PathPlus',
              plus: { '@type': 'PathPredicate', predicate: 'knows' },
            },
            object: { '@type': 'Value', variable: 'person' },
            path: { '@type': 'Value', variable: 'path_edges' },
          },
          {
            '@type': 'Member',
            member: { '@type': 'Value', variable: 'edge' },
            list: { '@type': 'Value', variable: 'path_edges' },
          },
          {
            '@type': 'Dot',
            document: { '@type': 'Value', variable: 'edge' },
            field: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'subject' },
            },
            value: { '@type': 'Value', variable: 'extracted_subject' },
          },
        ],
      },
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.be.an('array')
    expect(r.body.bindings.length).to.equal(1)

    const binding = r.body.bindings[0]
    const groupedSubjects = binding.grouped_subjects

    // Expected: ["Person/Alice", "Person/Bob", "Person/Ada", ...]
    // Actual (BUG): [["Person/Alice"], ["Person/Bob"], ["Person/Ada"], ...]

    expect(groupedSubjects).to.be.an('array')

    // This is what we expect - a flat array of strings
    const expectedFlat = groupedSubjects.every(item => typeof item === 'string')

    // This is what we actually get - nested arrays
    const actualNested = groupedSubjects.every(item => Array.isArray(item))

    if (actualNested && !expectedFlat) {
      console.error('ERROR: group_by added extra array nesting!')
      console.error('Expected flat array of strings: ["Person/Alice", "Person/Bob", ...]')
      console.error('Got nested arrays: [["Person/Alice"], ["Person/Bob"], ...]')
      console.error('Each value is wrapped in its own array, creating double nesting')
    }

    // The assertion that should pass but currently fails
    expect(groupedSubjects[0]).to.be.a('string',
      'First element should be a string, not an array. Bug: group_by adds extra nesting.')
  })

  it('demonstrates the workaround needed for nested arrays', async function () {
    // Same query as above
    const query = {
      '@type': 'GroupBy',
      group_by: [],
      template: {
        '@type': 'Value',
        list: [{ '@type': 'Value', variable: 'extracted_subject' }],
      },
      grouped: { '@type': 'Value', variable: 'grouped_subjects' },
      query: {
        '@type': 'And',
        and: [
          {
            '@type': 'Path',
            subject: { '@type': 'NodeValue', node: 'Person/Alice' },
            pattern: {
              '@type': 'PathPlus',
              plus: { '@type': 'PathPredicate', predicate: 'knows' },
            },
            object: { '@type': 'Value', variable: 'person' },
            path: { '@type': 'Value', variable: 'path_edges' },
          },
          {
            '@type': 'Member',
            member: { '@type': 'Value', variable: 'edge' },
            list: { '@type': 'Value', variable: 'path_edges' },
          },
          {
            '@type': 'Dot',
            document: { '@type': 'Value', variable: 'edge' },
            field: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'subject' },
            },
            value: { '@type': 'Value', variable: 'extracted_subject' },
          },
        ],
      },
    }

    const r = await woql.post(agent, query)
    const binding = r.body.bindings[0]
    const groupedSubjects = binding.grouped_subjects

    // With the bug, we need to unwrap each nested array
    if (Array.isArray(groupedSubjects[0])) {
      const unwrapped = groupedSubjects.map(item => item[0])
      console.log('Unwrapped values:', unwrapped)
      expect(unwrapped).to.include('Person/Alice')
    }
  })
})
