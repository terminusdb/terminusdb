const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

// Test for list indexing with path queries (GitHub issue: times{0,0} bug)
// Note: Path {n,m} uses inclusive bounds (n to m hops inclusive),
// unlike slice() which uses half-open interval [start, end)
// This follows regex-style matching
describe('woql-path-list-indexing', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    // Create db with schema for list slicing tests
    await db.create(agent)

    // Add StringList schema with Random key
    const schema = {
      '@type': 'Class',
      '@id': 'StringList',
      list: {
        '@class': 'xsd:string',
        '@type': 'List',
      },
    }
    await document.insert(agent, { schema })

    // Add StringList document with list ["0", "1", "2", "3", "4"]
    const instance = {
      '@type': 'StringList',
      list: ['0', '1', '2', '3', '4'],
    }
    await document.insert(agent, { instance })
  })

  after(async function () {
    await db.delete(agent)
  })

  // BUG: This is the exact use case from the GitHub issue
  // Using (rdf:rest>){0,0} to select the 0th element of a list
  // {0,0} = 0 hops inclusive = element at index 0
  it('path {0,0} returns element at index 0 (0 hops)', async function () {
    // Query to get the 0th element: follow rdf:rest 0 times, then get rdf:first
    // First find the document by type, then traverse its list
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: 'rdf:type' },
          object: { '@type': 'NodeValue', node: '@schema:StringList' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: '@schema:list' },
          object: { '@type': 'NodeValue', variable: 'listRoot' },
        },
        {
          '@type': 'Path',
          subject: { '@type': 'NodeValue', variable: 'listRoot' },
          pattern: {
            '@type': 'PathTimes',
            from: 0,
            to: 0,
            times: {
              '@type': 'PathPredicate',
              predicate: 'rdf:rest',
            },
          },
          object: { '@type': 'NodeValue', variable: 'listItem' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'listItem' },
          predicate: { '@type': 'NodeValue', node: 'rdf:first' },
          object: { '@type': 'NodeValue', variable: 'selection' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    // Should find only the first element "0"
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    expect(r.body.bindings[0].selection['@value']).to.equal('0')
  })

  // {1,1} = exactly 1 hop = element at index 1
  it('path {1,1} returns element at index 1 (1 hop)', async function () {
    // Query to get the element at index 1: follow rdf:rest 1 time, then get rdf:first
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: 'rdf:type' },
          object: { '@type': 'NodeValue', node: '@schema:StringList' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: '@schema:list' },
          object: { '@type': 'NodeValue', variable: 'listRoot' },
        },
        {
          '@type': 'Path',
          subject: { '@type': 'NodeValue', variable: 'listRoot' },
          pattern: {
            '@type': 'PathTimes',
            from: 1,
            to: 1,
            times: {
              '@type': 'PathPredicate',
              predicate: 'rdf:rest',
            },
          },
          object: { '@type': 'NodeValue', variable: 'listItem' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'listItem' },
          predicate: { '@type': 'NodeValue', node: 'rdf:first' },
          object: { '@type': 'NodeValue', variable: 'selection' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    // Should find only the second element "1"
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(1)
    expect(r.body.bindings[0].selection['@value']).to.equal('1')
  })

  // {0,2} = 0 to 2 hops inclusive = elements at indices 0, 1, 2
  it('path {0,2} returns elements at indices 0-2 (0 to 2 hops inclusive)', async function () {
    // Query to get elements at indices 0, 1, 2: follow rdf:rest 0, 1, or 2 times
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: 'rdf:type' },
          object: { '@type': 'NodeValue', node: '@schema:StringList' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: '@schema:list' },
          object: { '@type': 'NodeValue', variable: 'listRoot' },
        },
        {
          '@type': 'Path',
          subject: { '@type': 'NodeValue', variable: 'listRoot' },
          pattern: {
            '@type': 'PathTimes',
            from: 0,
            to: 2,
            times: {
              '@type': 'PathPredicate',
              predicate: 'rdf:rest',
            },
          },
          object: { '@type': 'NodeValue', variable: 'listItem' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'listItem' },
          predicate: { '@type': 'NodeValue', node: 'rdf:first' },
          object: { '@type': 'NodeValue', variable: 'selection' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    // Should find elements "0", "1", "2"
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(3)
    const values = r.body.bindings.map(b => b.selection['@value']).sort()
    expect(values).to.deep.equal(['0', '1', '2'])
  })

  // {2,4} = 2 to 4 hops inclusive = elements at indices 2, 3, 4
  it('path {2,4} returns elements at indices 2-4 (2 to 4 hops inclusive)', async function () {
    // Query to get elements at indices 2, 3, 4: follow rdf:rest 2, 3, or 4 times
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: 'rdf:type' },
          object: { '@type': 'NodeValue', node: '@schema:StringList' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'id' },
          predicate: { '@type': 'NodeValue', node: '@schema:list' },
          object: { '@type': 'NodeValue', variable: 'listRoot' },
        },
        {
          '@type': 'Path',
          subject: { '@type': 'NodeValue', variable: 'listRoot' },
          pattern: {
            '@type': 'PathTimes',
            from: 2,
            to: 4,
            times: {
              '@type': 'PathPredicate',
              predicate: 'rdf:rest',
            },
          },
          object: { '@type': 'NodeValue', variable: 'listItem' },
        },
        {
          '@type': 'Triple',
          subject: { '@type': 'NodeValue', variable: 'listItem' },
          predicate: { '@type': 'NodeValue', node: 'rdf:first' },
          object: { '@type': 'NodeValue', variable: 'selection' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    // Should find elements "2", "3", "4"
    expect(r.body.bindings).to.be.an('array').that.has.lengthOf(3)
    const values = r.body.bindings.map(b => b.selection['@value']).sort()
    expect(values).to.deep.equal(['2', '3', '4'])
  })
})
