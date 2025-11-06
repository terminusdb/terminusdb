const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('woql-dot-operator', function () {
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

  it('extracts fields from path edges using dot operator', async function () {
    // Query path from Alice and extract edge subjects using dot

    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Path',
          subject: { '@type': 'NodeValue', node: 'Person/Alice' },
          pattern: {
            '@type': 'PathPlus',
            plus: { '@type': 'PathPredicate', predicate: 'knows' },
          },
          object: { '@type': 'Value', variable: 'known_person' },
          path: { '@type': 'Value', variable: 'path_edges' },
        },
        {
          '@type': 'Member',
          member: { '@type': 'Value', variable: 'edge' },
          list: { '@type': 'Value', variable: 'path_edges' },
        },
        {
          '@type': 'Optional',
          query: {
            '@type': 'Dot',
            document: { '@type': 'Value', variable: 'edge' },
            field: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:string', '@value': 'subject' },
            },
            value: { '@type': 'Value', variable: 'extracted_subject' },
          },
        },
      ],
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.be.an('array')
    expect(r.body.bindings.length).to.be.greaterThan(0)

    // Verify that extracted values are present
    const binding = r.body.bindings[0]
    expect(binding.extracted_subject).to.equal('Person/Alice')
  })

  it('extracts nested fields from document dict using chained dot operators', async function () {
    // Test dot operator on nested dictionaries without database setup
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Equals',
          left: {
            '@type': 'Value',
            variable: 'doc',
          },
          right: {
            '@type': 'Value',
            dictionary: {
              '@type': 'DictionaryTemplate',
              data: [
                {
                  '@type': 'FieldValuePair',
                  field: '@type',
                  value: {
                    '@type': 'Value',
                    data: {
                      '@type': 'xsd:string',
                      '@value': 'mydoc',
                    },
                  },
                },
                {
                  '@type': 'FieldValuePair',
                  field: 'test',
                  value: {
                    '@type': 'Value',
                    dictionary: {
                      '@type': 'DictionaryTemplate',
                      data: [
                        {
                          '@type': 'FieldValuePair',
                          field: 'test',
                          value: {
                            '@type': 'Value',
                            data: {
                              '@type': 'xsd:string',
                              '@value': 'test1',
                            },
                          },
                        },
                      ],
                    },
                  },
                },
              ],
            },
          },
        },
        {
          '@type': 'Dot',
          document: {
            '@type': 'Value',
            variable: 'doc',
          },
          field: {
            '@type': 'DataValue',
            data: {
              '@type': 'xsd:string',
              '@value': 'test',
            },
          },
          value: {
            '@type': 'Value',
            variable: 'doc2',
          },
        },
        {
          '@type': 'Dot',
          document: {
            '@type': 'Value',
            variable: 'doc2',
          },
          field: {
            '@type': 'DataValue',
            data: {
              '@type': 'xsd:string',
              '@value': 'test',
            },
          },
          value: {
            '@type': 'Value',
            variable: 'doc3',
          },
        },
      ],
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.be.an('array')
    expect(r.body.bindings.length).to.equal(1)

    const binding = r.body.bindings[0]
    // Verify nested extraction worked - value is unwrapped to plain string
    expect(binding.doc3).to.equal('test1')
  })
})
