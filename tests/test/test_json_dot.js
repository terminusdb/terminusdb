const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('JSON dot operator tests', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should extract field from JSON string using typecast and dot', async function () {
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Equals',
          left: { '@type': 'Value', variable: 'json' },
          right: {
            '@type': 'Value',
            data: {
              '@type': 'xsd:string',
              '@value': '{"test":"hello world"}',
            },
          },
        },
        {
          '@type': 'Typecast',
          value: { '@type': 'Value', variable: 'json' },
          type: {
            '@type': 'NodeValue',
            node: 'http://terminusdb.com/schema/xdd#json',
          },
          result: { '@type': 'Value', variable: 'json_dict' },
        },
        {
          '@type': 'Dot',
          document: { '@type': 'Value', variable: 'json_dict' },
          field: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': 'test' },
          },
          value: { '@type': 'Value', variable: 'out' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].out).to.equal('hello world')
  })

  it('should handle nested JSON objects', async function () {
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Equals',
          left: { '@type': 'Value', variable: 'json' },
          right: {
            '@type': 'Value',
            data: {
              '@type': 'xsd:string',
              '@value': '{"user":{"name":"Alice","age":30}}',
            },
          },
        },
        {
          '@type': 'Typecast',
          value: { '@type': 'Value', variable: 'json' },
          type: {
            '@type': 'NodeValue',
            node: 'http://terminusdb.com/schema/xdd#json',
          },
          result: { '@type': 'Value', variable: 'json_dict' },
        },
        {
          '@type': 'Dot',
          document: { '@type': 'Value', variable: 'json_dict' },
          field: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': 'user' },
          },
          value: { '@type': 'Value', variable: 'user_obj' },
        },
        {
          '@type': 'Dot',
          document: { '@type': 'Value', variable: 'user_obj' },
          field: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': 'name' },
          },
          value: { '@type': 'Value', variable: 'name' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].name).to.equal('Alice')
  })

  it('should work with sys:Dictionary without unwrapping (direct dict)', async function () {
    // This test documents that sys:Dictionary does NOT need unwrapping
    // because it's already a plain Prolog dict, not a typed literal
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Equals',
          left: { '@type': 'Value', variable: 'json_str' },
          right: {
            '@type': 'Value',
            data: {
              '@type': 'xsd:string',
              '@value': '{"city":"Amsterdam","country":"Netherlands"}',
            },
          },
        },
        {
          '@type': 'Typecast',
          value: { '@type': 'Value', variable: 'json_str' },
          type: {
            '@type': 'NodeValue',
            node: 'http://terminusdb.com/schema/sys#Dictionary',
          },
          result: { '@type': 'Value', variable: 'dict' },
        },
        {
          '@type': 'Dot',
          document: { '@type': 'Value', variable: 'dict' },
          field: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': 'city' },
          },
          value: { '@type': 'Value', variable: 'city' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].city).to.equal('Amsterdam')
  })

  it('should work with nested sys:Dictionary without unwrapping', async function () {
    // Documents that nested dictionaries also work directly
    const query = {
      '@type': 'And',
      and: [
        {
          '@type': 'Equals',
          left: { '@type': 'Value', variable: 'json_str' },
          right: {
            '@type': 'Value',
            data: {
              '@type': 'xsd:string',
              '@value': '{"location":{"city":"Rotterdam","region":"Zuid-Holland"}}',
            },
          },
        },
        {
          '@type': 'Typecast',
          value: { '@type': 'Value', variable: 'json_str' },
          type: {
            '@type': 'NodeValue',
            node: 'http://terminusdb.com/schema/sys#Dictionary',
          },
          result: { '@type': 'Value', variable: 'dict' },
        },
        {
          '@type': 'Dot',
          document: { '@type': 'Value', variable: 'dict' },
          field: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': 'location' },
          },
          value: { '@type': 'Value', variable: 'location' },
        },
        {
          '@type': 'Dot',
          document: { '@type': 'Value', variable: 'location' },
          field: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': 'region' },
          },
          value: { '@type': 'Value', variable: 'region' },
        },
      ],
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].region).to.equal('Zuid-Holland')
  })
})
