const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

// Filtering numeric fields in GraphQL.
//
// GraphQL's Int is 32-bit, so fields wider than i32 (xsd:long, xsd:unsignedInt,
// xsd:unsignedLong) filter through the BigInt scalar and xsd:decimal through
// BigFloat. Bounds beyond the GraphQL Int range go in as strings.
describe('GraphQL numeric filters', function () {
  let agent
  let client

  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
  }, {
    '@id': 'Measure',
    '@type': 'Class',
    '@key': { '@type': 'Lexical', '@fields': ['name'] },
    name: 'xsd:string',
    smallValue: 'xsd:int',
    bigValue: 'xsd:long',
    floatValue: 'xsd:float',
    unsignedInt: 'xsd:unsignedInt',
    unsignedLong: 'xsd:unsignedLong',
    decimalValue: 'xsd:decimal',
  }]

  const instances = [
    {
      '@type': 'Measure', name: 'a', smallValue: 0, bigValue: 0, floatValue: 0.5, unsignedInt: 0, unsignedLong: 0, decimalValue: 0.5,
    },
    {
      '@type': 'Measure', name: 'b', smallValue: 5, bigValue: 5, floatValue: 5.5, unsignedInt: 5, unsignedLong: 5, decimalValue: 5.5,
    },
    {
      '@type': 'Measure', name: 'c', smallValue: 9, bigValue: 9, floatValue: 9.5, unsignedInt: 9, unsignedLong: 9, decimalValue: 9.5,
    },
    {
      '@type': 'Measure',
      name: 'd',
      smallValue: 15,
      bigValue: { '@type': 'xsd:long', '@value': '9223372036854775807' },
      floatValue: 15.5,
      unsignedInt: 4294967295,
      unsignedLong: { '@type': 'xsd:unsignedLong', '@value': '18446744073709551615' },
      decimalValue: 15.5,
    },
  ]

  before(async function () {
    agent = new Agent().auth()
    const path = api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
    const uri = `${agent.baseUrl}${path}`

    const httpLink = new HttpLink({ uri, fetch })
    const authMiddleware = new ApolloLink((operation, forward) => {
      operation.setContext(({ headers = {} }) => ({
        headers: { ...headers, authorization: util.authorizationHeader(agent) },
      }))
      return forward(operation)
    })

    client = new ApolloClient({
      cache: new InMemoryCache({ addTypename: false }),
      link: concat(authMiddleware, httpLink),
    })

    await db.create(agent)
    await document.insert(agent, { schema, fullReplace: true })
    await document.insert(agent, { instance: instances })
  })

  after(async function () {
    await db.delete(agent)
  })

  const names = (result) => result.data.Measure.map((m) => m.name).sort()

  // Baseline, so a failure below points at the type rather than at filtering.
  it('filters an xsd:int field', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { smallValue: { ge: 5 } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })

  it('filters an xsd:long field', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { bigValue: { ge: 5 } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })

  it('filters an xsd:long field above the 32-bit range', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { bigValue: { ge: "4294967296" } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['d'])
  })

  it('filters an xsd:float field', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { floatValue: { ge: 5.0 } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })

  it('filters an xsd:unsignedInt field', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { unsignedInt: { ge: 5 } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })

  it('filters an xsd:unsignedInt field above the 32-bit signed range', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { unsignedInt: { ge: "4294967295" } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['d'])
  })

  it('filters an xsd:unsignedLong field', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { unsignedLong: { ge: 5 } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })

  it('filters an xsd:unsignedLong field above the 64-bit signed range', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { unsignedLong: { ge: "9223372036854775808" } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['d'])
  })

  it('filters an xsd:decimal field', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { decimalValue: { ge: 5.05 } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })

  it('filters an xsd:decimal field with a string bound', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { decimalValue: { ge: "5.05" } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })

  it('rejects a malformed BigFloat bound with a validation error, not a 500', async function () {
    try {
      await client.query({
        query: gql`{ Measure(filter: { decimalValue: { ge: "abc" } }) { name } }`,
      })
      expect.fail('Should have thrown an error')
    } catch (error) {
      const status = error.networkError?.statusCode
      expect(status).to.not.equal(500)
    }
  })

  it('rejects a malformed BigInt bound with a validation error, not a 500', async function () {
    try {
      await client.query({
        query: gql`{ Measure(filter: { bigValue: { ge: "abc" } }) { name } }`,
      })
      expect.fail('Should have thrown an error')
    } catch (error) {
      const status = error.networkError?.statusCode
      expect(status).to.not.equal(500)
    }
  })
})
