const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

// Filtering numeric fields in GraphQL.
//
// `xsd:int` filters correctly. `xsd:long` and `xsd:float` return 500, because
// base_type_kind puts "long" (and "unsignedInt") in SMALL_INTEGER_TYPES and
// groups "float" with "double" in FLOAT_TYPES, so the filter is built for the
// narrower type and then panics on the stored value.
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
  }]

  const instances = [
    { '@type': 'Measure', name: 'a', smallValue: 0, bigValue: 0, floatValue: 0.5 },
    { '@type': 'Measure', name: 'b', smallValue: 5, bigValue: 5, floatValue: 5.5 },
    { '@type': 'Measure', name: 'c', smallValue: 9, bigValue: 9, floatValue: 9.5 },
    { '@type': 'Measure', name: 'd', smallValue: 15, bigValue: 15, floatValue: 15.5 },
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

  it('filters an xsd:float field', async function () {
    const result = await client.query({
      query: gql`{ Measure(filter: { floatValue: { ge: 5.0 } }) { name } }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c', 'd'])
  })
})
