const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

// More than one comparison operator on a single property.
//
// Only one operator takes effect and the rest are discarded. The first test
// pins which one, and the second proposes rejecting the query instead.
describe('GraphQL filters with several operators on one property', function () {
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
    value: 'xsd:int',
  }]

  const instances = [
    { '@type': 'Measure', name: 'a', value: 0 },
    { '@type': 'Measure', name: 'b', value: 5 },
    { '@type': 'Measure', name: 'c', value: 9 },
    { '@type': 'Measure', name: 'd', value: 15 },
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

  // Which operator survives follows the filter type's own declaration order
  // (eq, ne, lt, le, gt, ge) rather than the order written in the query.
  // Measured pairwise: {eq,lt}->eq, {ne,lt}->ne, {lt,le}->lt, {lt,ge}->lt,
  // {le,gt}->le, {gt,ge}->gt. This test passes and pins that.
  it('keeps one operator, chosen by declaration order rather than query order', async function () {
    // `ge: 5` alone is b, c, d. `lt: 10` alone is a, b, c. lt precedes ge, so
    // lt wins — and writing ge second does not change the outcome.
    const geFirst = await client.query({
      query: gql`{ Measure(filter: { value: { ge: 5, lt: 10 } }) { name } }`,
    })
    const ltFirst = await client.query({
      query: gql`{ Measure(filter: { value: { lt: 10, ge: 5 } }) { name } }`,
    })
    expect(names(geFirst)).to.deep.equal(['a', 'b', 'c'])
    expect(names(ltFirst)).to.deep.equal(['a', 'b', 'c'])
  })

  // The conjunction is expressible, and correct, via _and.
  it('conjoins the same bounds when written with _and', async function () {
    const result = await client.query({
      query: gql`{
        Measure(filter: { _and: [{ value: { ge: 5 } }, { value: { lt: 10 } }] }) { name }
      }`,
    })
    expect(names(result)).to.deep.equal(['b', 'c'])
  })

  // The proposal. This test fails today, on purpose.
  it('rejects a filter with more than one operator on a property', async function () {
    let rejected = false
    try {
      await client.query({
        query: gql`{ Measure(filter: { value: { ge: 5, lt: 10 } }) { name } }`,
      })
    } catch (e) {
      rejected = true
    }
    expect(rejected, 'several operators on one property should be an error').to.equal(true)
  })
})
