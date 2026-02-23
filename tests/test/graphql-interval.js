const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

describe('GraphQL dateTimeInterval', function () {
  let agent
  let client

  const schema = [{
    '@id': 'Event',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    interval: 'xdd:dateTimeInterval',
  }]

  const events = [
    { '@type': 'Event', name: 'ev-2024-H1', interval: '2024-01-01/2024-07-01' },
    { '@type': 'Event', name: 'ev-2024-H2', interval: '2024-07-01/2025-01-01' },
    { '@type': 'Event', name: 'ev-2025-Q1', interval: '2025-01-01/2025-04-01' },
    { '@type': 'Event', name: 'ev-2025-Q2', interval: '2025-04-01/2025-07-01' },
    { '@type': 'Event', name: 'ev-2025-Q3', interval: '2025-07-01/2025-10-01' },
  ]

  before(async function () {
    agent = new Agent().auth()
    const path = api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
    const base = agent.baseUrl
    const uri = `${base}${path}`

    const httpLink = new HttpLink({ uri, fetch })
    const authMiddleware = new ApolloLink((operation, forward) => {
      operation.setContext(({ headers = {} }) => ({
        headers: {
          ...headers,
          authorization: util.authorizationHeader(agent),
        },
      }))
      return forward(operation)
    })

    const ComposedLink = concat(authMiddleware, httpLink)
    const cache = new InMemoryCache({ addTypename: false })
    client = new ApolloClient({ cache, link: ComposedLink })

    await db.create(agent)
    await document.insert(agent, { schema })
    await document.insert(agent, { instance: events })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('queries all events via GraphQL', async function () {
    const Q = gql`query { Event { name interval } }`
    const result = await client.query({ query: Q })
    expect(result.data.Event).to.have.lengthOf(5)
  })

  it('filters with eq', async function () {
    const Q = gql`query { Event(filter: { interval: { eq: "2025-01-01/2025-04-01" }}) { name } }`
    const result = await client.query({ query: Q })
    expect(result.data.Event).to.deep.equal([{ name: 'ev-2025-Q1' }])
  })

  it('filters with ne', async function () {
    const Q = gql`query { Event(filter: { interval: { ne: "2025-01-01/2025-04-01" }}) { name } }`
    const result = await client.query({ query: Q })
    expect(result.data.Event).to.have.lengthOf(4)
    const names = result.data.Event.map(e => e.name)
    expect(names).to.not.include('ev-2025-Q1')
  })

  it('filters with gt (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { gt: "2025-01-01/2025-04-01" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2025-Q2', 'ev-2025-Q3'])
  })

  it('filters with ge (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { ge: "2025-01-01/2025-04-01" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2025-Q1', 'ev-2025-Q2', 'ev-2025-Q3'])
  })

  it('filters with lt (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { lt: "2025-01-01/2025-04-01" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2024-H1', 'ev-2024-H2'])
  })

  it('filters with le (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { le: "2025-01-01/2025-04-01" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2024-H1', 'ev-2024-H2', 'ev-2025-Q1'])
  })
})
