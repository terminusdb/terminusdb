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
    { '@type': 'Event', name: 'ev-2024-H1', interval: '2024-01-01/2024-06-30' },
    { '@type': 'Event', name: 'ev-2024-H2', interval: '2024-07-01/2024-12-31' },
    { '@type': 'Event', name: 'ev-2025-Q1', interval: '2025-01-01/2025-03-31' },
    { '@type': 'Event', name: 'ev-2025-Q2', interval: '2025-04-01/2025-06-30' },
    { '@type': 'Event', name: 'ev-2025-Q3', interval: '2025-07-01/2025-09-30' },
  ]

  before(async function () {
    agent = new Agent().auth()
    const path = api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
    const base = agent.baseUrl
    const uri = `${base}${path}?compress_ids=false`

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
    // Stored values are normalized: date-only end bumps to next day (exclusive)
    // e.g. Q1 end 2025-03-31 bumps to 2025-04-01T00:00:00Z
    const Q = gql`query { Event(filter: { interval: { eq: "2025-01-01T00:00:00Z/2025-04-01T00:00:00Z" }}) { name } }`
    const result = await client.query({ query: Q })
    expect(result.data.Event).to.deep.equal([{ name: 'ev-2025-Q1' }])
  })

  it('filters with ne', async function () {
    const Q = gql`query { Event(filter: { interval: { ne: "2025-01-01T00:00:00Z/2025-04-01T00:00:00Z" }}) { name } }`
    const result = await client.query({ query: Q })
    expect(result.data.Event).to.have.lengthOf(4)
    const names = result.data.Event.map(e => e.name)
    expect(names).to.not.include('ev-2025-Q1')
  })

  it('filters with gt (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { gt: "2025-01-01T00:00:00Z/2025-04-01T00:00:00Z" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2025-Q2', 'ev-2025-Q3'])
  })

  it('filters with ge (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { ge: "2025-01-01T00:00:00Z/2025-04-01T00:00:00Z" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2025-Q1', 'ev-2025-Q2', 'ev-2025-Q3'])
  })

  it('filters with lt (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { lt: "2025-01-01T00:00:00Z/2025-04-01T00:00:00Z" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2024-H1', 'ev-2024-H2'])
  })

  it('filters with le (chronological)', async function () {
    const Q = gql`query { Event(filter: { interval: { le: "2025-01-01T00:00:00Z/2025-04-01T00:00:00Z" }}) { name } }`
    const result = await client.query({ query: Q })
    const names = result.data.Event.map(e => e.name).sort()
    expect(names).to.deep.equal(['ev-2024-H1', 'ev-2024-H2', 'ev-2025-Q1'])
  })

  // Allen interval relations via the generic relationship filter.
  // Stored spans (date ends bump to next-day exclusive):
  //   H1 [24-01-01,24-07-01) H2 [24-07-01,25-01-01) Q1 [25-01-01,25-04-01)
  //   Q2 [25-04-01,25-07-01) Q3 [25-07-01,25-10-01) — a consecutive chain.
  describe('relationship (Allen) filters', function () {
    const rel = (r, v) => `query { Event(filter: { interval: { relationship: { rel: "${r}", value: "${v}" }}}) { name } }`
    const expectNames = async (r, v, names) => {
      const result = await client.query({ query: gql(rel(r, v)) })
      expect(result.data.Event.map(e => e.name).sort()).to.deep.equal(names)
    }

    const cases = [
      ['equals', '2025-04-01T00:00:00Z/2025-07-01T00:00:00Z', ['ev-2025-Q2']],
      ['before', '2025-04-01T00:00:00Z/2025-07-01T00:00:00Z', ['ev-2024-H1', 'ev-2024-H2']],
      ['after', '2025-04-01T00:00:00Z/2025-07-01T00:00:00Z', []],
      ['meets', '2025-04-01T00:00:00Z/2025-07-01T00:00:00Z', ['ev-2025-Q1']],
      ['met_by', '2025-04-01T00:00:00Z/2025-07-01T00:00:00Z', ['ev-2025-Q3']],
      ['overlaps', '2025-03-15T00:00:00Z/2025-05-15T00:00:00Z', ['ev-2025-Q1']],
      ['overlapped_by', '2025-03-15T00:00:00Z/2025-05-15T00:00:00Z', ['ev-2025-Q2']],
      ['starts', '2025-04-01T00:00:00Z/2025-12-31T00:00:00Z', ['ev-2025-Q2']],
      ['started_by', '2025-04-01T00:00:00Z/2025-05-01T00:00:00Z', ['ev-2025-Q2']],
      ['during', '2025-01-01T00:00:00Z/2025-10-01T00:00:00Z', ['ev-2025-Q2']],
      ['contains', '2025-04-05T00:00:00Z/2025-06-15T00:00:00Z', ['ev-2025-Q2']],
      ['finishes', '2025-01-01T00:00:00Z/2025-07-01T00:00:00Z', ['ev-2025-Q2']],
      ['finished_by', '2025-06-01T00:00:00Z/2025-07-01T00:00:00Z', ['ev-2025-Q2']],
    ]
    for (const [r, v, names] of cases) {
      it(`filters ${r}`, async function () { await expectNames(r, v, names) })
    }

    it('as-of point lands in exactly one period at a seam', async function () {
      // the instant 2025-04-01T00:00:00Z is Q1's end and Q2's start — it belongs to Q2
      await expectNames('contains', '2025-04-01T00:00:00Z', ['ev-2025-Q2'])
      await expectNames('contains', '2025-05-15T00:00:00Z', ['ev-2025-Q2'])
      await expectNames('contains', '2025-01-01T00:00:00Z', ['ev-2025-Q1'])
    })

    it('an interval is never during a point', async function () {
      await expectNames('during', '2025-05-15T00:00:00Z', [])
    })

    it('rejects an unknown relation', async function () {
      const result = await client.query({ query: gql(rel('nonsense', '2025-01-01/2025-02-01')) }).catch(e => e)
      expect(result.graphQLErrors?.length || result.networkError).to.be.ok
    })
  })
})
