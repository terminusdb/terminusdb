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

  const schema = [
    {
      '@id': 'Event',
      '@type': 'Class',
      '@key': {
        '@type': 'Lexical',
        '@fields': ['name'],
      },
      name: 'xsd:string',
      interval: 'xdd:dateTimeInterval',
    },
    {
      '@id': 'InstantEvent',
      '@type': 'Class',
      '@key': {
        '@type': 'Lexical',
        '@fields': ['name'],
      },
      name: 'xsd:string',
      interval: 'xdd:dateTimeInterval',
    },
  ]

  const events = [
    { '@type': 'Event', name: 'ev-2024-H1', interval: '2024-01-01/2024-06-30' },
    { '@type': 'Event', name: 'ev-2024-H2', interval: '2024-07-01/2024-12-31' },
    { '@type': 'Event', name: 'ev-2025-Q1', interval: '2025-01-01/2025-03-31' },
    { '@type': 'Event', name: 'ev-2025-Q2', interval: '2025-04-01/2025-06-30' },
    { '@type': 'Event', name: 'ev-2025-Q3', interval: '2025-07-01/2025-09-30' },
  ]

  // Single-component spellings (issue #2626): each document exercises one
  // literal form through store → dictionary → GraphQL render.
  const instantEvents = [
    { '@type': 'InstantEvent', name: 'inst-offset', interval: '2025-03-15T00:00:00+02:00' },
    { '@type': 'InstantEvent', name: 'inst-z', interval: '2025-03-15T00:00:00Z' },
    { '@type': 'InstantEvent', name: 'inst-local', interval: '2025-03-15T00:00:00' },
    { '@type': 'InstantEvent', name: 'inst-pair', interval: '2025-03-15T00:00:00+02:00/2025-03-15T00:00:00+02:00' },
    { '@type': 'InstantEvent', name: 'inst-day', interval: '2026-10-01' },
    { '@type': 'InstantEvent', name: 'inst-datepair', interval: '2025-01-01/2025-03-31' },
    { '@type': 'InstantEvent', name: 'inst-zonepair', interval: '2025-01-01T00:00:00/2025-04-01T00:00:00' },
    { '@type': 'InstantEvent', name: 'inst-mixed', interval: '2025-01-01/2025-04-01T00:00:00' },
    { '@type': 'InstantEvent', name: 'inst-pd', interval: 'P3M/2025-03-31' },
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

    await db.create(agent) // NOSONAR tests/lib helpers return thenables, not Promises
    await document.insert(agent, { schema }) // NOSONAR
    await document.insert(agent, { instance: events }) // NOSONAR
    await document.insert(agent, { instance: instantEvents }) // NOSONAR
  })

  after(async function () {
    await db.delete(agent) // NOSONAR
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

  describe('single-instant literals (issue #2626)', function () {
    // Stored spellings — inst-offset/inst-z/inst-local are bare instant
    // points, inst-pair is an explicit zero-duration pair, inst-day a bare
    // ISO date covering its whole day.
    const intervalOf = async (name) => {
      const result = await client.query({
        query: gql`query { InstantEvent(filter: { name: { eq: "${name}" }}) { interval } }`,
      })
      expect(result.data.InstantEvent).to.have.lengthOf(1)
      return result.data.InstantEvent[0].interval
    }
    const namesFor = async (filter) => {
      const result = await client.query({
        query: gql`query { InstantEvent(filter: { interval: ${filter} }) { name } }`,
      })
      return result.data.InstantEvent.map(e => e.name).sort()
    }

    it('renders a stored bare instant with offset as the single timestamp', async function () {
      expect(await intervalOf('inst-offset')).to.equal('2025-03-15T00:00:00+02:00')
    })

    it('renders a stored bare instant with Z as the single timestamp', async function () {
      expect(await intervalOf('inst-z')).to.equal('2025-03-15T00:00:00Z')
    })

    it('renders a stored bare zoneless instant as the single timestamp', async function () {
      expect(await intervalOf('inst-local')).to.equal('2025-03-15T00:00:00')
    })

    it('renders a stored explicit zero-duration pair as the pair', async function () {
      expect(await intervalOf('inst-pair')).to.equal('2025-03-15T00:00:00+02:00/2025-03-15T00:00:00+02:00')
    })

    it('renders a stored bare date as the single date', async function () {
      expect(await intervalOf('inst-day')).to.equal('2026-10-01')
    })

    it('renders a stored inclusive date pair as the date spellings', async function () {
      expect(await intervalOf('inst-datepair')).to.equal('2025-01-01/2025-03-31')
    })

    it('renders a stored zoneless datetime pair verbatim', async function () {
      expect(await intervalOf('inst-zonepair')).to.equal('2025-01-01T00:00:00/2025-04-01T00:00:00')
    })

    it('renders a stored mixed date/datetime pair verbatim', async function () {
      expect(await intervalOf('inst-mixed')).to.equal('2025-01-01/2025-04-01T00:00:00')
    })

    it('renders a stored duration-end with a date as the supplied spelling', async function () {
      expect(await intervalOf('inst-pd')).to.equal('P3M/2025-03-31')
    })

    it('eq matches a bare instant under either spelling', async function () {
      // eq compares interval bounds: the point and the equal-bound pair are
      // the same [t,t) value even though their literals stay distinct
      const pair = '2025-03-15T00:00:00+02:00/2025-03-15T00:00:00+02:00'
      expect(await namesFor('{ eq: "2025-03-15T00:00:00+02:00" }')).to.deep.equal(['inst-offset', 'inst-pair'])
      expect(await namesFor(`{ eq: "${pair}" }`)).to.deep.equal(['inst-offset', 'inst-pair'])
    })

    it('ne excludes every spelling of the same instant', async function () {
      const v = '2025-03-15T00:00:00+02:00/2025-03-15T00:00:00+02:00'
      const names = await namesFor(`{ ne: "${v}" }`)
      expect(names).to.not.include('inst-offset')
      expect(names).to.not.include('inst-pair')
    })

    it('eq matches a stored bare date under either day spelling', async function () {
      expect(await namesFor('{ eq: "2026-10-01" }')).to.deep.equal(['inst-day'])
      expect(await namesFor('{ eq: "2026-10-01/2026-10-01" }')).to.deep.equal(['inst-day'])
    })

    it('relationship equals treats a bare date as the D/D pair', async function () {
      // same [D, D+1) interval — spelling is the only difference
      expect(await namesFor('{ relationship: { rel: "equals", value: "2026-10-01" } }')).to.deep.equal(['inst-day'])
      expect(await namesFor('{ relationship: { rel: "equals", value: "2026-10-01/2026-10-01" } }')).to.deep.equal(['inst-day'])
    })

    it('relationship contains on a bare date covers the whole day', async function () {
      const result = await client.query({
        query: gql`query { Event(filter: { interval: { relationship: { rel: "contains", value: "2025-04-01" } }}) { name } }`,
      })
      expect(result.data.Event.map(e => e.name)).to.deep.equal(['ev-2025-Q2'])
    })

    it('a date pair and its zoneless datetime twin are the same period', async function () {
      // `2025-01-01/2025-03-31`, `2025-01-01T00:00:00/2025-04-01T00:00:00`,
      // the mixed spelling and the duration form are all [Jan 1, Apr 1):
      // eq compares bounds, so each spelling matches every stored variant
      const samePeriod = ['inst-datepair', 'inst-mixed', 'inst-pd', 'inst-zonepair']
      expect(await namesFor('{ eq: "2025-01-01/2025-03-31" }')).to.deep.equal(samePeriod)
      expect(await namesFor('{ eq: "2025-01-01T00:00:00/2025-04-01T00:00:00" }')).to.deep.equal(samePeriod)
      expect(await namesFor('{ relationship: { rel: "equals", value: "2025-01-01T00:00:00/2025-04-01T00:00:00" } }'))
        .to.deep.equal(samePeriod)
    })
  })
})
