const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

describe('GraphQL _count', function () {
  let agent
  let client

  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
  }, {
    '@id': 'Person',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    age: 'xsd:integer',
    city: { '@type': 'Optional', '@class': 'xsd:string' },
  }, {
    '@id': 'Animal',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    species: 'xsd:string',
  }, {
    '@id': 'Parent',
    '@type': 'Class',
    name: 'xsd:string',
  }, {
    '@id': 'Child',
    '@type': 'Class',
    '@inherits': ['Parent'],
    age: 'xsd:integer',
  }]

  const instances = [
    { '@type': 'Person', name: 'Alice', age: 30, city: 'London' },
    { '@type': 'Person', name: 'Bob', age: 25, city: 'Paris' },
    { '@type': 'Person', name: 'Charlie', age: 35, city: 'London' },
    { '@type': 'Person', name: 'Diana', age: 28 },
    { '@type': 'Person', name: 'Eve', age: 30, city: 'Berlin' },
    { '@type': 'Animal', name: 'Fluffy', species: 'Cat' },
    { '@type': 'Animal', name: 'Rex', species: 'Dog' },
    { '@type': 'Animal', name: 'Whiskers', species: 'Cat' },
    { '@type': 'Child', name: 'Tommy', age: 5 },
    { '@type': 'Child', name: 'Sally', age: 8 },
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

    const cache = new InMemoryCache({
      addTypename: false,
    })

    client = new ApolloClient({
      cache,
      link: ComposedLink,
    })

    await db.create(agent)
    await document.insert(agent, { schema, fullReplace: true })
    await document.insert(agent, { instance: instances })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('basic count operations', function () {
    it('counts all documents of a type with empty filter', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Person: {})
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(5)
    })

    it('counts all documents of another type', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Animal: {})
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(3)
    })

    it('returns zero for type with no instances', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Parent: {})
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      // Parent has no direct instances (only Child instances which inherit from Parent)
      // But with include_children=true (default), it should count Child instances too
      expect(result.data._count).to.equal(2)
    })
  })

  describe('filtered count operations', function () {
    it('counts documents matching equality filter', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Person: { age: { eq: "30" } })
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(2) // Alice and Eve are both 30
    })

    it('counts documents matching string equality filter', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Person: { city: { eq: "London" } })
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(2) // Alice and Charlie in London
    })

    it('counts documents matching range filter (ge)', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Person: { age: { ge: "30" } })
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(3) // Alice (30), Charlie (35), Eve (30)
    })

    it('counts documents matching range filter (le)', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Person: { age: { le: "28" } })
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(2) // Bob (25), Diana (28)
    })

    it('counts documents matching species filter', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Animal: { species: { eq: "Cat" } })
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(2) // Fluffy and Whiskers
    })

    it('returns zero when no documents match filter', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Person: { age: { ge: "100" } })
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(0)
    })
  })

  describe('inheritance behavior', function () {
    it('counts child type instances directly', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Child: {})
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      expect(result.data._count).to.equal(2) // Tommy and Sally
    })

    it('counts parent type includes children by default', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Parent: {})
        }
      `
      const result = await client.query({ query: COUNT_QUERY })
      // Parent has no direct instances, but Child inherits from Parent
      // With include_children=true (default), should count Child instances
      expect(result.data._count).to.equal(2)
    })
  })

  describe('error handling', function () {
    it('returns error when no model filter is provided', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count
        }
      `
      try {
        await client.query({ query: COUNT_QUERY })
        expect.fail('Should have thrown an error')
      } catch (error) {
        expect(error.message).to.include('_count requires exactly one model filter argument')
      }
    })
  })

  describe('count combined with regular queries', function () {
    it('can execute count alongside regular query', async function () {
      const COMBINED_QUERY = gql`
        query CombinedQuery {
          _count(Person: { city: { eq: "London" } })
          Person(filter: { city: { eq: "London" } }) {
            name
            age
          }
        }
      `
      const result = await client.query({ query: COMBINED_QUERY })
      expect(result.data._count).to.equal(2)
      expect(result.data.Person).to.have.lengthOf(2)
      expect(result.data.Person.map(p => p.name).sort()).to.deep.equal(['Alice', 'Charlie'])
    })

    it('count matches length of filtered query result', async function () {
      const COUNT_QUERY = gql`
        query CountQuery {
          _count(Person: { age: { ge: "28" } })
        }
      `
      const DATA_QUERY = gql`
        query DataQuery {
          Person(filter: { age: { ge: "28" } }) {
            name
          }
        }
      `
      const countResult = await client.query({ query: COUNT_QUERY })
      const dataResult = await client.query({ query: DATA_QUERY })
      expect(countResult.data._count).to.equal(dataResult.data.Person.length)
    })
  })
})
