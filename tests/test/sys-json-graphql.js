const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

describe('sys:JSON GraphQL Compatibility', function () {
  let agent
  let client

  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
  }, {
    '@id': 'GeoJSON',
    '@type': 'Class',
    '@abstract': [],
  }, {
    '@id': 'FeatureCollection_Type',
    '@type': 'Enum',
    '@value': ['FeatureCollection'],
  }, {
    '@id': 'Feature',
    '@type': 'Class',
    '@key': { '@type': 'Random' },
    name: 'xsd:string',
    properties: { '@type': 'Optional', '@class': 'sys:JSON' },
  }, {
    '@id': 'FeatureCollection',
    '@type': 'Class',
    '@inherits': 'GeoJSON',
    '@key': { '@type': 'Random' },
    '@unfoldable': [],
    crs: { '@type': 'Optional', '@class': 'sys:JSON' },
    features: { '@type': 'Set', '@class': 'Feature' },
    name: { '@type': 'Optional', '@class': 'xsd:string' },
    properties: { '@type': 'Optional', '@class': 'sys:JSON' },
    type: 'FeatureCollection_Type',
  }, {
    '@id': 'JSONDocument',
    '@type': 'Class',
    '@key': { '@type': 'Random' },
    name: 'xsd:string',
    data: 'sys:JSON',
  }, {
    '@id': 'JSONOptionalDocument',
    '@type': 'Class',
    '@key': { '@type': 'Random' },
    name: 'xsd:string',
    metadata: { '@type': 'Optional', '@class': 'sys:JSON' },
  }]

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

    await db.create(agent, { label: 'sys:JSON GraphQL Test', schema: true })
    await document.insert(agent, { schema, fullReplace: true })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('sys:JSON fields should return as strings in GraphQL', function () {
    it('should return sys:JSON as string for simple JSON object', async function () {
      // Insert document with sys:JSON field
      await document.insert(agent, {
        instance: {
          '@type': 'JSONDocument',
          name: 'test-doc',
          data: {
            key: 'value',
            nested: { prop: 123 },
          },
        },
      })

      // Query via GraphQL
      const QUERY = gql`
        query JSONDocumentQuery {
          JSONDocument {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query: QUERY })

      expect(gqlResult.data.JSONDocument).to.be.an('array').with.lengthOf(1)
      expect(gqlResult.data.JSONDocument[0].name).to.equal('test-doc')

      // CRITICAL: sys:JSON should be returned as a STRING, not an object
      expect(gqlResult.data.JSONDocument[0].data).to.be.a('string')

      // Verify it's valid JSON when parsed
      const parsed = JSON.parse(gqlResult.data.JSONDocument[0].data)
      expect(parsed).to.deep.equal({
        key: 'value',
        nested: { prop: 123 },
      })
    })

    it('should return optional sys:JSON as string', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONOptionalDocument',
          name: 'optional-doc',
          metadata: {
            source: 'import',
            tags: ['tag1', 'tag2'],
          },
        },
      })

      const QUERY = gql`
        query JSONOptionalQuery {
          JSONOptionalDocument {
            name
            metadata
          }
        }
      `

      const gqlResult = await client.query({ query: QUERY })

      expect(gqlResult.data.JSONOptionalDocument).to.be.an('array').with.lengthOf(1)
      expect(gqlResult.data.JSONOptionalDocument[0].name).to.equal('optional-doc')

      // Optional sys:JSON should also be a string
      expect(gqlResult.data.JSONOptionalDocument[0].metadata).to.be.a('string')

      const parsed = JSON.parse(gqlResult.data.JSONOptionalDocument[0].metadata)
      expect(parsed).to.deep.equal({
        source: 'import',
        tags: ['tag1', 'tag2'],
      })
    })

    it('should return sys:JSON as string in FeatureCollection (user-reported case)', async function () {
      // Create a Feature first
      const featureResult = await document.insert(agent, {
        instance: {
          '@type': 'Feature',
          name: 'test-feature',
          properties: {
            id: 'feature-1',
            category: 'test',
          },
        },
      })

      const featureId = featureResult.body[0]

      // Create FeatureCollection with sys:JSON fields
      await document.insert(agent, {
        instance: {
          '@type': 'FeatureCollection',
          type: 'FeatureCollection',
          name: 'test-collection',
          features: [featureId],
          crs: {
            type: 'name',
            properties: {
              name: 'EPSG:4326',
            },
          },
          properties: {
            description: 'Test collection',
            version: 1,
          },
        },
      })

      // Query via GraphQL - this is where the user reported failure
      const QUERY = gql`
        query FeatureCollectionQuery {
          FeatureCollection {
            name
            type
            crs
            properties
            features {
              name
              properties
            }
          }
        }
      `

      const gqlResult = await client.query({ query: QUERY })

      expect(gqlResult.data.FeatureCollection).to.be.an('array').with.lengthOf(1)
      const collection = gqlResult.data.FeatureCollection[0]

      expect(collection.name).to.equal('test-collection')
      expect(collection.type).to.equal('FeatureCollection')

      // CRITICAL: crs and properties should be strings, not objects
      expect(collection.crs).to.be.a('string')
      expect(collection.properties).to.be.a('string')

      // Verify they're valid JSON
      const crs = JSON.parse(collection.crs)
      expect(crs).to.deep.equal({
        type: 'name',
        properties: {
          name: 'EPSG:4326',
        },
      })

      const props = JSON.parse(collection.properties)
      expect(props).to.deep.equal({
        description: 'Test collection',
        version: 1,
      })

      // Feature properties should also be a string
      expect(collection.features).to.be.an('array').with.lengthOf(1)
      expect(collection.features[0].properties).to.be.a('string')
    })

    it('should handle sys:JSON arrays as strings', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONDocument',
          name: 'array-doc',
          data: ['item1', 'item2', 'item3'],
        },
      })

      const QUERY = gql`
        query ArrayQuery {
          JSONDocument(filter: {name: {eq: "array-doc"}}) {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query: QUERY })

      expect(gqlResult.data.JSONDocument).to.be.an('array').with.lengthOf(1)
      expect(gqlResult.data.JSONDocument[0].data).to.be.a('string')

      const parsed = JSON.parse(gqlResult.data.JSONDocument[0].data)
      expect(parsed).to.deep.equal(['item1', 'item2', 'item3'])
    })

    it('should handle sys:JSON primitives as strings', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONDocument',
          name: 'primitive-doc',
          data: 'plain string value',
        },
      })

      const QUERY = gql`
        query PrimitiveQuery {
          JSONDocument(filter: {name: {eq: "primitive-doc"}}) {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query: QUERY })

      expect(gqlResult.data.JSONDocument).to.be.an('array').with.lengthOf(1)
      expect(gqlResult.data.JSONDocument[0].data).to.be.a('string')

      // For primitive strings, the JSON encoding will add quotes
      const parsed = JSON.parse(gqlResult.data.JSONDocument[0].data)
      expect(parsed).to.equal('plain string value')
    })
  })
})
