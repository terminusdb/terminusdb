const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache, HttpLink, gql,
} = require('@apollo/client/core')
const fetch = require('cross-fetch')

describe('sys:JSON GraphQL with @-Prefixed Properties', function () {
  let agent
  let client

  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
  }, {
    '@id': 'JSONTestDoc',
    '@type': 'Class',
    '@key': { '@type': 'Random' },
    name: 'xsd:string',
    data: 'sys:JSON',
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

    await db.create(agent, { label: 'sys:JSON GraphQL At-Prefix Test', schema: true })
    await document.insert(agent, { schema, fullReplace: true })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('GraphQL with @id and @type in sys:JSON', function () {
    it('should insert and retrieve document with @id and @type via GraphQL', async function () {
      // Insert document via REST API (which handles escaping automatically)
      await document.insert(agent, {
        instance: {
          '@type': 'JSONTestDoc',
          name: 'test-at-prefix',
          data: {
            '@id': 'my-identifier',
            '@type': 'MyType',
            title: 'Test Document',
            nested: {
              '@id': 'nested-id',
              '@type': 'NestedType',
              value: 42,
            },
          },
        },
      })

      // Query via GraphQL
      const query = gql`
        query {
          JSONTestDoc {
            _id
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query })

      expect(gqlResult.data.JSONTestDoc).to.be.an('array').with.lengthOf(1)
      const doc = gqlResult.data.JSONTestDoc[0]

      expect(doc.name).to.equal('test-at-prefix')
      expect(doc.data).to.be.a('string')

      // Parse the JSON string and verify @-prefixed properties are preserved
      const parsedData = JSON.parse(doc.data)
      expect(parsedData).to.deep.equal({
        '@id': 'my-identifier',
        '@type': 'MyType',
        title: 'Test Document',
        nested: {
          '@id': 'nested-id',
          '@type': 'NestedType',
          value: 42,
        },
      })
    })

    it('should handle multiple @-prefixed properties in sys:JSON', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONTestDoc',
          name: 'test-multiple-at-prefix',
          data: {
            '@id': 'doc-123',
            '@type': 'ComplexDoc',
            '@context': 'https://schema.org',
            '@value': 'important',
            '@language': 'en',
            '@container': '@set',
            content: 'Test content',
          },
        },
      })

      const query = gql`
        query {
          JSONTestDoc(filter: { name: { eq: "test-multiple-at-prefix" } }) {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query })

      expect(gqlResult.data.JSONTestDoc).to.be.an('array').with.lengthOf(1)
      const doc = gqlResult.data.JSONTestDoc[0]

      const parsedData = JSON.parse(doc.data)
      expect(parsedData).to.deep.equal({
        '@id': 'doc-123',
        '@type': 'ComplexDoc',
        '@context': 'https://schema.org',
        '@value': 'important',
        '@language': 'en',
        '@container': '@set',
        content: 'Test content',
      })
    })

    it('should handle @id and @type with special characters', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONTestDoc',
          name: 'test-special-chars',
          data: {
            '@id': 'https://example.com/doc/123',
            '@type': 'schema:Document',
            'special-key': 'value',
            'key with spaces': 'another value',
          },
        },
      })

      const query = gql`
        query {
          JSONTestDoc(filter: { name: { eq: "test-special-chars" } }) {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query })

      expect(gqlResult.data.JSONTestDoc).to.be.an('array').with.lengthOf(1)
      const parsedData = JSON.parse(gqlResult.data.JSONTestDoc[0].data)

      expect(parsedData['@id']).to.equal('https://example.com/doc/123')
      expect(parsedData['@type']).to.equal('schema:Document')
    })

    it('should handle arrays with @-prefixed properties', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONTestDoc',
          name: 'test-arrays',
          data: {
            items: [
              {
                '@id': 'item-1',
                '@type': 'Item',
                value: 'first',
              },
              {
                '@id': 'item-2',
                '@type': 'Item',
                value: 'second',
              },
            ],
          },
        },
      })

      const query = gql`
        query {
          JSONTestDoc(filter: { name: { eq: "test-arrays" } }) {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query })

      const parsedData = JSON.parse(gqlResult.data.JSONTestDoc[0].data)
      expect(parsedData.items).to.be.an('array').with.lengthOf(2)
      expect(parsedData.items[0]).to.deep.equal({
        '@id': 'item-1',
        '@type': 'Item',
        value: 'first',
      })
      expect(parsedData.items[1]).to.deep.equal({
        '@id': 'item-2',
        '@type': 'Item',
        value: 'second',
      })
    })

    it('should handle null and empty string @-prefixed values', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONTestDoc',
          name: 'test-null-empty',
          data: {
            '@id': '',
            '@type': null,
            content: 'test',
          },
        },
      })

      const query = gql`
        query {
          JSONTestDoc(filter: { name: { eq: "test-null-empty" } }) {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query })

      const parsedData = JSON.parse(gqlResult.data.JSONTestDoc[0].data)
      expect(parsedData['@id']).to.equal('')
      expect(parsedData['@type']).to.be.null
      expect(parsedData.content).to.equal('test')
    })

    it('should handle deeply nested @-prefixed properties', async function () {
      await document.insert(agent, {
        instance: {
          '@type': 'JSONTestDoc',
          name: 'test-deep-nesting',
          data: {
            '@id': 'root',
            '@type': 'Root',
            level1: {
              '@id': 'level1-id',
              '@type': 'Level1',
              level2: {
                '@id': 'level2-id',
                '@type': 'Level2',
                level3: {
                  '@id': 'level3-id',
                  '@type': 'Level3',
                  value: 'deep',
                },
              },
            },
          },
        },
      })

      const query = gql`
        query {
          JSONTestDoc(filter: { name: { eq: "test-deep-nesting" } }) {
            name
            data
          }
        }
      `

      const gqlResult = await client.query({ query })

      const parsedData = JSON.parse(gqlResult.data.JSONTestDoc[0].data)
      expect(parsedData.level1.level2.level3).to.deep.equal({
        '@id': 'level3-id',
        '@type': 'Level3',
        value: 'deep',
      })
    })
  })
})
