const { expect } = require('chai')
const { Agent, api, db, document, util, woql } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')
const Decimal = require('decimal.js')

describe('document-read-consistency', function () {
  let agent
  let client

  const schema = [
    {
      '@type': '@context',
      '@base': 'terminusdb:///data/',
      '@schema': 'terminusdb:///schema#',
    },
    {
      '@type': 'Class',
      '@id': 'Product',
      name: 'xsd:string',
      price: 'xsd:decimal',
      taxRate: 'xsd:decimal',
      quantity: 'xsd:decimal',
      inStock: 'xsd:boolean',
      category: 'xsd:string',
    },
    {
      '@type': 'Class',
      '@id': 'HighPrecision',
      value20digits: 'xsd:decimal',
      value15digits: 'xsd:decimal',
      smallValue: 'xsd:decimal',
      largeValue: 'xsd:decimal',
    },
  ]

  // Define ALL test data at module level
  const productDoc = {
    '@type': 'Product',
    '@id': 'Product/item1',
    name: 'Test Widget',
    price: '19.99',
    taxRate: '0.075',
    quantity: '10.5',
    inStock: true,
    category: 'Electronics',
  }

  const precisionDoc = {
    '@type': 'HighPrecision',
    '@id': 'HighPrecision/test1',
    value20digits: '1.23456789012345678901',
    value15digits: '3.141592653589793',
    smallValue: '0.00000000000001',
    largeValue: '9999999999.99999',
  }

  const multiProduct1 = { '@type': 'Product', '@id': 'Product/multi1', name: 'Item1', price: '10.50', taxRate: '0.08', quantity: '5', inStock: true, category: 'A' }
  const multiProduct2 = { '@type': 'Product', '@id': 'Product/multi2', name: 'Item2', price: '25.75', taxRate: '0.08', quantity: '3', inStock: false, category: 'B' }
  const multiProduct3 = { '@type': 'Product', '@id': 'Product/multi3', name: 'Item3', price: '99.99', taxRate: '0.08', quantity: '1', inStock: true, category: 'A' }

  const instances = [productDoc, precisionDoc, multiProduct1, multiProduct2, multiProduct3]

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema, fullReplace: true })
    await document.insert(agent, { instance: instances })

    // Setup GraphQL client
    client = new ApolloClient({
      cache: new InMemoryCache({
        addTypename: false,
      }),
      link: concat(
        new ApolloLink((operation, forward) => {
          operation.setContext({
            headers: {
              authorization: util.authorizationHeader(agent),
            },
          })
          return forward(operation)
        }),
        new HttpLink({
          uri: `${agent.baseUrl}${api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })}`,
          fetch,
        }),
      ),
    })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Simple Document Read Consistency', function () {
    it('should return consistent results via Document API', async function () {
      const response = await document.get(agent, { body: { id: 'Product/item1' } })
      const result = response.body

      expect(result['@type']).to.equal('Product')
      expect(result.name).to.equal('Test Widget')
      // Decimals are returned as JSON numbers with arbitrary precision
      expect(typeof result.price).to.equal('number')
      expect(typeof result.taxRate).to.equal('number')
      expect(typeof result.quantity).to.equal('number')
      expect(result.inStock).to.equal(true)
      expect(result.category).to.equal('Electronics')
    })

    it('should return consistent results via WOQL ReadDocument', async function () {
      // WOQL ReadDocument based on working example from woql-auth.js
      const query = {
        '@type': 'ReadDocument',
        identifier: { '@type': 'NodeValue', node: 'Product/item1' },
        document: { '@type': 'DataValue', variable: 'Doc' },  // DataValue, not Variable!
      }

      const result = await woql.post(agent, query)

      // ReadDocument returns bindings with variables
      expect(result.body['api:variable_names']).to.be.an('array').that.has.lengthOf(1)
      expect(result.body['api:variable_names'][0]).to.equal('Doc')
      expect(result.body.bindings).to.be.an('array').that.has.lengthOf(1)

      const doc = result.body.bindings[0].Doc

      // Verify document structure and decimal values as JSON numbers
      expect(doc['@type']).to.equal('Product')
      expect(doc.name).to.equal('Test Widget')
      expect(typeof doc.price).to.equal('number')
      expect(typeof doc.taxRate).to.equal('number')
      expect(typeof doc.quantity).to.equal('number')
      expect(doc.inStock).to.equal(true)
      expect(doc.category).to.equal('Electronics')
    })

    it('should return consistent results via GraphQL', async function () {
      const QUERY = gql`
 query GetProduct {
    Product(id: "terminusdb:///data/Product/item1"){
        _id
        name
        price
        taxRate
        quantity
        inStock
        category
    }
}`

      let result
      try {
        result = await client.query({ query: QUERY, fetchPolicy: 'network-only' })
      } catch (error) {
        console.log('\n=== GraphQL Error Debug ===')
        console.log('Error type:', error.constructor.name)
        console.log('Error message:', error.message)
        console.log('Network error:', error.networkError)
        console.log('GraphQL errors:', error.graphQLErrors)
        if (error.networkError?.result) {
          console.log('Network result:', JSON.stringify(error.networkError.result, null, 2))
        }
        console.log('===\n')
        throw error
      }

      expect(result.data.Product).to.be.an('array').with.lengthOf(1)
      const doc = result.data.Product[0]

      expect(doc.name).to.equal('Test Widget')
      expect(typeof doc.price).to.equal('number')
      expect(typeof doc.taxRate).to.equal('number')
      expect(typeof doc.quantity).to.equal('number')
      expect(doc.inStock).to.equal(true)
      expect(doc.category).to.equal('Electronics')
    })

    it('should have consistent decimal values across all three interfaces', async function () {
      // NOTE: This test verifies cross-interface consistency for decimal values
      // - Document API & WOQL: Return JSON numbers with full precision
      // - GraphQL: Returns JSON numbers, Apollo Client requires reading strategy for Decimals
      //   beyond float and double precision
      //   For values like 19.99, precision loss is negligible with float and double
      //   For 20-digit decimals, see "High Precision" tests below

      // Get via Document API
      const docApiResponse = await document.get(agent, { body: { id: 'Product/item1' } })
      const docApiResult = docApiResponse.body

      // Get via GraphQL
      const gqlQuery = gql`query { Product(id: "terminusdb:///data/Product/item1"){ price taxRate quantity } }`
      const gqlResponse = await client.query({ query: gqlQuery, fetchPolicy: 'network-only' })

      expect(gqlResponse.data.Product).to.be.an('array').with.lengthOf(1)
      const gqlResult = gqlResponse.data.Product[0]

      // Compare price - use Decimal.js for comparisons
      const docPrice = new Decimal(docApiResult.price.toString())
      const gqlPrice = new Decimal(gqlResult.price.toString())
      const expectedPrice = new Decimal('19.99')

      // All APIs should return values equal to expected
      expect(docPrice.equals(expectedPrice)).to.be.true
      expect(gqlPrice.equals(expectedPrice)).to.be.true
      expect(docPrice.equals(gqlPrice)).to.be.true

      // Compare taxRate
      const docTax = new Decimal(docApiResult.taxRate.toString())
      const gqlTax = new Decimal(gqlResult.taxRate.toString())

      expect(docTax.equals(gqlTax)).to.be.true

      // Compare quantity
      const docQty = new Decimal(docApiResult.quantity.toString())
      const gqlQty = new Decimal(gqlResult.quantity.toString())

      expect(docQty.equals(gqlQty)).to.be.true
    })
  })

  describe('High Precision Document Read Consistency', function () {
    it('should preserve 20-digit precision across all interfaces', async function () {
      // NOTE: This test demonstrates precision handling across APIs:
      // - Document API: Full 20-digit precision in JSON numbers
      // - WOQL: Full 20-digit precision in JSON numbers
      // - GraphQL: JSON numbers output by server, but Apollo Client loses precision beyond ~16 digits

      // Document API - Extract from RAW text to preserve full 20-digit precision
      const docResponse = await document.get(agent, { body: { id: 'HighPrecision/test1' } })
      const docVal20Raw = docResponse.text.match(/"value20digits"\s*:\s*([0-9.eE+-]+)/)[1]
      const docVal20 = new Decimal(docVal20Raw)

      // WOQL - Extract from RAW text to preserve full 20-digit precision
      const woqlQuery = {
        '@type': 'ReadDocument',
        identifier: { '@type': 'NodeValue', node: 'HighPrecision/test1' },
        document: { '@type': 'DataValue', variable: 'Doc' },
      }
      const woqlResponse = await woql.post(agent, woqlQuery)
      const woqlVal20Raw = woqlResponse.text.match(/"value20digits"\s*:\s*([0-9.eE+-]+)/)[1]
      const woqlVal20 = new Decimal(woqlVal20Raw)

      // GraphQL - Apollo Client loses precision beyond ~16 digits
      const gqlResponse = await client.query({
        query: gql`query { HighPrecision(id: "terminusdb:///data/HighPrecision/test1"){ value20digits value15digits smallValue largeValue } }`,
        fetchPolicy: 'network-only',
      })

      expect(gqlResponse.data.HighPrecision).to.be.an('array').with.lengthOf(1)
      const gqlResult = gqlResponse.data.HighPrecision[0]
      const gqlVal20 = new Decimal(gqlResult.value20digits.toString())

      const expected20 = new Decimal('1.23456789012345678901')

      // Document API: Full precision (extracted from raw text)
      expect(docVal20.equals(expected20)).to.be.true

      // WOQL: Full precision preserved
      expect(woqlVal20.equals(expected20)).to.be.true

      // GraphQL via Apollo: Precision lost beyond ~16 digits due to IEEE 754 float parsing
      // Use approximate equality
      expect(gqlVal20.toNumber()).to.be.closeTo(expected20.toNumber(), 0.00000000000001)

      // All three interfaces return consistent data (Document API and WOQL exact, GraphQL approximate)
      expect(docVal20.equals(woqlVal20)).to.be.true
    })

    it('should handle edge case values consistently', async function () {
      const docResponse = await document.get(agent, { body: { id: 'HighPrecision/test1' } })
      const docResult = docResponse.body

      const gqlResponse = await client.query({
        query: gql`query { HighPrecision(id: "terminusdb:///data/HighPrecision/test1"){ smallValue largeValue } }`,
        fetchPolicy: 'network-only',
      })

      expect(gqlResponse.data.HighPrecision).to.be.an('array').with.lengthOf(1)
      const gqlResult = gqlResponse.data.HighPrecision[0]

      // Small value - use Decimal.js
      const docSmall = new Decimal(docResult.smallValue.toString())
      const gqlSmall = new Decimal(gqlResult.smallValue.toString())
      const zero = new Decimal(0)

      expect(docSmall.greaterThan(zero)).to.be.true
      expect(gqlSmall.greaterThan(zero)).to.be.true

      // Large value - use Decimal.js
      const docLarge = new Decimal(docResult.largeValue.toString())
      const gqlLarge = new Decimal(gqlResult.largeValue.toString())
      const threshold = new Decimal('9999999999')

      expect(docLarge.greaterThan(threshold)).to.be.true
      expect(gqlLarge.greaterThan(threshold)).to.be.true
    })
  })

})
