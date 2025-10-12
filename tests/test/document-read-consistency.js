const { expect } = require('chai')
const { Agent, api, db, document, woql } = require('../lib')
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

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema })

    // Setup GraphQL client
    client = new ApolloClient({
      cache: new InMemoryCache(),
      link: concat(
        new ApolloLink((operation, forward) => {
          operation.setContext({
            headers: {
              authorization: api.util.autorizationHeader(agent),
            },
          })
          return forward(operation)
        }),
        new HttpLink({
          uri: api.path(agent, 'graphql', {}),
          fetch,
        }),
      ),
    })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Simple Document Read Consistency', function () {
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

    before(async function () {
      await document.insert(agent, productDoc)
    })

    it('should return consistent results via Document API', async function () {
      const result = await document.get(agent, { id: 'Product/item1' })

      expect(result['@type']).to.equal('Product')
      expect(result.name).to.equal('Test Widget')
      expect(typeof result.price).to.equal('number')
      expect(typeof result.taxRate).to.equal('number')
      expect(typeof result.quantity).to.equal('number')
      expect(result.inStock).to.equal(true)
      expect(result.category).to.equal('Electronics')
    })

    it('should return consistent results via WOQL ReadDocument', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'ReadDocument',
            identifier: { '@type': 'NodeValue', node: 'Product/item1' },
            document: { '@type': 'Variable', variable: 'Doc' },
          },
          {
            '@type': 'Get',
            columns: [
              { '@type': 'Column', indicator: { '@type': 'Indicator', name: 'Doc' } },
            ],
          },
        ],
      }

      const result = await woql.post(agent, query)

      expect(result.body.bindings).to.be.an('array').with.lengthOf(1)
      const doc = result.body.bindings[0].Doc

      expect(doc['@type']['@value']).to.equal('Product')
      expect(doc.name['@value']).to.equal('Test Widget')
      expect(typeof doc.price['@value']).to.equal('number')
      expect(typeof doc.taxRate['@value']).to.equal('number')
      expect(typeof doc.quantity['@value']).to.equal('number')
      expect(doc.inStock['@value']).to.equal(true)
      expect(doc.category['@value']).to.equal('Electronics')
    })

    it('should return consistent results via GraphQL', async function () {
      const QUERY = gql`
        query GetProduct {
          Product(id: "Product/item1") {
            name
            price
            taxRate
            quantity
            inStock
            category
          }
        }
      `

      const result = await client.query({ query: QUERY })

      expect(result.data.Product).to.be.an('array').with.lengthOf(1)
      const doc = result.data.Product[0]

      expect(doc.name).to.equal('Test Widget')
      expect(typeof doc.price).to.equal('string')
      expect(typeof doc.taxRate).to.equal('string')
      expect(typeof doc.quantity).to.equal('string')
      expect(doc.inStock).to.equal(true)
      expect(doc.category).to.equal('Electronics')
    })

    it('should have consistent decimal values across all three interfaces', async function () {
      // Get via Document API
      const docApiResult = await document.get(agent, { id: 'Product/item1' })

      // Get via WOQL
      const woqlQuery = {
        '@type': 'ReadDocument',
        identifier: { '@type': 'NodeValue', node: 'Product/item1' },
        document: { '@type': 'Variable', variable: 'Doc' },
      }
      const woqlResponse = await woql.post(agent, {
        '@type': 'And',
        and: [woqlQuery, { '@type': 'Get', columns: [{ '@type': 'Column', indicator: { '@type': 'Indicator', name: 'Doc' } }] }],
      })
      const woqlResult = woqlResponse.body.bindings[0].Doc

      // Get via GraphQL
      const gqlQuery = gql`query { Product(id: "Product/item1") { price taxRate quantity } }`
      const gqlResponse = await client.query({ query: gqlQuery })
      const gqlResult = gqlResponse.data.Product[0]

      // Compare price - use Decimal.js for exact precision
      const docPrice = new Decimal(docApiResult.price.toString())
      const woqlPrice = new Decimal(woqlResult.price['@value'])
      const gqlPrice = new Decimal(gqlResult.price)
      const expectedPrice = new Decimal('19.99')

      // Exact equality checks (not tolerance-based)
      expect(docPrice.equals(expectedPrice)).to.be.true
      expect(woqlPrice.equals(expectedPrice)).to.be.true
      expect(gqlPrice.equals(expectedPrice)).to.be.true
      expect(docPrice.equals(woqlPrice)).to.be.true
      expect(docPrice.equals(gqlPrice)).to.be.true

      // Compare taxRate
      const docTax = new Decimal(docApiResult.taxRate.toString())
      const woqlTax = new Decimal(woqlResult.taxRate['@value'])
      const gqlTax = new Decimal(gqlResult.taxRate)

      expect(docTax.equals(woqlTax)).to.be.true
      expect(docTax.equals(gqlTax)).to.be.true

      // Compare quantity
      const docQty = new Decimal(docApiResult.quantity.toString())
      const woqlQty = new Decimal(woqlResult.quantity['@value'])
      const gqlQty = new Decimal(gqlResult.quantity)

      expect(docQty.equals(woqlQty)).to.be.true
      expect(docQty.equals(gqlQty)).to.be.true
    })
  })

  describe('High Precision Document Read Consistency', function () {
    const precisionDoc = {
      '@type': 'HighPrecision',
      '@id': 'HighPrecision/test1',
      value20digits: '1.23456789012345678901',
      value15digits: '3.141592653589793',
      smallValue: '0.00000000000001',
      largeValue: '9999999999.99999',
    }

    before(async function () {
      await document.insert(agent, precisionDoc)
    })

    it('should preserve 20-digit precision across all interfaces', async function () {
      // Document API
      const docResult = await document.get(agent, { id: 'HighPrecision/test1' })

      // WOQL
      const woqlResponse = await woql.post(agent, {
        '@type': 'And',
        and: [
          { '@type': 'ReadDocument', identifier: { '@type': 'NodeValue', node: 'HighPrecision/test1' }, document: { '@type': 'Variable', variable: 'D' } },
          { '@type': 'Get', columns: [{ '@type': 'Column', indicator: { '@type': 'Indicator', name: 'D' } }] },
        ],
      })
      const woqlResult = woqlResponse.body.bindings[0].D

      // GraphQL
      const gqlResponse = await client.query({
        query: gql`query { HighPrecision(id: "HighPrecision/test1") { value20digits value15digits smallValue largeValue } }`,
      })
      const gqlResult = gqlResponse.data.HighPrecision[0]

      // Compare 20-digit value - use Decimal.js for exact precision
      const docVal20 = new Decimal(docResult.value20digits.toString())
      const woqlVal20 = new Decimal(woqlResult.value20digits['@value'])
      const gqlVal20 = new Decimal(gqlResult.value20digits)
      const expected20 = new Decimal('1.23456789012345678901')

      // Exact equality checks with 20-digit precision
      expect(docVal20.equals(expected20)).to.be.true
      expect(woqlVal20.equals(expected20)).to.be.true
      expect(gqlVal20.equals(expected20)).to.be.true

      // All three should return the exact same value
      expect(docVal20.equals(woqlVal20)).to.be.true
      expect(docVal20.equals(gqlVal20)).to.be.true
    })

    it('should handle edge case values consistently', async function () {
      const docResult = await document.get(agent, { id: 'HighPrecision/test1' })

      const woqlResponse = await woql.post(agent, {
        '@type': 'And',
        and: [
          { '@type': 'ReadDocument', identifier: { '@type': 'NodeValue', node: 'HighPrecision/test1' }, document: { '@type': 'Variable', variable: 'D' } },
          { '@type': 'Get', columns: [{ '@type': 'Column', indicator: { '@type': 'Indicator', name: 'D' } }] },
        ],
      })
      const woqlResult = woqlResponse.body.bindings[0].D

      const gqlResponse = await client.query({
        query: gql`query { HighPrecision(id: "HighPrecision/test1") { smallValue largeValue } }`,
      })
      const gqlResult = gqlResponse.data.HighPrecision[0]

      // Small value - use Decimal.js
      const docSmall = new Decimal(docResult.smallValue.toString())
      const woqlSmall = new Decimal(woqlResult.smallValue['@value'])
      const gqlSmall = new Decimal(gqlResult.smallValue)
      const zero = new Decimal(0)

      expect(docSmall.greaterThan(zero)).to.be.true
      expect(woqlSmall.greaterThan(zero)).to.be.true
      expect(gqlSmall.greaterThan(zero)).to.be.true

      // Large value - use Decimal.js
      const docLarge = new Decimal(docResult.largeValue.toString())
      const woqlLarge = new Decimal(woqlResult.largeValue['@value'])
      const gqlLarge = new Decimal(gqlResult.largeValue)
      const threshold = new Decimal('9999999999')

      expect(docLarge.greaterThan(threshold)).to.be.true
      expect(woqlLarge.greaterThan(threshold)).to.be.true
      expect(gqlLarge.greaterThan(threshold)).to.be.true
    })
  })

  describe('Multiple Documents Read Consistency', function () {
    before(async function () {
      await document.insert(agent, [
        { '@type': 'Product', '@id': 'Product/multi1', name: 'Item1', price: '10.50', taxRate: '0.08', quantity: '5', inStock: true, category: 'A' },
        { '@type': 'Product', '@id': 'Product/multi2', name: 'Item2', price: '25.75', taxRate: '0.08', quantity: '3', inStock: false, category: 'B' },
        { '@type': 'Product', '@id': 'Product/multi3', name: 'Item3', price: '99.99', taxRate: '0.08', quantity: '1', inStock: true, category: 'A' },
      ])
    })

    it('should return consistent count across all interfaces', async function () {
      // Document API - get all
      const docResult = await document.get(agent, { type: 'Product' })
      const docCount = docResult.filter(d => d['@id'].startsWith('Product/multi')).length

      // WOQL - read all
      const woqlResponse = await woql.post(agent, {
        '@type': 'And',
        and: [
          { '@type': 'Triple', subject: { '@type': 'Variable', variable: 'ID' }, predicate: { '@type': 'NodeValue', node: 'rdf:type' }, object: { '@type': 'NodeValue', node: 'Product' } },
          { '@type': 'ReadDocument', identifier: { '@type': 'Variable', variable: 'ID' }, document: { '@type': 'Variable', variable: 'Doc' } },
          { '@type': 'Get', columns: [{ '@type': 'Column', indicator: { '@type': 'Indicator', name: 'Doc' } }] },
        ],
      })
      const woqlCount = woqlResponse.body.bindings.filter(b => b.Doc['@id']['@value'].startsWith('Product/multi')).length

      // GraphQL - query all
      const gqlResponse = await client.query({ query: gql`query { Product { name } }` })
      const gqlCount = gqlResponse.data.Product.filter(p => p.name.startsWith('Item')).length

      expect(docCount).to.equal(3)
      expect(woqlCount).to.equal(3)
      expect(gqlCount).to.equal(3)
    })

    it('should return documents in consistent order when sorted', async function () {
      // Document API doesn't support sorting directly, get all and sort
      const docResult = await document.get(agent, { type: 'Product' })
      const docSorted = docResult
        .filter(d => d['@id'].startsWith('Product/multi'))
        .sort((a, b) => a.price - b.price)
        .map(d => d.name)

      // WOQL with ordering
      const woqlResponse = await woql.post(agent, {
        '@type': 'And',
        and: [
          { '@type': 'Triple', subject: { '@type': 'Variable', variable: 'ID' }, predicate: { '@type': 'NodeValue', node: 'rdf:type' }, object: { '@type': 'NodeValue', node: 'Product' } },
          { '@type': 'ReadDocument', identifier: { '@type': 'Variable', variable: 'ID' }, document: { '@type': 'Variable', variable: 'Doc' } },
          { '@type': 'Get', columns: [{ '@type': 'Column', indicator: { '@type': 'Indicator', name: 'Doc' } }] },
        ],
      })
      const woqlSorted = woqlResponse.body.bindings
        .filter(b => b.Doc['@id']['@value'].startsWith('Product/multi'))
        .sort((a, b) => a.Doc.price['@value'] - b.Doc.price['@value'])
        .map(b => b.Doc.name['@value'])

      // GraphQL with ordering
      const gqlResponse = await client.query({
        query: gql`query { Product(orderBy: { price: ASC }) { name price } }`,
      })
      const gqlSorted = gqlResponse.data.Product
        .filter(p => p.name.startsWith('Item'))
        .map(p => p.name)

      expect(docSorted).to.deep.equal(['Item1', 'Item2', 'Item3'])
      expect(woqlSorted).to.deep.equal(['Item1', 'Item2', 'Item3'])
      expect(gqlSorted).to.deep.equal(['Item1', 'Item2', 'Item3'])
    })
  })
})
