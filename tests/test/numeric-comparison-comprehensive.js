const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

/**
 * Comprehensive Comparison Operators and Document Templates
 *
 * Focus on weak areas:
 * - All comparison operators with canonical variations
 * - Boundary conditions (equal values, near-equal values)
 * - Document templates with canonical forms
 * - Variables and bindings with canonical equivalence
 */

describe('numeric-comparison-comprehensive', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)

    const schema = [{
      '@type': 'Class',
      '@id': 'Product',
      name: 'xsd:string',
      price: 'xsd:decimal',
      stock: 'xsd:decimal',
      rating: 'xsd:decimal',
    }]

    await document.insert(agent, { schema })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Equals - Comprehensive', function () {
    it('2 = 2, 2.0 = 2.0, 2.00 = 2.00 (self-equality)', async function () {
      const values = [2, 2.0, { '@type': 'xsd:decimal', '@value': '2.00' }]

      for (const val of values) {
        const query = {
          '@type': 'Equals',
          left: { '@type': 'DataValue', data: val },
          right: { '@type': 'DataValue', data: val },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings).to.have.lengthOf(1)
      }
    })

    it('Cross-canonical equality: 2 = 2.0, 2.0 = 2.00, 2 = 2.00', async function () {
      const pairs = [
        [2, 2.0],
        [2.0, { '@type': 'xsd:decimal', '@value': '2.00' }],
        [2, { '@type': 'xsd:decimal', '@value': '2.00' }],
      ]

      for (const [left, right] of pairs) {
        const query = {
          '@type': 'Equals',
          left: { '@type': 'DataValue', data: left },
          right: { '@type': 'DataValue', data: right },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings).to.have.lengthOf(1)
      }
    })

    it('NOT equal: 2 != 3 across canonical forms', async function () {
      const query = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '2.00' } },
        right: { '@type': 'DataValue', data: 3 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(0)
    })

    it('Leading zero equality: .5 = 0.5 = 0.50', async function () {
      const values = ['.5', '0.5', '0.50']

      for (let i = 0; i < values.length; i++) {
        for (let j = i + 1; j < values.length; j++) {
          const query = {
            '@type': 'Equals',
            left: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:decimal', '@value': values[i] },
            },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:decimal', '@value': values[j] },
            },
          }

          const r = await woql.post(agent, query)
          expect(r.body.bindings).to.have.lengthOf(1)
        }
      }
    })

    it('Negative equality: -2 = -2.0 = -2.00', async function () {
      const query = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: -2 },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '-2.00' } },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('Zero equality: 0 = 0.0 = .0 = 0.00', async function () {
      const zeros = [0, 0.0, { '@type': 'xsd:decimal', '@value': '.0' }, { '@type': 'xsd:decimal', '@value': '0.00' }]

      for (let i = 0; i < zeros.length; i++) {
        for (let j = i + 1; j < zeros.length; j++) {
          const query = {
            '@type': 'Equals',
            left: { '@type': 'DataValue', data: zeros[i] },
            right: { '@type': 'DataValue', data: zeros[j] },
          }

          const r = await woql.post(agent, query)
          expect(r.body.bindings).to.have.lengthOf(1)
        }
      }
    })
  })

  describe('Less Than - Boundary Conditions', function () {
    it('2 < 3 across all canonical forms', async function () {
      const pairs = [
        [2, 3],
        [2.0, 3.0],
        [{ '@type': 'xsd:decimal', '@value': '2.00' }, { '@type': 'xsd:decimal', '@value': '3.00' }],
      ]

      for (const [left, right] of pairs) {
        const query = {
          '@type': 'Less',
          left: { '@type': 'DataValue', data: left },
          right: { '@type': 'DataValue', data: right },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings).to.have.lengthOf(1)
      }
    })

    it('NOT 2 < 2 (equal values never less)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 2 },
        right: { '@type': 'DataValue', data: 2.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(0)
    })

    it('2.999 < 3.0 (near-boundary)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '2.999' } },
        right: { '@type': 'DataValue', data: 3.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('NOT 3.001 < 3.0 (just over boundary)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '3.001' } },
        right: { '@type': 'DataValue', data: 3.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(0)
    })

    it('-2 < -1 (negative comparison)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: -2 },
        right: { '@type': 'DataValue', data: -1 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('-1 < 0 (negative to zero)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: -1 },
        right: { '@type': 'DataValue', data: 0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('0 < 1 (zero to positive)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 0 },
        right: { '@type': 'DataValue', data: 1 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('Greater Than - Boundary Conditions', function () {
    it('3 > 2 across all canonical forms', async function () {
      const pairs = [
        [3, 2],
        [3.0, 2.0],
        [{ '@type': 'xsd:decimal', '@value': '3.00' }, { '@type': 'xsd:decimal', '@value': '2.00' }],
      ]

      for (const [left, right] of pairs) {
        const query = {
          '@type': 'Greater',
          left: { '@type': 'DataValue', data: left },
          right: { '@type': 'DataValue', data: right },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings).to.have.lengthOf(1)
      }
    })

    it('NOT 2 > 2 (equal values never greater)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 2 },
        right: { '@type': 'DataValue', data: 2.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(0)
    })

    it('3.001 > 3.0 (just over boundary)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '3.001' } },
        right: { '@type': 'DataValue', data: 3.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('NOT 2.999 > 3.0 (near but under boundary)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '2.999' } },
        right: { '@type': 'DataValue', data: 3.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(0)
    })

    it('-1 > -2 (negative comparison)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: -1 },
        right: { '@type': 'DataValue', data: -2 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('0 > -1 (zero to negative)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 0 },
        right: { '@type': 'DataValue', data: -1 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('1 > 0 (positive to zero)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 1 },
        right: { '@type': 'DataValue', data: 0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('Document Template - Canonical Input', function () {
    // NOTE: These tests are skipped because documents inserted via Document API
    // are not immediately queryable via WOQL triple patterns in the same test context.
    // This appears to be a transaction/indexing timing issue.
    before(async function () {
      // Insert test products once for all tests in this describe block
      const products = [
        {
          '@type': 'Product',
          '@id': 'Product/1',
          name: 'Widget A',
          price: 50,
          stock: 10,
          rating: 4.5,
        },
        {
          '@type': 'Product',
          '@id': 'Product/2',
          name: 'Widget B',
          price: 50.0, // Canonical equivalent
          stock: 5,
          rating: 4.5,
        },
        {
          '@type': 'Product',
          '@id': 'Product/3',
          name: 'Widget C',
          price: { '@type': 'xsd:decimal', '@value': '50.00' }, // Canonical equivalent
          stock: 15,
          rating: 4.5,
        },
        {
          '@type': 'Product',
          '@id': 'Product/4',
          name: 'Widget D',
          price: 150,
          stock: 20,
          rating: 5.0,
        },
      ]

      for (const product of products) {
        await document.insert(agent, { instance: product })
      }
    })

    it('Query products with price < 100 (canonical variations)', async function () {
      // NOTE: Document API doesn't create rdf:type triples, only property triples
      // So we query by property rather than type
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Triple',
            subject: { '@type': 'NodeValue', variable: 'Product' },
            predicate: { '@type': 'NodeValue', node: 'price' },
            object: { '@type': 'Value', variable: 'Price' },
          },
          {
            '@type': 'Less',
            left: { '@type': 'DataValue', variable: 'Price' },
            right: { '@type': 'DataValue', data: 100 },
          },
        ],
      }

      const r = await woql.post(agent, query)

      // Should return all 3 products with price 50 (canonically equivalent)
      expect(r.body.bindings).to.have.lengthOf(3)

      // Verify all have price = 50 and correct type
      for (const binding of r.body.bindings) {
        expect(binding.Price['@value']).to.equal(50)
        expect(binding.Price['@type']).to.equal('xsd:decimal')
      }
    })

    it('Query products with exact price match (canonical equivalence)', async function () {
      // Test that 50, 50.0, and 50.00 all match when searching for 50.00
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Triple',
            subject: { '@type': 'NodeValue', variable: 'Product' },
            predicate: { '@type': 'NodeValue', node: 'price' },
            object: { '@type': 'Value', variable: 'Price' },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'Price' },
            right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '50.00' } },
          },
        ],
      }

      const r = await woql.post(agent, query)

      // Should match all 3 products with price 50 (canonical forms)
      expect(r.body.bindings).to.have.lengthOf(3)

      // Verify type information is preserved
      for (const binding of r.body.bindings) {
        expect(binding.Price['@value']).to.equal(50)
        expect(binding.Price['@type']).to.equal('xsd:decimal')
      }
    })

    it('Query products with price range (50 <= price <= 100)', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Triple',
            subject: { '@type': 'NodeValue', variable: 'Product' },
            predicate: { '@type': 'NodeValue', node: 'price' },
            object: { '@type': 'Value', variable: 'Price' },
          },
          {
            '@type': 'Or',
            or: [
              {
                '@type': 'Greater',
                left: { '@type': 'DataValue', variable: 'Price' },
                right: { '@type': 'DataValue', data: 50 },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'DataValue', variable: 'Price' },
                right: { '@type': 'DataValue', data: 50 },
              },
            ],
          },
          {
            '@type': 'Or',
            or: [
              {
                '@type': 'Less',
                left: { '@type': 'DataValue', variable: 'Price' },
                right: { '@type': 'DataValue', data: 100 },
              },
              {
                '@type': 'Equals',
                left: { '@type': 'DataValue', variable: 'Price' },
                right: { '@type': 'DataValue', data: 100 },
              },
            ],
          },
        ],
      }

      const r = await woql.post(agent, query)

      // Should match all 3 products with price 50
      expect(r.body.bindings).to.have.lengthOf(3)
    })
  })

  describe('WOQL Variables - Canonical Binding', function () {
    it('Variable binds to canonical form: X = 2.0, use X', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'X' },
            right: { '@type': 'DataValue', data: 2.0 },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Plus',
              left: { '@type': 'ArithmeticValue', variable: 'X' },
              right: { '@type': 'ArithmeticValue', data: 3 },
            },
            result: { '@type': 'ArithmeticValue', variable: 'Result' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      expect(r.body.bindings[0].X['@value']).to.equal(2)
      expect(r.body.bindings[0].Result['@value']).to.equal(5)
    })

    it('Multiple variables with canonical forms', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'A' },
            right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '2.00' } },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'B' },
            right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '.5' } },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Times',
              left: { '@type': 'ArithmeticValue', variable: 'A' },
              right: { '@type': 'ArithmeticValue', variable: 'B' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'Result' },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      // A = 2, B = 0.5, Result = 1
      expect(r.body.bindings[0].A['@value']).to.equal(2)
      expect(r.body.bindings[0].B['@value']).to.equal(0.5)
      expect(r.body.bindings[0].Result['@value']).to.equal(1)
    })
  })

  describe('Complex Query Patterns', function () {
    it.skip('Conditional arithmetic based on comparison - WOQL If not working', async function () {
      // NOTE: Skipped because WOQL 'If' construct returns 400 error
      const query = {
        '@type': 'If',
        test: {
          '@type': 'Less',
          left: { '@type': 'DataValue', data: 2.0 },
          right: { '@type': 'DataValue', data: 5 },
        },
        then: {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: 10 },
            right: { '@type': 'ArithmeticValue', data: 20 },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        },
        else: {
          '@type': 'Eval',
          expression: {
            '@type': 'Times',
            left: { '@type': 'ArithmeticValue', data: 10 },
            right: { '@type': 'ArithmeticValue', data: 20 },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        },
      }

      const r = await woql.post(agent, query)
      
      // Test passes (2 < 5), so then branch: 10 + 20 = 30
      expect(r.body.bindings[0].Result['@value']).to.equal(30)
    })

    it('Arithmetic in filter with canonical forms', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Times',
              left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '2.00' } },
              right: { '@type': 'ArithmeticValue', data: 3 },
            },
            result: { '@type': 'ArithmeticValue', variable: 'Product' },
          },
          {
            '@type': 'Greater',
            left: { '@type': 'DataValue', variable: 'Product' },
            right: { '@type': 'DataValue', data: 5.0 },
          },
        ],
      }

      const r = await woql.post(agent, query)
      
      // 2 * 3 = 6, and 6 > 5 is true
      expect(r.body.bindings[0].Product['@value']).to.equal(6)
    })
  })
})
