const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')
const Decimal = require('decimal.js')
const {
  extractWOQLValue,
} = require('../lib/precision-helpers')

/**
 * Precision Helpers Demonstration
 *
 * Shows how to use precision helpers to extract numeric values
 * with full 20-digit precision from responses.
 *
 * Based on established patterns from decimal-precision.js
 */

describe('precision-helpers-demo', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('extractWOQLValue() - Core Functionality', function () {
    it('extracts WOQL result with full precision', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '.1' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '.2' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const response = await woql.post(agent, query).unverified()

      // Extract with precision preserved
      const result = extractWOQLValue(response)

      expect(result).to.be.instanceOf(Decimal)
      expect(result.equals(new Decimal('0.3'))).to.be.true
      expect(result.toString()).to.equal('0.3')
    })

    it('handles division with rational precision', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '1' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '3' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const response = await woql.post(agent, query).unverified()
      const result = extractWOQLValue(response)

      expect(result).to.be.instanceOf(Decimal)

      // Should be high-precision 1/3
      expect(result.toNumber()).to.be.closeTo(0.3333333333333333, 0.0000000000000001)
    })

    it('handles custom variable names', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '2.5' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '4' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'MyResult' },
      }

      const response = await woql.post(agent, query).unverified()
      const result = extractWOQLValue(response, 'MyResult')

      expect(result).to.be.instanceOf(Decimal)
      expect(result.equals(new Decimal('10'))).to.be.true
    })
  })

  describe('Real-world Usage Pattern - WOQL Arithmetic', function () {
    it('Pattern from decimal-precision.js: WOQL arithmetic with precision', async function () {
      // This demonstrates the pattern from decimal-precision.js lines 148-159
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: {
            '@type': 'Divide',
            left: { '@type': 'ArithmeticValue', data: 1 },
            right: { '@type': 'ArithmeticValue', data: 3 },
          },
          right: {
            '@type': 'Divide',
            left: { '@type': 'ArithmeticValue', data: 1 },
            right: { '@type': 'ArithmeticValue', data: 6 },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)
      const binding = result.bindings[0].Result

      expect(binding['@type']).to.equal('xsd:decimal')

      // Extract exact value
      const exactValue = new Decimal(binding['@value'].toString())
      const expected = new Decimal('0.5')

      // Verify exact equality: 1/3 + 1/6 = 1/2 = 0.5
      expect(exactValue.equals(expected)).to.be.true
    })
  })
})
