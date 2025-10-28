const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

/**
 * XSD Double Canonicalization Tests
 *
 * Tests for the original bug: xsd:double values like 33 and 33.0
 * should be equal but were showing as not_equal.
 *
 * Issue: https://github.com/terminusdb/terminusdb/issues/...
 *
 * xsd:double uses IEEE 754 double-precision floating point.
 * Canonical forms should work the same as xsd:decimal.
 */

describe('xsd-double-canonicalization', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('XSD Double - Canonical Form Equivalence', function () {
    it('33 = 33.0 = 33.00 (xsd:double)', async function () {
      // Test the exact scenario from the bug report
      const values = [
        { '@type': 'xsd:double', '@value': '33' },
        { '@type': 'xsd:double', '@value': '33.0' },
        { '@type': 'xsd:double', '@value': '33.00' },
      ]

      for (let i = 0; i < values.length; i++) {
        for (let j = 0; j < values.length; j++) {
          const query = {
            '@type': 'Equals',
            left: { '@type': 'DataValue', data: values[i] },
            right: { '@type': 'DataValue', data: values[j] },
          }

          const r = await woql.post(agent, query)

          expect(r.body.bindings).to.have.lengthOf(1,
            `xsd:double ${values[i]['@value']} should equal ${values[j]['@value']}`)
        }
      }
    })

    it('literal(33, xsd:double) = literal(33.0, xsd:double)', async function () {
      // Exact replication of bug scenario
      const query = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33.0' } },
      }

      const r = await woql.post(agent, query)

      expect(r.body.bindings).to.have.lengthOf(1,
        'literal(33, xsd:double) should equal literal(33.0, xsd:double)')
    })

    it('2.5 = 2.50 = 2.500 (xsd:double with decimals)', async function () {
      const values = [
        { '@type': 'xsd:double', '@value': '2.5' },
        { '@type': 'xsd:double', '@value': '2.50' },
        { '@type': 'xsd:double', '@value': '2.500' },
      ]

      for (let i = 0; i < values.length; i++) {
        for (let j = 0; j < values.length; j++) {
          const query = {
            '@type': 'Equals',
            left: { '@type': 'DataValue', data: values[i] },
            right: { '@type': 'DataValue', data: values[j] },
          }

          const r = await woql.post(agent, query)

          expect(r.body.bindings).to.have.lengthOf(1,
            `xsd:double ${values[i]['@value']} should equal ${values[j]['@value']}`)
        }
      }
    })

    it('0 = 0.0 = 0.00 (xsd:double zero)', async function () {
      const values = [
        { '@type': 'xsd:double', '@value': '0' },
        { '@type': 'xsd:double', '@value': '0.0' },
        { '@type': 'xsd:double', '@value': '0.00' },
      ]

      for (let i = 0; i < values.length; i++) {
        for (let j = 0; j < values.length; j++) {
          const query = {
            '@type': 'Equals',
            left: { '@type': 'DataValue', data: values[i] },
            right: { '@type': 'DataValue', data: values[j] },
          }

          const r = await woql.post(agent, query)

          expect(r.body.bindings).to.have.lengthOf(1,
            `xsd:double ${values[i]['@value']} should equal ${values[j]['@value']}`)
        }
      }
    })
  })

  describe('XSD Double - Comparisons', function () {
    it('33.0 < 34.1 (xsd:double)', async function () {
      // From original bug report
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33.0' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '34.1' } },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('33.0 > 31.1 (xsd:double)', async function () {
      // From original bug report
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33.0' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '31.1' } },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('33 < 33.1 (canonical form comparison)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33.1' } },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('XSD Double - Arithmetic Behavior', function () {
    it('0.1 + 0.2 with xsd:double uses IEEE 754 arithmetic', async function () {
      // IMPORTANT: xsd:double should use IEEE 754 floating point arithmetic
      // This means it WILL have the classic floating point "error"
      // Result: 0.30000000000000004 is CORRECT for xsd:double
      // (Only xsd:decimal uses rational arithmetic for exact results)
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      const resultValue = r.body.bindings[0].Result['@value']
      const resultType = r.body.bindings[0].Result['@type']

      console.log('0.1 + 0.2 (xsd:double) result value:', resultValue)
      console.log('0.1 + 0.2 (xsd:double) result type:', resultType)

      // xsd:double uses IEEE 754, so floating point "error" is expected and correct
      expect(resultValue).to.be.closeTo(0.30000000000000004, 0.0000000000000001)

      // Type: Pure xsd:double arithmetic should return xsd:double
      expect(resultType).to.equal('xsd:double', 'Pure float/double ops return xsd:double')
    })

    it('0.1 * 10 with xsd:double uses IEEE 754', async function () {
      // Pure xsd:double arithmetic uses IEEE 754, returns xsd:double
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '10' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.be.closeTo(1.0, 0.0000000001)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('1.0 - 0.9 with xsd:double uses IEEE 754', async function () {
      // Pure xsd:double arithmetic uses IEEE 754, returns xsd:double
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Minus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.9' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      // May have slight IEEE 754 imprecision
      expect(r.body.bindings[0].Result['@value']).to.be.closeTo(0.1, 0.0000000001)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('33.0 + 1.0 = 34.0 (pure xsd:double returns xsd:double)', async function () {
      // Pure xsd:double arithmetic returns xsd:double
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '33.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.equal(34)
      // Pure xsd:double arithmetic returns xsd:double
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('1.0 / 3.0 with xsd:double uses IEEE 754 division', async function () {
      // Pure xsd:double division uses IEEE 754 (not rational division)
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '3.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      // IEEE 754 division result
      expect(r.body.bindings[0].Result['@value']).to.be.closeTo(0.3333333333333333, 0.0000000000000001)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('Result of 33.0 + 1.0 should equal 34 (canonical)', async function () {
      // Test that arithmetic result is canonicalized
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '33.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const result1 = r1.body.bindings[0].Result['@value']

      // Compare with literal 34
      const query2 = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': result1.toString() } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '34' } },
      }

      const r2 = await woql.post(agent, query2)
      expect(r2.body.bindings).to.have.lengthOf(1,
        'Result of 33.0 + 1.0 should equal literal 34')
    })
  })

  describe('XSD Double vs XSD Decimal', function () {
    it('xsd:double 33.0 should NOT equal xsd:decimal 33.0 (cross-family comparison forbidden)', async function () {
      const query = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33.0' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:decimal', '@value': '33.0' } },
      }

      // Cross-family comparison (IEEE 754 vs Rational) should throw error
      const r = await woql.post(agent, query).fails()
      expect(r.status).to.equal(400)
      expect(r.body['api:message']).to.match(/incompatible/i)
    })

    it('Arithmetic Edge Case: xsd:double + xsd:decimal inherits IEEE 754 precision', async function () {
      // EDGE CASE DOCUMENTED: Mixing xsd:double with xsd:decimal
      //
      // IMPORTANT: Precision is already lost when xsd:double values are parsed!
      // xsd:double('0.1') → IEEE 754 float (0.1000000000000000055...)
      // Even though result type is xsd:decimal, the input precision is gone

      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.2' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // ACTUAL BEHAVIOR: Still get IEEE 754 error because xsd:double(0.1)
      // was already converted to imprecise float before arithmetic
      expect(r.body.bindings[0].Result['@value']).to.equal(0.30000000000000004)

      // Type IS xsd:double (Prolog: floats are contagious), and precision already lost
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('Edge Case: Pure xsd:decimal avoids IEEE 754 precision issues', async function () {
      // CONTRAST: Pure xsd:decimal arithmetic gives exact results
      // This shows the difference - use xsd:decimal throughout for precision

      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.1' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.2' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // Pure xsd:decimal: EXACT result (no IEEE 754 error)
      expect(r.body.bindings[0].Result['@value']).to.equal(0.3)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:decimal')
    })
  })

  describe('XSD Float - Canonical Form Equivalence', function () {
    it('33 = 33.0 = 33.00 (xsd:float)', async function () {
      // Test the same canonicalization for xsd:float
      const values = [
        { '@type': 'xsd:float', '@value': '33' },
        { '@type': 'xsd:float', '@value': '33.0' },
        { '@type': 'xsd:float', '@value': '33.00' },
      ]

      for (let i = 0; i < values.length; i++) {
        for (let j = 0; j < values.length; j++) {
          const query = {
            '@type': 'Equals',
            left: { '@type': 'DataValue', data: values[i] },
            right: { '@type': 'DataValue', data: values[j] },
          }

          const r = await woql.post(agent, query)

          expect(r.body.bindings).to.have.lengthOf(1,
            `xsd:float ${values[i]['@value']} should equal ${values[j]['@value']}`)
        }
      }
    })

    it('literal(33, xsd:float) = literal(33.0, xsd:float)', async function () {
      const query = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '33' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '33.0' } },
      }

      const r = await woql.post(agent, query)

      expect(r.body.bindings).to.have.lengthOf(1,
        'literal(33, xsd:float) should equal literal(33.0, xsd:float)')
    })

    it('1.5 = 1.50 = 1.500 (xsd:float with decimals)', async function () {
      const values = [
        { '@type': 'xsd:float', '@value': '1.5' },
        { '@type': 'xsd:float', '@value': '1.50' },
        { '@type': 'xsd:float', '@value': '1.500' },
      ]

      for (let i = 0; i < values.length; i++) {
        for (let j = 0; j < values.length; j++) {
          const query = {
            '@type': 'Equals',
            left: { '@type': 'DataValue', data: values[i] },
            right: { '@type': 'DataValue', data: values[j] },
          }

          const r = await woql.post(agent, query)

          expect(r.body.bindings).to.have.lengthOf(1,
            `xsd:float ${values[i]['@value']} should equal ${values[j]['@value']}`)
        }
      }
    })

    it('0 = 0.0 = 0.00 (xsd:float zero)', async function () {
      const values = [
        { '@type': 'xsd:float', '@value': '0' },
        { '@type': 'xsd:float', '@value': '0.0' },
        { '@type': 'xsd:float', '@value': '0.00' },
      ]

      for (let i = 0; i < values.length; i++) {
        for (let j = 0; j < values.length; j++) {
          const query = {
            '@type': 'Equals',
            left: { '@type': 'DataValue', data: values[i] },
            right: { '@type': 'DataValue', data: values[j] },
          }

          const r = await woql.post(agent, query)

          expect(r.body.bindings).to.have.lengthOf(1,
            `xsd:float ${values[i]['@value']} should equal ${values[j]['@value']}`)
        }
      }
    })
  })

  describe('XSD Float - Comparisons', function () {
    it('33.0 < 34.1 (xsd:float)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '33.0' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '34.1' } },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('33.0 > 31.1 (xsd:float)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '33.0' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '31.1' } },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })

    it('33 < 33.1 (xsd:float canonical form comparison)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '33' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:float', '@value': '33.1' } },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('XSD Float - Arithmetic Behavior', function () {
    it('0.1 + 0.2 with xsd:float, uses xsd:double arithmetic internally)', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '0.1' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '0.2' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // Pure xsd:float uses IEEE 754, so we get the floating point "error"
      // This is the EXACT expected value from IEEE 754 arithmetic
      expect(r.body.bindings[0].Result['@value']).to.equal(0.30000000000000004)

      // Type: Pure xsd:float arithmetic returns xsd:double
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('0.1 * 10 = 1.0 (xsd:float, no floating point error)', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '0.1' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '10' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.equal(1)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('33.0 + 1.0 = 34.0 (xsd:float arithmetic returns xsd:double)', async function () {
      // Same as xsd:double - arithmetic converts to xsd:decimal for precision
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '33.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '1.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.equal(34)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })

    it('1.0 / 3.0 with xsd:float returns xsd:double', async function () {
      // Pure xsd:float division returns xsd:double (promoted for operation)
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '3.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.be.closeTo(0.3333333333333333, 0.0000000000000001)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    })
  })

  describe('Type Inference Rules - Mixed vs Pure Float/Double', function () {
    it('Pure xsd:double arithmetic returns xsd:double', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '2.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double',
        'Rule: xsd:double + xsd:double → xsd:double')
    })

    it('Pure xsd:float arithmetic returns xsd:double', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '2.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double',
        'Rule: xsd:float + xsd:float → xsd:double')
    })

    it('Mixed xsd:float and xsd:double arithmetic returns xsd:double', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:float', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '2.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double',
        'Rule: xsd:float + xsd:double → xsd:double (both are float types)')
    })

    it('Mixed xsd:double and xsd:decimal returns xsd:double', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '2.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double',
        'Rule: xsd:double + xsd:decimal → xsd:double (Prolog: floats are contagious)')
    })

    it('Mixed xsd:double and xsd:integer returns xsd:double', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.5' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:integer', '@value': '3' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double',
        'Rule: xsd:double + xsd:integer → xsd:double (Prolog: floats are contagious)')
    })
  })

  describe('Bug Reproduction - Original Scenario', function () {
    it('Reproduce exact bug: 33 and 33.0 equality check', async function () {
      // This reproduces the exact scenario from the bug report where
      // one branch shows is_equal and another shows not_equal

      // Check 1: Does 33 equal stored value 33?
      const query1 = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33' } },
      }

      const r1 = await woql.post(agent, query1)
      const result1 = r1.body.bindings.length > 0 ? 'is_equal' : 'not_equal'

      // Check 2: Does 33.0 equal stored value 33?
      const query2 = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33.0' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '33' } },
      }

      const r2 = await woql.post(agent, query2)
      const result2 = r2.body.bindings.length > 0 ? 'is_equal' : 'not_equal'

      console.log('literal(33, xsd:double) == 33:', result1)
      console.log('literal(33.0, xsd:double) == 33:', result2)

      // Both should be is_equal
      expect(result1).to.equal('is_equal', 'literal(33) should equal 33')
      expect(result2).to.equal('is_equal', 'literal(33.0) should equal 33 (BUG if this fails!)')
      expect(result1).to.equal(result2, 'Both checks should have the same result')
    })
  })
})
