const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

/**
 * WOQL Arithmetic Edge Cases and Corner Cases
 *
 * Focus on weak areas:
 * - Complex nested arithmetic expressions
 * - Mixed canonical forms in single expression
 * - Negative numbers with canonical forms
 * - Zero handling across forms
 * - Very small and very large decimals
 * - Rational division precision
 */

describe('numeric-arithmetic-edge-cases', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Nested Arithmetic Expressions', function () {
    it('(2 + 3) * 4 === (2.0 + 3.0) * 4.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: 2 },
            right: { '@type': 'ArithmeticValue', data: 3 },
          },
          right: { '@type': 'ArithmeticValue', data: 4 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '2.0' } },
            right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '3.0' } },
          },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '4.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(20)
      expect(r2.body.bindings[0].Result['@value']).to.equal(20)
    })

    it('(10 - 3) / (2 + 1) with canonical variations', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: {
            '@type': 'Minus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '10.00' } },
            right: { '@type': 'ArithmeticValue', data: 3 },
          },
          right: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '2.0' } },
            right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '1.00' } },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // (10 - 3) / (2 + 1) = 7 / 3 = 2.33333...
      expect(r.body.bindings[0].Result['@value']).to.be.closeTo(2.333333, 0.00001)
    })

    it('2 ** (3 + 1) === 2.0 ** (3.0 + 1.0)', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Exp',
          left: { '@type': 'ArithmeticValue', data: 2 },
          right: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: 3 },
            right: { '@type': 'ArithmeticValue', data: 1 },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Exp',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '2.0' } },
          right: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '3.0' } },
            right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '1.0' } },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(16)
      expect(r2.body.bindings[0].Result['@value']).to.equal(16)
    })
  })

  describe('Negative Numbers - Canonical Forms', function () {
    it('Plus: -2 + -3 === -2.0 + -3.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: -2 },
          right: { '@type': 'ArithmeticValue', data: -3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-2.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-3.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(-5)
      expect(r2.body.bindings[0].Result['@value']).to.equal(-5)
    })

    it('Minus: -10 - -3 === -10.0 - -3.0 (equals -7)', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Minus',
          left: { '@type': 'ArithmeticValue', data: -10 },
          right: { '@type': 'ArithmeticValue', data: -3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Minus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-10.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-3.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(-7)
      expect(r2.body.bindings[0].Result['@value']).to.equal(-7)
    })

    it('Times: -2 * 3 === -2.0 * 3.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: -2 },
          right: { '@type': 'ArithmeticValue', data: 3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-2.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '3.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(-6)
      expect(r2.body.bindings[0].Result['@value']).to.equal(-6)
    })

    it('Divide: -10 / 2 === -10.0 / 2.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: -10 },
          right: { '@type': 'ArithmeticValue', data: 2 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-10.0' } },
          right: { '@type': 'ArithmeticValue', data: 2.0 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(-5)
      expect(r2.body.bindings[0].Result['@value']).to.equal(-5)
    })

    it('Negative with leading zero: -.5 * 2 === -0.5 * 2', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-.5' } },
          right: { '@type': 'ArithmeticValue', data: 2 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '-0.5' } },
          right: { '@type': 'ArithmeticValue', data: 2 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(-1)
      expect(r2.body.bindings[0].Result['@value']).to.equal(-1)
    })
  })

  describe('Zero Handling', function () {
    it('Plus: 0 + 5 === 0.0 + 5.0 === 0.00 + 5.00', async function () {
      const variations = [
        { left: 0, right: 5 },
        { left: 0.0, right: 5.0 },
        { left: { '@type': 'xsd:decimal', '@value': '0.00' }, right: { '@type': 'xsd:decimal', '@value': '5.00' } },
      ]

      for (const v of variations) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: v.left },
            right: { '@type': 'ArithmeticValue', data: v.right },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings[0].Result['@value']).to.equal(5)
      }
    })

    it('Minus: 5 - 0 === 5.0 - 0.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Minus',
          left: { '@type': 'ArithmeticValue', data: 5 },
          right: { '@type': 'ArithmeticValue', data: 0 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Minus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '5.0' } },
          right: { '@type': 'ArithmeticValue', data: 0.0 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(5)
      expect(r2.body.bindings[0].Result['@value']).to.equal(5)
    })

    it('Times: 0 * 5 === 0.0 * 5.0 (always zero)', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: 0 },
          right: { '@type': 'ArithmeticValue', data: 5 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '5.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(0)
      expect(r2.body.bindings[0].Result['@value']).to.equal(0)
    })

    it('Zero with leading decimal: .0 === 0.0 === 0.00 in arithmetic', async function () {
      const variations = ['.0', '0.0', '0.00']

      for (const zero of variations) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': zero } },
            right: { '@type': 'ArithmeticValue', data: 1 },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings[0].Result['@value']).to.equal(1)
      }
    })
  })

  describe('Rational Division Precision', function () {
    it('1 / 3 produces high-precision rational', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: 1 },
          right: { '@type': 'ArithmeticValue', data: 3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // Should be exact rational 1/3 = 0.333333... (with many digits)
      expect(r.body.bindings[0].Result['@value']).to.be.closeTo(0.3333333333, 0.0000000001)
    })

    it('(1 / 3) * 3 === 1 (exact rational arithmetic)', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: {
            '@type': 'Divide',
            left: { '@type': 'ArithmeticValue', data: 1 },
            right: { '@type': 'ArithmeticValue', data: 3 },
          },
          right: { '@type': 'ArithmeticValue', data: 3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // Should be exactly 1 (rational arithmetic is exact)
      expect(r.body.bindings[0].Result['@value']).to.equal(1)
    })

    it('1 / 7 produces repeating decimal with high precision', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: 1 },
          right: { '@type': 'ArithmeticValue', data: 7 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // 1/7 = 0.142857142857...
      expect(r.body.bindings[0].Result['@value']).to.be.closeTo(0.14285714285714, 0.00000000000001)
    })

    it('10 / 3 with canonical variations', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: 10 },
          right: { '@type': 'ArithmeticValue', data: 3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '10.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '3.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      // Both should be exactly equal (same rational)
      expect(r1.body.bindings[0].Result['@value']).to.equal(r2.body.bindings[0].Result['@value'])
      expect(r1.body.bindings[0].Result['@value']).to.be.closeTo(3.333333, 0.000001)
    })
  })

  describe('Very Small Decimals', function () {
    it('.001 + .002 === 0.001 + 0.002 === 0.003', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '.001' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '.002' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.001' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.002' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(0.003)
      expect(r2.body.bindings[0].Result['@value']).to.equal(0.003)
    })

    it('.0001 * 10000 === 0.0001 * 10000 === 1', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '.0001' } },
          right: { '@type': 'ArithmeticValue', data: 10000 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.0001' } },
          right: { '@type': 'ArithmeticValue', data: 10000 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(1)
      expect(r2.body.bindings[0].Result['@value']).to.equal(1)
    })
  })

  describe('Very Large Numbers', function () {
    it('1000000 + 1 === 1000000.0 + 1.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: 1000000 },
          right: { '@type': 'ArithmeticValue', data: 1 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '1000000.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '1.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(1000001)
      expect(r2.body.bindings[0].Result['@value']).to.equal(1000001)
    })

    it('999999 * 2 === 999999.0 * 2.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: 999999 },
          right: { '@type': 'ArithmeticValue', data: 2 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '999999.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '2.0' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(1999998)
      expect(r2.body.bindings[0].Result['@value']).to.equal(1999998)
    })
  })

  describe('Classic Floating Point Problems - SOLVED', function () {
    it('0.1 + 0.2 === 0.3 (NOT 0.30000000000000004)', async function () {
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

      // Should be exactly 0.3 (rational arithmetic)
      expect(r.body.bindings[0].Result['@value']).to.equal(0.3)
    })

    it('0.1 * 10 === 1.0 (NOT 0.9999999999999999)', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.1' } },
          right: { '@type': 'ArithmeticValue', data: 10 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // Should be exactly 1
      expect(r.body.bindings[0].Result['@value']).to.equal(1)
    })

    it('1.0 - 0.9 === 0.1 (NOT 0.09999999999999998)', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Minus',
          left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '1.0' } },
          right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '0.9' } },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r = await woql.post(agent, query)

      // Should be exactly 0.1
      expect(r.body.bindings[0].Result['@value']).to.equal(0.1)
    })
  })
})
