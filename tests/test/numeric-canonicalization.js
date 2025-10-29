const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

/**
 * Numeric Canonicalization Integration Tests
 *
 * Tests canonical form handling across all interfaces:
 * - WOQL Arithmetic operators (WEAK AREA)
 * - WOQL Comparison operators (WEAK AREA)
 * - WOQL Typecast and Literal handling
 * - Document Templates
 * - Document API input
 *
 * Focus: Ensure inputs like 2, 2.0, 2.00, .1, 0.1 are canonically equivalent
 */

describe('numeric-canonicalization', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)

    const schema = [{
      '@type': 'Class',
      '@id': 'NumericTest',
      decimalField: 'xsd:decimal',
      integerField: 'xsd:integer',
    }]

    await document.insert(agent, { schema })
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('WOQL Arithmetic - Canonical Equivalence', function () {
    it('Plus: 2 + 3 === 2.0 + 3.0 === 2.00 + 3.00', async function () {
      const variations = [
        { left: 2, right: 3 },
        { left: 2.0, right: 3.0 },
        { left: { '@type': 'xsd:decimal', '@value': '2.00' }, right: { '@type': 'xsd:decimal', '@value': '3.00' } },
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

    it('Minus: 10 - 3 with canonical variations', async function () {
      const variations = [
        { left: 10, right: 3 },
        { left: 10.0, right: 3.0 },
        { left: { '@type': 'xsd:decimal', '@value': '10.00' }, right: { '@type': 'xsd:decimal', '@value': '3.00' } },
      ]

      for (const v of variations) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Minus',
            left: { '@type': 'ArithmeticValue', data: v.left },
            right: { '@type': 'ArithmeticValue', data: v.right },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings[0].Result['@value']).to.equal(7)
      }
    })

    it('Times: 4 * 5 with canonical variations', async function () {
      const variations = [
        { left: 4, right: 5 },
        { left: 4.0, right: 5.0 },
        { left: { '@type': 'xsd:decimal', '@value': '4.00' }, right: { '@type': 'xsd:decimal', '@value': '5.00' } },
      ]

      for (const v of variations) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Times',
            left: { '@type': 'ArithmeticValue', data: v.left },
            right: { '@type': 'ArithmeticValue', data: v.right },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings[0].Result['@value']).to.equal(20)
      }
    })

    it('Divide: 10 / 2 with canonical variations', async function () {
      const variations = [
        { left: 10, right: 2 },
        { left: 10.0, right: 2.0 },
        { left: { '@type': 'xsd:decimal', '@value': '10.00' }, right: { '@type': 'xsd:decimal', '@value': '2.00' } },
      ]

      for (const v of variations) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Divide',
            left: { '@type': 'ArithmeticValue', data: v.left },
            right: { '@type': 'ArithmeticValue', data: v.right },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings[0].Result['@value']).to.equal(5)
      }
    })

    it('Div: integer division with canonical variations', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Div',
          left: { '@type': 'ArithmeticValue', data: 10 },
          right: { '@type': 'ArithmeticValue', data: 3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Div',
          left: { '@type': 'ArithmeticValue', data: 10.0 },
          right: { '@type': 'ArithmeticValue', data: 3.0 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(3)
      expect(r2.body.bindings[0].Result['@value']).to.equal(3)
    })

    it('Exp (Power): 2 ** 3 === 2.0 ** 3.0', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Exp',
          left: { '@type': 'ArithmeticValue', data: 2 },
          right: { '@type': 'ArithmeticValue', data: 3 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Exp',
          left: { '@type': 'ArithmeticValue', data: 2.0 },
          right: { '@type': 'ArithmeticValue', data: 3.0 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(8)
      expect(r2.body.bindings[0].Result['@value']).to.equal(8)
    })

    it('Floor: floor(5.9) === floor(5.90)', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Floor',
          argument: { '@type': 'ArithmeticValue', data: 5.9 },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Floor',
          argument: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '5.90' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(5)
      expect(r2.body.bindings[0].Result['@value']).to.equal(5)
    })
  })

  describe('WOQL Arithmetic - Leading Zero Canonicalization', function () {
    it('Plus: .1 + .2 === 0.1 + 0.2 === 0.10 + 0.20 (all equal 0.3)', async function () {
      const variations = [
        { left: '.1', right: '.2' },
        { left: '0.1', right: '0.2' },
        { left: '0.10', right: '0.20' },
      ]

      for (const v of variations) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: {
              '@type': 'ArithmeticValue',
              data: { '@type': 'xsd:decimal', '@value': v.left },
            },
            right: {
              '@type': 'ArithmeticValue',
              data: { '@type': 'xsd:decimal', '@value': v.right },
            },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings[0].Result['@value']).to.equal(0.3)
      }
    })

    it('Times: .5 * 2 === 0.5 * 2 === 0.50 * 2', async function () {
      const variations = ['.5', '0.5', '0.50']

      for (const val of variations) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Times',
            left: {
              '@type': 'ArithmeticValue',
              data: { '@type': 'xsd:decimal', '@value': val },
            },
            right: { '@type': 'ArithmeticValue', data: 2 },
          },
          result: { '@type': 'ArithmeticValue', variable: 'Result' },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings[0].Result['@value']).to.equal(1)
      }
    })

    it('Divide: .6 / .2 === 0.6 / 0.2', async function () {
      const query1 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '.6' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '.2' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const query2 = {
        '@type': 'Eval',
        expression: {
          '@type': 'Divide',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '0.6' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '0.2' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const r1 = await woql.post(agent, query1)
      const r2 = await woql.post(agent, query2)

      expect(r1.body.bindings[0].Result['@value']).to.equal(3)
      expect(r2.body.bindings[0].Result['@value']).to.equal(3)
    })
  })

  describe('WOQL Comparison - Canonical Equivalence', function () {
    it('Equals: 2 = 2.0 = 2.00', async function () {
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

    it('Equals: 0.1 = .1 = 0.10', async function () {
      const pairs = [
        ['0.1', '.1'],
        ['.1', '0.10'],
        ['0.1', '0.10'],
      ]

      for (const [left, right] of pairs) {
        const query = {
          '@type': 'Equals',
          left: {
            '@type': 'DataValue',
            data: { '@type': 'xsd:decimal', '@value': left },
          },
          right: {
            '@type': 'DataValue',
            data: { '@type': 'xsd:decimal', '@value': right },
          },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings).to.have.lengthOf(1)
      }
    })

    it('Less: 2 < 3 with canonical variations', async function () {
      const variations = [
        [2, 3],
        [2.0, 3.0],
        [{ '@type': 'xsd:decimal', '@value': '2.00' }, { '@type': 'xsd:decimal', '@value': '3.00' }],
      ]

      for (const [left, right] of variations) {
        const query = {
          '@type': 'Less',
          left: { '@type': 'DataValue', data: left },
          right: { '@type': 'DataValue', data: right },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings).to.have.lengthOf(1)
      }
    })

    it('Less: NOT 2 < 2.0 (canonical equivalents)', async function () {
      const query = {
        '@type': 'Less',
        left: { '@type': 'DataValue', data: 2 },
        right: { '@type': 'DataValue', data: 2.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(0)
    })

    it('Greater: 3 > 2 with canonical variations', async function () {
      const variations = [
        [3, 2],
        [3.0, 2.0],
        [{ '@type': 'xsd:decimal', '@value': '3.00' }, { '@type': 'xsd:decimal', '@value': '2.00' }],
      ]

      for (const [left, right] of variations) {
        const query = {
          '@type': 'Greater',
          left: { '@type': 'DataValue', data: left },
          right: { '@type': 'DataValue', data: right },
        }

        const r = await woql.post(agent, query)
        expect(r.body.bindings).to.have.lengthOf(1)
      }
    })

    it('Greater: NOT 2 > 2.0 (canonical equivalents)', async function () {
      const query = {
        '@type': 'Greater',
        left: { '@type': 'DataValue', data: 2 },
        right: { '@type': 'DataValue', data: 2.0 },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(0)
    })
  })

  describe('WOQL Typecast - Canonical Forms', function () {
    it('Typecast "2.0" to xsd:decimal produces 2', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': '2.0' },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:decimal',
        },
        result: {
          '@type': 'Value',
          variable: 'Result',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.equal(2)
    })

    it('Typecast ".1" to xsd:decimal produces 0.1', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': '.1' },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:decimal',
        },
        result: {
          '@type': 'Value',
          variable: 'Result',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.equal(0.1)
    })

    it('Typecast "2.00" to xsd:integer correctly fails (negative test)', async function () {
      // NEGATIVE TEST: Strings with decimal points should NOT cast to xsd:integer
      // "2.00" has a decimal point, so it's not a valid integer representation
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': '2.00' },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:integer',
        },
        result: {
          '@type': 'Value',
          variable: 'Result',
        },
      }

      const r = await woql.post(agent, query).unverified()

      // Should fail with 400 Bad Request
      expect(r.status).to.equal(400)
      expect(r.body['@type']).to.equal('api:WoqlErrorResponse')
      expect(r.body['api:error']['@type']).to.equal('api:BadCast')
      expect(r.body['api:error']['api:value']).to.equal('2.00')
      expect(r.body['api:error']['api:type']).to.equal('http://www.w3.org/2001/XMLSchema#integer')
    })

    it('Typecast "2" to xsd:integer correctly succeeds', async function () {
      // POSITIVE TEST: String without decimal point should cast successfully
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': '2' },
        },
        type: {
          '@type': 'NodeValue',
          node: 'xsd:integer',
        },
        result: {
          '@type': 'Value',
          variable: 'Result',
        },
      }

      const r = await woql.post(agent, query)
      expect(r.body.bindings[0].Result['@value']).to.equal(2)
      expect(r.body.bindings[0].Result['@type']).to.equal('xsd:integer')
    })
  })

  describe('Document API - Canonical Input', function () {
    it('Insert with 2.0 retrieves as canonical 2', async function () {
      const doc = {
        '@type': 'NumericTest',
        '@id': 'NumericTest/test1',
        decimalField: 2.0,
        integerField: 10,
      }

      await document.insert(agent, { instance: doc })

      const retrieved = await document.get(agent, {
        body: { id: 'NumericTest/test1' },
      })

      expect(retrieved.body.decimalField).to.equal(2)
    })

    it('Insert with ".1" retrieves as canonical 0.1', async function () {
      const doc = {
        '@type': 'NumericTest',
        '@id': 'NumericTest/test2',
        decimalField: { '@type': 'xsd:decimal', '@value': '.1' },
        integerField: 5,
      }

      await document.insert(agent, { instance: doc })

      const retrieved = await document.get(agent, {
        body: { id: 'NumericTest/test2' },
      })

      expect(retrieved.body.decimalField).to.equal(0.1)
    })

    it('Insert with "2.00" retrieves as canonical 2', async function () {
      const doc = {
        '@type': 'NumericTest',
        '@id': 'NumericTest/test3',
        decimalField: '2.00',
        integerField: 3,
      }

      await document.insert(agent, { instance: doc })

      const retrieved = await document.get(agent, {
        body: { id: 'NumericTest/test3' },
      })

      expect(retrieved.body.decimalField).to.equal(2)
    })
  })
})
