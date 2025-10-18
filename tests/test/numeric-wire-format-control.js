const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

/**
 * Numeric Wire Format Control Tests
 *
 * CRITICAL: Test exact JSON wire format to ensure server-side canonicalization
 * JavaScript canonicalizes numbers (2.0 → 2), so we must use strings or raw JSON
 * to test that the server correctly handles all canonical variations.
 *
 * These tests send raw JSON with explicit string values to test:
 * - "2.0", "2.00", "2" all canonicalize to 2
 * - ".1", "0.1", "0.10" all canonicalize to 0.1
 * - Very large and very small numbers preserve precision
 */

describe('numeric-wire-format-control', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Wire Format - Whole Number Variations', function () {
    it('Server receives "2", "2.0", "2.00" as distinct strings and canonicalizes all to 2', async function () {
      const variations = ['2', '2.0', '2.00', '2.000']

      for (const val of variations) {
        // Use raw JSON to control exact wire format
        const query = {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': val },
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

        const response = await woql.post(agent, query)
        expect(response.body.bindings).to.have.lengthOf(1)
        
        // All should canonicalize to 2 (no decimal point)
        expect(response.body.bindings[0].Result['@value']).to.equal(2)
      }
    })

    it('Arithmetic with "2.0" string on wire produces canonical 2', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '2.0' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '3.0' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const response = await woql.post(agent, query)
      expect(response.body.bindings[0].Result['@value']).to.equal(5)
    })

    it('Comparison with "2.0" string on wire equals 2', async function () {
      const query = {
        '@type': 'Equals',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:decimal', '@value': '2.0' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:decimal', '@value': '2.00' },
        },
      }

      const response = await woql.post(agent, query)
      expect(response.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('Wire Format - Leading Zero Variations', function () {
    it('Server receives ".1", "0.1", "0.10" as distinct strings and canonicalizes all to 0.1', async function () {
      const variations = ['.1', '0.1', '0.10', '0.100']

      for (const val of variations) {
        const query = {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': val },
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

        const response = await woql.post(agent, query)
        expect(response.body.bindings).to.have.lengthOf(1)
        
        // All should canonicalize to 0.1 (leading zero added, trailing zeros removed)
        expect(response.body.bindings[0].Result['@value']).to.equal(0.1)
      }
    })

    it('Arithmetic with ".1" string on wire produces canonical 0.1', async function () {
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

      const response = await woql.post(agent, query)
      
      // Should be exactly 0.3 (not 0.30000000000000004)
      expect(response.body.bindings[0].Result['@value']).to.equal(0.3)
    })

    it('Comparison ".1" equals "0.1" equals "0.10"', async function () {
      const pairs = [
        ['.1', '0.1'],
        ['0.1', '0.10'],
        ['.1', '0.10'],
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

        const response = await woql.post(agent, query)
        expect(response.body.bindings).to.have.lengthOf(1, 
          `"${left}" should equal "${right}"`)
      }
    })
  })

  describe('Wire Format - Very Large Numbers', function () {
    it('20-digit number preserves full precision on wire (demonstrates JS limitation)', async function () {
      const largeNum = '12345678901234567890'

      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': largeNum },
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

      const response = await woql.post(agent, query).unverified()

      // VERIFY: Server sends full 20 digits in raw JSON
      expect(response.text).to.include(`"@value":${largeNum}`)

      // DEMONSTRATE: JavaScript loses precision during JSON.parse()
      const parsed = JSON.parse(response.text)
      const result = parsed.bindings[0].Result['@value']

      // Expected precision loss (IEEE 754 limitation)
      expect(result.toString()).to.equal('12345678901234567000')
      expect(result.toString()).to.not.equal(largeNum)
    })

    it('Arithmetic with large numbers preserves precision', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '999999999999999999' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '1' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const response = await woql.post(agent, query)
      
      // Should be exactly 1000000000000000000
      const result = response.body.bindings[0].Result['@value']
      expect(result.toString()).to.equal('1000000000000000000')
    })

    it('Large number with trailing zeros: "1000000.0" → "1000000"', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': '1000000.0' },
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

      const response = await woql.post(agent, query)
      
      // Should canonicalize to 1000000 (no .0)
      expect(response.body.bindings[0].Result['@value']).to.equal(1000000)
    })
  })

  describe('Wire Format - Very Small Numbers', function () {
    it('20 decimal places preserves precision on wire', async function () {
      const smallNum = '0.00000000000000000001'

      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': smallNum },
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

      const response = await woql.post(agent, query)
      
      // Should preserve tiny value (20 decimal places)
      const result = response.body.bindings[0].Result['@value']
      expect(result).to.be.closeTo(0.00000000000000000001, 0.000000000000000000001)
    })

    it('Very small with leading zero: ".000001" → "0.000001"', async function () {
      const query = {
        '@type': 'Typecast',
        value: {
          '@type': 'Value',
          data: { '@type': 'xsd:string', '@value': '.000001' },
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

      const response = await woql.post(agent, query)
      
      // Should canonicalize to 0.000001 (leading zero added)
      expect(response.body.bindings[0].Result['@value']).to.equal(0.000001)
    })

    it('Arithmetic with very small numbers: .0001 + .0002 = 0.0003', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Plus',
          left: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '.0001' },
          },
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '.0002' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const response = await woql.post(agent, query)
      
      // Should be exactly 0.0003
      expect(response.body.bindings[0].Result['@value']).to.equal(0.0003)
    })
  })

  describe('Wire Format - Negative Numbers', function () {
    it('Negative with trailing zeros: "-2.0", "-2.00" → "-2"', async function () {
      const variations = ['-2.0', '-2.00', '-2.000']

      for (const val of variations) {
        const query = {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': val },
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

        const response = await woql.post(agent, query)
        
        // All should canonicalize to -2
        expect(response.body.bindings[0].Result['@value']).to.equal(-2)
      }
    })

    it('Negative with leading zero: "-.5", "-0.5", "-0.50" → "-0.5"', async function () {
      const variations = ['-.5', '-0.5', '-0.50']

      for (const val of variations) {
        const query = {
          '@type': 'Typecast',
          value: {
            '@type': 'Value',
            data: { '@type': 'xsd:string', '@value': val },
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

        const response = await woql.post(agent, query)
        
        // All should canonicalize to -0.5
        expect(response.body.bindings[0].Result['@value']).to.equal(-0.5)
      }
    })
  })

  describe('Wire Format - Cross-Type with Strings', function () {
    it('xsd:integer "2" equals xsd:decimal "2.0" on wire', async function () {
      const query = {
        '@type': 'Equals',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:integer', '@value': '2' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:decimal', '@value': '2.0' },
        },
      }

      const response = await woql.post(agent, query)
      expect(response.body.bindings).to.have.lengthOf(1)
    })

    it('xsd:integer "50" equals xsd:decimal "50.00" on wire', async function () {
      const query = {
        '@type': 'Equals',
        left: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:integer', '@value': '50' },
        },
        right: {
          '@type': 'DataValue',
          data: { '@type': 'xsd:decimal', '@value': '50.00' },
        },
      }

      const response = await woql.post(agent, query)
      expect(response.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('Wire Format - Rational Division Precision', function () {
    it('Division "1" / "3" on wire produces rational with 20 digits', async function () {
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

      const response = await woql.post(agent, query)
      
      // Should be high-precision 1/3, not float
      const result = response.body.bindings[0].Result['@value']
      expect(result).to.be.closeTo(0.3333333333333333, 0.0000000000000001)
    })

    it('("1" / "3") * "3" = "1" (exact rational)', async function () {
      const query = {
        '@type': 'Eval',
        expression: {
          '@type': 'Times',
          left: {
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
          right: {
            '@type': 'ArithmeticValue',
            data: { '@type': 'xsd:decimal', '@value': '3' },
          },
        },
        result: { '@type': 'ArithmeticValue', variable: 'Result' },
      }

      const response = await woql.post(agent, query)
      
      // Should be exactly 1 (rational arithmetic is exact)
      expect(response.body.bindings[0].Result['@value']).to.equal(1)
    })
  })
})
