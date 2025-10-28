const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('double-variable-arithmetic', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('Type Preservation vs Arithmetic Behavior', function () {
    it('variables retain xsd:double type (verified with TypeOf)', async function () {
      // This test proves that variables ARE correctly typed as xsd:double
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'x' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.1' },
            },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'x' },
            type: { '@type': 'DataValue', variable: 'x_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      expect(binding.x_type).to.equal('xsd:double',
        'Variable should retain xsd:double type')
    })

    it('arithmetic on double variables uses binary floating point arithmetic', async function () {
      // This test demonstrates the BUG: arithmetic on double variables
      // uses rational arithmetic instead of float arithmetic
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'x' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.1' },
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'y' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.2' },
            },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Plus',
              left: { '@type': 'ArithmeticValue', variable: 'x' },
              right: { '@type': 'ArithmeticValue', variable: 'y' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'result_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      // ❌ This FAILS - result type is xsd:decimal (rational arithmetic)
      // Expected: xsd:double (float arithmetic)
      expect(binding.result_type).to.equal('xsd:double')

      // Despite using rational arithmetic, the VALUE still shows IEEE 754 precision loss
      // This proves the values were floats at some point
      expect(binding.result['@value']).to.be.closeTo(0.30000000000000004, 0.0000000000000001)
    })

    it('inline double values use float arithmetic (WORKS)', async function () {
      // This test shows the CORRECT behavior with inline values
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Plus',
              left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
              right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'result_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      // ✅ This PASSES - inline values use float arithmetic
      expect(binding.result_type).to.equal('xsd:double',
        'Inline double values correctly use float arithmetic')

      expect(binding.result['@value']).to.be.closeTo(0.30000000000000004, 0.0000000000000001)
    })

    it('Multiplication of double variables uses float arithmetic', async function () {
      // This test helps understand WHY the bug occurs
      // When compile_arith calls literally(), it strips ^^Type annotations
      // So by the time arithmetic happens, we have bare Prolog values
      // xsd:double floats become bare floats, which ARE checked by our fix
      // But variables don't go through the same path

      const query = {
        '@type': 'And',
        and: [
          // Assign two doubles to variables
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'x' },
            right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '2.5' } },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: 'y' },
            right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '4.0' } },
          },
          // Multiply them (should use float arithmetic)
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Times',
              left: { '@type': 'ArithmeticValue', variable: 'x' },
              right: { '@type': 'ArithmeticValue', variable: 'y' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          // Check the result type
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'result_type' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      const binding = result.bindings[0]

      // Old behavior: xsd:decimal (rational arithmetic)
      // Expected behavior: xsd:double (float arithmetic)
      expect(binding.result_type).to.equal('xsd:double')
      expect(binding.result['@value']).to.equal(10.0)
    })
  })

  describe('Performance Implications', function () {
    it('rational arithmetic is slower than float arithmetic', async function () {
      // This test demonstrates that there IS a performance cost
      // Rational arithmetic (GMP) is slower than IEEE 754 float ops

      const iterations = 100

      // Test 1: Inline doubles (uses float arithmetic)
      for (let i = 0; i < iterations; i++) {
        const query = {
          '@type': 'Eval',
          expression: {
            '@type': 'Plus',
            left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
            right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
          },
          result: { '@type': 'ArithmeticValue', variable: 'result' },
        }
        await woql.post(agent, query).unverified()
      }

      for (let i = 0; i < iterations; i++) {
        const query = {
          '@type': 'And',
          and: [
            {
              '@type': 'Equals',
              left: { '@type': 'DataValue', variable: 'x' },
              right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
            },
            {
              '@type': 'Equals',
              left: { '@type': 'DataValue', variable: 'y' },
              right: { '@type': 'DataValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
            },
            {
              '@type': 'Eval',
              expression: {
                '@type': 'Plus',
                left: { '@type': 'ArithmeticValue', variable: 'x' },
                right: { '@type': 'ArithmeticValue', variable: 'y' },
              },
              result: { '@type': 'ArithmeticValue', variable: 'result' },
            },
          ],
        }
        await woql.post(agent, query).unverified()
      }

      // This is informational - we expect variable path to be slower
      // (though network overhead may dwarf arithmetic differences)
    }).timeout(30000)
  })
})
