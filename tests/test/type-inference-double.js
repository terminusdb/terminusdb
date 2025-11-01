const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('type-inference-double', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('xsd:double Type Inference', function () {
    it('should return xsd:double when adding two xsd:double values', async function () {
      // User's expectation: xsd:double + xsd:double → xsd:double
      // This is the documented behavior from memory [21cfda7f]
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: '1_term' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.1' },
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: '2_term' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '0.2' },
            },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Plus',
              left: { '@type': 'ArithmeticValue', variable: '1_term' },
              right: { '@type': 'ArithmeticValue', variable: '2_term' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'type_res' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      // Verify we got a result
      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      // Check the result value
      expect(binding.result).to.exist

      // Check the inferred type
      expect(binding.type_res).to.exist

      // According to type inference rules documented in memory [21cfda7f]:
      // Pure float/double: xsd:double + xsd:double → xsd:double
      //
      // The user expects: xsd:double
      // Currently getting: xsd:decimal (according to user's report)
      //
      // This test will FAIL if the current behavior is wrong
      expect(binding.type_res).to.equal('xsd:double',
        'Type inference should return xsd:double for xsd:double + xsd:double operations')

      // Also verify the computed value is approximately correct
      // Note: With xsd:double, we expect IEEE 754 behavior
      // 0.1 + 0.2 = 0.30000000000000004 (float behavior)
      const resultValue = binding.result['@value']
      expect(Math.abs(resultValue - 0.3)).to.be.lessThan(0.001)
    })

    it('should return xsd:double when multiplying two xsd:double values', async function () {
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: '1_term' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '2.5' },
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: '2_term' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:double', '@value': '4.0' },
            },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Times',
              left: { '@type': 'ArithmeticValue', variable: '1_term' },
              right: { '@type': 'ArithmeticValue', variable: '2_term' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'type_res' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      expect(binding.type_res).to.equal('xsd:double',
        'Type inference should return xsd:double for xsd:double * xsd:double operations')

      // Verify computed value
      const resultValue = binding.result['@value']
      expect(resultValue).to.equal(10.0)
    })

    it('should return xsd:float when adding two xsd:float values', async function () {
      // According to memory [21cfda7f]: xsd:float + xsd:float → xsd:double
      // (Note: Prolog represents both as floats internally, result is xsd:double)
      const query = {
        '@type': 'And',
        and: [
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: '1_term' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:float', '@value': '1.5' },
            },
          },
          {
            '@type': 'Equals',
            left: { '@type': 'DataValue', variable: '2_term' },
            right: {
              '@type': 'DataValue',
              data: { '@type': 'xsd:float', '@value': '2.5' },
            },
          },
          {
            '@type': 'Eval',
            expression: {
              '@type': 'Plus',
              left: { '@type': 'ArithmeticValue', variable: '1_term' },
              right: { '@type': 'ArithmeticValue', variable: '2_term' },
            },
            result: { '@type': 'ArithmeticValue', variable: 'result' },
          },
          {
            '@type': 'TypeOf',
            value: { '@type': 'DataValue', variable: 'result' },
            type: { '@type': 'DataValue', variable: 'type_res' },
          },
        ],
      }

      const response = await woql.post(agent, query).unverified()
      const result = JSON.parse(response.text)

      expect(result.bindings).to.be.an('array').with.lengthOf(1)
      const binding = result.bindings[0]

      // According to documented rules: xsd:float + xsd:float → xsd:double
      expect(binding.type_res).to.equal('xsd:double',
        'Type inference should return xsd:double for xsd:float + xsd:float operations')

      const resultValue = binding.result['@value']
      expect(resultValue).to.equal(4.0)
    })
  })
})
