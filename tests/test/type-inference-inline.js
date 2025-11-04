const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('type-inference-inline', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should return xsd:double when adding two inline xsd:double values', async function () {
    // This test uses INLINE data values, not variables
    // Matches the working test pattern from xsd-float-double-canonicalization.js
    const query = {
      '@type': 'Eval',
      expression: {
        '@type': 'Plus',
        left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.1' } },
        right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '0.2' } },
      },
      result: { '@type': 'ArithmeticValue', variable: 'Result' },
    }

    const response = await woql.post(agent, query).unverified()
    const result = JSON.parse(response.text)

    const resultType = result.bindings[0].Result['@type']
    const resultValue = result.bindings[0].Result['@value']

    // Should return xsd:double
    expect(resultType).to.equal('xsd:double',
      'Inline xsd:double + xsd:double should return xsd:double')

    // Value should be IEEE 754 result
    expect(Math.abs(resultValue - 0.3)).to.be.lessThan(0.001)
  })
})
