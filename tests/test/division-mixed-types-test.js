// Quick test to verify division behavior with mixed types
const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

const agent = new Agent().auth()

describe('Division with Mixed Types', function () {
  before(async function () {
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('xsd:double / xsd:decimal uses float division', async function () {
    // Mixed types: xsd:double / xsd:decimal
    // Uses / (float division) because ANY arg is xsd:double
    // Result type is xsd:double (mixed type rule)
    const query = {
      '@type': 'Eval',
      expression: {
        '@type': 'Divide',
        left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '1.0' } },
        right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '3.0' } },
      },
      result: { '@type': 'ArithmeticValue', variable: 'Result' },
    }

    const r = await woql.post(agent, query)
    expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
    expect(r.body.bindings[0].Result['@value']).to.be.closeTo(0.3333333333333333, 0.0000000000000001)
  })

  it('xsd:decimal / xsd:double also uses float division', async function () {
    const query = {
      '@type': 'Eval',
      expression: {
        '@type': 'Divide',
        left: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:decimal', '@value': '1.0' } },
        right: { '@type': 'ArithmeticValue', data: { '@type': 'xsd:double', '@value': '3.0' } },
      },
      result: { '@type': 'ArithmeticValue', variable: 'Result' },
    }

    const r = await woql.post(agent, query)

    expect(r.body.bindings[0].Result['@type']).to.equal('xsd:double')
  })
})
