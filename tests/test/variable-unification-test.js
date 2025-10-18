const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

describe('variable-unification-with-equals', function () {
  let agent

  before(async function () {
    this.timeout(200000)
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should unify variable with literal value', async function () {
    const query = {
      '@type': 'Equals',
      left: {
        '@type': 'Value',
        variable: 'X',
      },
      right: {
        '@type': 'Value',
        data: {
          '@type': 'xsd:integer',
          '@value': '42',
        },
      },
    }

    const r = await woql.post(agent, query)
    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].X['@value']).to.equal(42)
    expect(r.body.bindings[0].X['@type']).to.equal('xsd:integer')
  })

  it('should unify variable with decimal literal', async function () {
    const query = {
      '@type': 'Equals',
      left: {
        '@type': 'Value',
        variable: 'Y',
      },
      right: {
        '@type': 'Value',
        data: {
          '@type': 'xsd:decimal',
          '@value': '3.14',
        },
      },
    }

    const r = await woql.post(agent, query)
    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].Y['@value']).to.equal(3.14)
    expect(r.body.bindings[0].Y['@type']).to.equal('xsd:decimal')
  })

  it('should unify two variables together', async function () {
    const query = {
      '@type': 'Equals',
      left: {
        '@type': 'Value',
        variable: 'X',
      },
      right: {
        '@type': 'Value',
        variable: 'Y',
      },
    }

    const r = await woql.post(agent, query)
    expect(r.body.bindings).to.have.lengthOf(1)
    // Both variables should be unified (same variable)
  })
})
