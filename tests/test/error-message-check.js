const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

/**
 * Error Message Quality Tests
 *
 * Verifies that type family incompatibility errors provide clear, actionable messages
 * including actual values (in human-readable format), type names, and guidance.
 *
 * Related: docs/NUMERIC_TYPE_SAFETY.md
 */
describe('error-message-check', function () {
  let agent

  before(async function () {
    this.timeout(200000)
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should show clear error message for cross-family comparison', async function () {
    const query = {
      '@type': 'Equals',
      left: {
        '@type': 'Value',
        data: {
          '@type': 'xsd:double',
          '@value': '33.0',
        },
      },
      right: {
        '@type': 'Value',
        data: {
          '@type': 'xsd:decimal',
          '@value': '33.0',
        },
      },
    }

    const r = await woql.post(agent, query).fails()
    // Expected error message format:
    // "33.0 (xsd:double) is not safely comparable with 33.0 (xsd:decimal): ..."
    expect(r.status).to.equal(400)
    expect(r.body['api:message']).to.include('33')
    expect(r.body['api:message']).to.include('xsd:double')
    expect(r.body['api:message']).to.include('xsd:decimal')
  })

  it('should show clear error message for less than cross-family', async function () {
    const query = {
      '@type': 'Less',
      left: {
        '@type': 'Value',
        data: {
          '@type': 'xsd:float',
          '@value': '10.5',
        },
      },
      right: {
        '@type': 'Value',
        data: {
          '@type': 'xsd:integer',
          '@value': '20',
        },
      },
    }

    const r = await woql.post(agent, query).fails()
    // Expected error message format:
    // "10.5 (xsd:float) is not safely comparable with 20.0 (xsd:integer): ..."
    expect(r.status).to.equal(400)
    expect(r.body['api:message']).to.include('10.5')
    expect(r.body['api:message']).to.include('20')
    expect(r.body['api:message']).to.include('xsd:float')
    expect(r.body['api:message']).to.include('xsd:integer')
  })
})
