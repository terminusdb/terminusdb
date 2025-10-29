const { expect } = require('chai')
const { Agent, db, document, util } = require('../lib')

describe('reserved-marker-guard', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)

    // Create a simple schema with string fields
    const schema = [
      {
        '@type': 'Class',
        '@id': 'TestDoc',
        name: 'xsd:string',
        description: 'xsd:string',
      },
    ]
    await document.insert(agent, { schema })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('should reject string values starting with __TERMINUS_NUM__', async function () {
    const doc = {
      '@type': 'TestDoc',
      '@id': 'TestDoc/test1',
      name: '__TERMINUS_NUM__123.456',
      description: 'valid string',
    }

    const response = await document.insert(agent, { instance: [doc] }).unverified()
    expect(response.status).to.equal(400)
    expect(response.body['api:message']).to.match(/String values cannot start with reserved marker prefix/)
    expect(response.body['api:error']['@type']).to.equal('api:ReservedMarkerInString')
    expect(response.body['api:error']['api:marker']).to.equal('__TERMINUS_NUM__')
  })

  it('should allow normal string values without the marker', async function () {
    const doc = {
      '@type': 'TestDoc',
      '@id': 'TestDoc/test2',
      name: 'Normal string',
      description: 'Another normal string',
    }

    const result = await document.insert(agent, { instance: [doc] })
    expect(result.status).to.equal(200)
  })

  it('should allow strings that contain but do not start with __TERMINUS_NUM__', async function () {
    const doc = {
      '@type': 'TestDoc',
      '@id': 'TestDoc/test3',
      name: 'This contains __TERMINUS_NUM__ in the middle',
      description: 'valid string',
    }

    const result = await document.insert(agent, { instance: [doc] })
    expect(result.status).to.equal(200)
  })

  it('should reject __TERMINUS_NUM__ prefix in any string field', async function () {
    const doc = {
      '@type': 'TestDoc',
      '@id': 'TestDoc/test4',
      name: 'valid name',
      description: '__TERMINUS_NUM__789.012',
    }

    const response = await document.insert(agent, { instance: [doc] }).unverified()
    expect(response.status).to.equal(400)
    expect(response.body['api:message']).to.match(/String values cannot start with reserved marker prefix/)
    expect(response.body['api:error']['@type']).to.equal('api:ReservedMarkerInString')
  })

  it('should reject __TERMINUS_NUM__ with various numeric patterns', async function () {
    const invalidStrings = [
      '__TERMINUS_NUM__123',
      '__TERMINUS_NUM__123.456',
      '__TERMINUS_NUM__1.23e10',
      '__TERMINUS_NUM__-456.789',
      '__TERMINUS_NUM__0.00000001',
    ]

    for (const invalidValue of invalidStrings) {
      const doc = {
        '@type': 'TestDoc',
        '@id': `TestDoc/test_${util.randomString()}`,
        name: invalidValue,
        description: 'test',
      }

      const response = await document.insert(agent, { instance: [doc] }).unverified()
      expect(response.status).to.equal(400)
      expect(response.body['api:error']['@type']).to.equal('api:ReservedMarkerInString')
    }
  })

  it('should allow valid strings with similar but different prefixes', async function () {
    const validStrings = [
      '_TERMINUS_NUM__123', // Single underscore
      '__TERMINUS_NUM_123', // Single underscore before number
      '__TERMINUS_NUMBER__123', // Different suffix
      'PREFIX__TERMINUS_NUM__123', // Has prefix before marker
    ]

    for (const validValue of validStrings) {
      const doc = {
        '@type': 'TestDoc',
        '@id': `TestDoc/test_${util.randomString()}`,
        name: validValue,
        description: 'test',
      }

      const result = await document.insert(agent, { instance: [doc] })
      expect(result.status).to.equal(200)
    }
  })
})
