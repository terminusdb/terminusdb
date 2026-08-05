const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

const HEADER_RETRY_COUNT = 'terminusdb-transaction-retry-count'
const HEADER_DATA_VERSION = 'terminusdb-data-version'
const ORIGIN = 'http://example.com'

function verifyRetryCountHeader (response) {
  expect(response.headers).to.have.property(HEADER_RETRY_COUNT)
  const retryCount = Number.parseInt(response.headers[HEADER_RETRY_COUNT], 10)
  expect(retryCount).to.be.a('number')
  expect(Number.isNaN(retryCount)).to.equal(false)
  if (retryCount > 0) {
    console.warn(`${HEADER_RETRY_COUNT} > 0: ${retryCount}`)
  }
  return retryCount
}

function verifyDataVersionHeader (response) {
  expect(response.headers).to.have.property(HEADER_DATA_VERSION)
  const dataVersion = response.headers[HEADER_DATA_VERSION]
  expect(dataVersion).to.be.a('string')
  expect(dataVersion).to.match(/^branch:/)
  return dataVersion
}

function verifyCorsHeaders (response) {
  expect(response.headers).to.have.property('access-control-allow-origin')
  expect(response.headers).to.have.property('access-control-allow-credentials')
  expect(response.headers['access-control-allow-credentials']).to.equal('true')
}

describe('response-headers', function () {
  let agent
  let docId

  before(async function () {
    this.timeout(20000)
    agent = new Agent().auth()
    await db.create(agent, { label: 'Test response headers', schema: true }).unverified()

    const schema = {
      '@type': 'Class',
      '@id': 'Person',
      '@key': {
        '@type': 'Lexical',
        '@fields': ['name'],
      },
      name: 'xsd:string',
    }

    const result = await document.insert(agent, { schema }).set('Origin', ORIGIN).unverified()
    expect(result.status).to.equal(200)
    verifyRetryCountHeader(result)
    verifyDataVersionHeader(result)
  })

  after(async function () {
    this.timeout(20000)
    await db.delete(agent).unverified()
  })

  describe('document interface', function () {
    it('returns retry count, data version, and CORS headers on document insert', async function () {
      this.timeout(20000)
      const doc = {
        '@type': 'Person',
        name: 'Alice',
      }

      const result = await document.insert(agent, { instance: doc }).set('Origin', ORIGIN).unverified()
      expect(result.status).to.equal(200)
      expect(result.headers).to.have.property('content-type')
      verifyRetryCountHeader(result)
      verifyDataVersionHeader(result)
      verifyCorsHeaders(result)

      docId = result.body[0]
    })

    it('returns retry count, data version, and CORS headers on document replace', async function () {
      this.timeout(20000)
      expect(docId).to.be.a('string')

      const doc = {
        '@type': 'Person',
        '@id': docId,
        name: 'Alice',
      }

      const result = await document.replace(agent, { instance: doc }).set('Origin', ORIGIN).unverified()
      expect(result.status).to.equal(200)
      expect(result.headers).to.have.property('content-type')
      verifyRetryCountHeader(result)
      verifyDataVersionHeader(result)
      verifyCorsHeaders(result)
    })

    it('returns retry count, data version, and CORS headers on document delete', async function () {
      this.timeout(20000)
      expect(docId).to.be.a('string')

      const result = await document.delete(agent, { query: { id: docId } }).set('Origin', ORIGIN).unverified()
      expect(result.status).to.equal(204)
      verifyRetryCountHeader(result)
      verifyDataVersionHeader(result)
      verifyCorsHeaders(result)
    })
  })

  describe('woql interface', function () {
    it('returns retry count and data version headers on woql post', async function () {
      this.timeout(20000)
      const query = {
        '@type': 'Equals',
        left: { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'hello' } },
        right: { '@type': 'DataValue', data: { '@type': 'xsd:string', '@value': 'hello' } },
      }

      const result = await woql.post(agent, query).set('Origin', ORIGIN).unverified()
      expect(result.status).to.equal(200)
      verifyRetryCountHeader(result)
      verifyDataVersionHeader(result)
      verifyCorsHeaders(result)
    })
  })

  describe('graphql interface', function () {
    it('returns retry count and data version headers on graphql query', async function () {
      this.timeout(20000)
      const path = `/api/graphql/${agent.orgName}/${agent.dbName}`
      const result = await agent
        .post(path)
        .set('Origin', ORIGIN)
        .set('Content-Type', 'application/json')
        .send({ query: '{ Person { name } }' })

      expect(result.status).to.equal(200)
      verifyRetryCountHeader(result)
      verifyDataVersionHeader(result)
      verifyCorsHeaders(result)
    })
  })
})
