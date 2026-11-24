const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('xsd:anyURI validation', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('accepts relative URI references per XSD 1.1 spec', function () {
    before(async function () {
      await db.create(agent, { label: 'Test anyURI Validation', schema: true })

      const schema = {
        '@type': 'Class',
        '@id': 'BaseInformation',
        '@key': {
          '@type': 'Random',
        },
        schemaLocation: 'xsd:anyURI',
      }

      const result = await document.insert(agent, { schema }).unverified()
      expect(result.status).to.equal(200)
    })

    after(async function () {
      await db.delete(agent)
    })

    it('accepts bare filename as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: 'helloWorld.xsd',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts relative path as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: '../schemas/test.xsd',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts absolute URL as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: 'http://example.com/schema.xsd',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts fragment-only URI as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: '#section1',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts query string URI as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: '?query=value',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts prefixed IRI (CURIE) as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: 'schema:Person',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts rdf:type style prefixed IRI as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: 'rdf:type',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts prefixed IRI with path as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: 'foaf:knows/friend',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts IRI with Japanese characters as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: 'http://example.com/日本語',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })

    it('accepts IRI with accented characters as anyURI', async function () {
      const doc = {
        '@type': 'BaseInformation',
        schemaLocation: 'données/café.xml',
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
    })
  })
})
