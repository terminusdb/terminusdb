const { expect } = require('chai')
const { Agent, db, document, util } = require('../lib')
const axios = require('axios')

describe('HTTP Link Header for @context', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Schema Context @context Link Header', function () {
    beforeEach(async function () {
      agent.dbName = `test_link_header_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent)
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should add Link header for string @context when retrieving instance documents', async function () {
      // Schema with string @context and a Person class
      const contextWithStringContext = {
        ...util.defaultContext,
        '@context': 'https://schema.org/docs/jsonldcontext.jsonld',
      }
      const personClass = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
      }

      const insertResult = await document.insert(agent, {
        schema: [contextWithStringContext, personClass],
        fullReplace: true,
      }).unverified()
      if (insertResult.status !== 200) {
        console.error('Schema insert failed:', JSON.stringify(insertResult.body, null, 2))
        throw new Error(`Schema insert failed: ${insertResult.status}`)
      }

      // Insert a Person instance
      const janeDoe = {
        '@type': 'Person',
        '@id': 'Person/janedoe',
        name: 'Jane Doe',
      }
      await document.insert(agent, { instance: [janeDoe] })

      // Retrieve the instance document and check Link header
      const response = await axios.get(
        `http://127.0.0.1:6363/api/document/admin/${agent.dbName}`,
        {
          auth: { username: 'admin', password: 'root' },
          params: { id: 'Person/janedoe' },
          validateStatus: () => true, // Don't throw on non-2xx
        },
      )

      if (response.status !== 200) {
        console.error('GET failed:', response.status, JSON.stringify(response.data, null, 2))
        throw new Error(`GET failed: ${response.status}`)
      }
      expect(response.headers.link).to.exist
      expect(response.headers.link).to.include('https://schema.org/docs/jsonldcontext.jsonld')
      expect(response.headers.link).to.include('rel="http://www.w3.org/ns/json-ld#context"')
      expect(response.headers.link).to.include('type="application/ld+json"')
    })

    it('should NOT add Link header for object @context', async function () {
      const contextWithObjectContext = {
        ...util.defaultContext,
        '@context': {
          schema: 'http://schema.org/',
          foaf: 'http://xmlns.com/foaf/0.1/',
        },
      }
      const personClass = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
      }

      await document.insert(agent, {
        schema: [contextWithObjectContext, personClass],
        fullReplace: true,
      })

      const janeDoe = { '@type': 'Person', '@id': 'Person/janedoe', name: 'Jane Doe' }
      await document.insert(agent, { instance: [janeDoe] })

      const response = await axios.get(
        `http://127.0.0.1:6363/api/document/admin/${agent.dbName}`,
        {
          auth: { username: 'admin', password: 'root' },
          params: { id: 'Person/janedoe' },
          validateStatus: () => true,
        },
      )

      expect(response.status).to.equal(200)
      expect(response.headers.link).to.be.undefined
    })

    it('should NOT add Link header when @context property is missing', async function () {
      const contextWithoutContext = {
        ...util.defaultContext,
      }
      delete contextWithoutContext['@context']
      const personClass = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
      }

      await document.insert(agent, {
        schema: [contextWithoutContext, personClass],
        fullReplace: true,
      })

      const janeDoe = { '@type': 'Person', '@id': 'Person/janedoe', name: 'Jane Doe' }
      await document.insert(agent, { instance: [janeDoe] })

      const response = await axios.get(
        `http://127.0.0.1:6363/api/document/admin/${agent.dbName}`,
        {
          auth: { username: 'admin', password: 'root' },
          params: { id: 'Person/janedoe' },
          validateStatus: () => true,
        },
      )

      expect(response.status).to.equal(200)
      expect(response.headers.link).to.be.undefined
    })

    it('should add Link header for atom string @context', async function () {
      const contextWithAtomContext = {
        ...util.defaultContext,
        '@context': 'http://purl.org/dc/terms/',
      }
      const personClass = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
      }

      await document.insert(agent, {
        schema: [contextWithAtomContext, personClass],
        fullReplace: true,
      })

      const janeDoe = { '@type': 'Person', '@id': 'Person/janedoe', name: 'Jane Doe' }
      await document.insert(agent, { instance: [janeDoe] })

      const response = await axios.get(
        `http://127.0.0.1:6363/api/document/admin/${agent.dbName}`,
        {
          auth: { username: 'admin', password: 'root' },
          params: { id: 'Person/janedoe' },
          validateStatus: () => true,
        },
      )

      expect(response.status).to.equal(200)
      expect(response.headers.link).to.exist
      expect(response.headers.link).to.include('http://purl.org/dc/terms/')
    })

    it('should retrieve document correctly with string @context', async function () {
      const contextWithStringContext = {
        ...util.defaultContext,
        '@context': 'https://schema.org/docs/jsonldcontext.jsonld',
      }
      const personClass = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
      }

      await document.insert(agent, {
        schema: [contextWithStringContext, personClass],
        fullReplace: true,
      })

      const janeDoe = { '@type': 'Person', '@id': 'Person/janedoe', name: 'Jane Doe' }
      await document.insert(agent, { instance: [janeDoe] })

      const response = await axios.get(
        `http://127.0.0.1:6363/api/document/admin/${agent.dbName}`,
        {
          auth: { username: 'admin', password: 'root' },
          params: { id: 'Person/janedoe' },
          validateStatus: () => true,
        },
      )

      expect(response.status).to.equal(200)
      expect(response.data).to.exist
      expect(response.data['@type']).to.equal('Person')
      expect(response.data.name).to.equal('Jane Doe')
      expect(response.headers.link).to.exist
    })

    it('should NOT add Link header when no @context in schema', async function () {
      // Schema without @context property
      const classSchema = {
        '@type': 'Class',
        '@id': 'Person',
        name: 'xsd:string',
      }

      await document.insert(agent, {
        schema: [util.defaultContext, classSchema],
        fullReplace: true,
      })

      const janeDoe = { '@type': 'Person', '@id': 'Person/janedoe', name: 'Jane Doe' }
      await document.insert(agent, { instance: [janeDoe] })

      const response = await axios.get(
        `http://127.0.0.1:6363/api/document/admin/${agent.dbName}`,
        {
          auth: { username: 'admin', password: 'root' },
          params: { id: 'Person/janedoe' },
          validateStatus: () => true,
        },
      )

      expect(response.status).to.equal(200)
      expect(response.headers.link).to.be.undefined
    })
  })
})
