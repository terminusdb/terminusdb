const { expect } = require('chai')
const { Agent, db, document, util } = require('../lib')

/**
 * @context supports being a URI, JSON-LD @context array OR a JSON-LD @context object.
 * The @context field, within these constraints, is handled as sys:JSON.
 * All @-prefixed keys are converted to @@-prefixed predicates in the triples
 * This enables a consistent handling of JSON-LD keywords, as @id and @type are
 * required by the sys:JSON triple-based representation.
 * @context as a string will will set the Link http header to indicate how to treat the JSON
 * @context does not affect how TerminusDB handles records internally, it is purely
 * meant to enable clients to treat the materialized documents, when encoded as JSON.
 */

describe('schema-context-at-context-field', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Schema Context @context field', function () {
    beforeEach(async function () {
      agent.dbName = `test_context_field_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Context Field', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should store and retrieve @context as dict in schema context', async function () {
      const contextWithContextField = {
        ...util.defaultContext,
        '@context': {
          dfrnt2: 'https://dfrnt.com',
          schema: 'http://schema.org/',
        },
      }

      await document.insert(agent, {
        schema: [contextWithContextField],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.deep.equal({
        dfrnt2: 'https://dfrnt.com',
        schema: 'http://schema.org/',
      })
    })

    it('should support @context alongside top-level prefixes', async function () {
      const contextWithBoth = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': {
          dfrnt2: 'https://dfrnt.com',
        },
        dfrnt: 'https://dfrnt.com/schema#',
      }

      await document.insert(agent, {
        schema: [contextWithBoth],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.deep.equal({
        dfrnt2: 'https://dfrnt.com',
      })
      expect(context.dfrnt).to.equal('https://dfrnt.com/schema#')
    })

    it('should handle nested @-prefixed keys in @context field', async function () {
      const contextWithNestedAt = {
        ...util.defaultContext,
        '@context': {
          '@id': 'some_id',
          '@type': 'SomeType',
          nested: {
            '@value': 'nested_value',
          },
        },
      }

      await document.insert(agent, {
        schema: [contextWithNestedAt],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      // Verify sys:context no longer leaks
      expect(context['sys:context']).to.be.undefined
      expect(context['@context']['@id']).to.equal('some_id')
      expect(context['@context']['@type']).to.equal('SomeType')
      expect(context['@context'].nested['@value']).to.equal('nested_value')
    })

    it('should support both @context and @metadata in schema context', async function () {
      const contextWithBoth = {
        ...util.defaultContext,
        '@context': {
          schema: 'http://schema.org/',
        },
        '@metadata': {
          version: '1.0',
          '@id': 'metadata_id',
        },
      }

      await document.insert(agent, {
        schema: [contextWithBoth],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.deep.equal({
        schema: 'http://schema.org/',
      })
      expect(context['@metadata']).to.deep.equal({
        version: '1.0',
        '@id': 'metadata_id',
      })
    })
  })

  describe('@context array support (JSON-LD compliant)', function () {
    beforeEach(async function () {
      agent.dbName = `test_context_array_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Context Array', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should store and retrieve @context as empty array', async function () {
      const contextWithEmptyArray = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': [],
      }

      await document.insert(agent, {
        schema: [contextWithEmptyArray],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.deep.equal([])
    })

    it('should store and retrieve @context as array of strings', async function () {
      // JSON-LD allows @context to be an array of URIs
      const contextWithArrayOfStrings = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': [
          'http://schema.org/',
          'https://example.com/context.jsonld',
        ],
      }

      await document.insert(agent, {
        schema: [contextWithArrayOfStrings],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.deep.equal([
        'http://schema.org/',
        'https://example.com/context.jsonld',
      ])
    })

    it('should store and retrieve @context as array of dicts', async function () {
      // JSON-LD allows @context to be an array of context objects
      const contextWithArrayOfDicts = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': [
          { schema: 'http://schema.org/' },
          { dfrnt: 'https://dfrnt.com/' },
        ],
      }

      await document.insert(agent, {
        schema: [contextWithArrayOfDicts],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.deep.equal([
        { schema: 'http://schema.org/' },
        { dfrnt: 'https://dfrnt.com/' },
      ])
    })

    it('should store and retrieve @context as mixed array (strings and dicts)', async function () {
      // JSON-LD allows @context to be a mixed array
      const contextWithMixedArray = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': [
          'http://schema.org/',
          {
            dfrnt: 'https://dfrnt.com/',
            custom: 'http://example.com/custom#',
          },
          'https://w3id.org/security/v1',
        ],
      }

      await document.insert(agent, {
        schema: [contextWithMixedArray],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.deep.equal([
        'http://schema.org/',
        {
          dfrnt: 'https://dfrnt.com/',
          custom: 'http://example.com/custom#',
        },
        'https://w3id.org/security/v1',
      ])
    })

    it('should store and retrieve @context array with nested @id and @type (array of strings)', async function () {
      // JSON-LD term definitions can include @id and @type with arrays
      const contextWithNestedSpecialKeys = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': [
          {
            '@id': 'http://example.com/base',
            '@type': ['http://example.com/Type1', 'http://example.com/Type2'],
            Person: {
              '@id': 'http://schema.org/Person',
              '@type': '@id',
            },
          },
        ],
      }

      await document.insert(agent, {
        schema: [contextWithNestedSpecialKeys],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@context']).to.be.an('array').with.lengthOf(1)
      expect(context['@context'][0]['@id']).to.equal('http://example.com/base')
      expect(context['@context'][0]['@type']).to.deep.equal([
        'http://example.com/Type1',
        'http://example.com/Type2',
      ])
      expect(context['@context'][0].Person).to.deep.equal({
        '@id': 'http://schema.org/Person',
        '@type': '@id',
      })
    })
  })

  describe('@context validation - reject invalid types', function () {
    beforeEach(async function () {
      agent.dbName = `test_context_validation_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Context Validation', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should reject @context as boolean', async function () {
      const contextWithBooleanContext = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': true,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithBooleanContext],
          fullReplace: true,
        })
        expect.fail('Should have rejected boolean @context')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })

    it('should reject @context as null', async function () {
      const contextWithNullContext = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': null,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithNullContext],
          fullReplace: true,
        })
        expect.fail('Should have rejected null @context')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })

    it('should reject @context as integer number', async function () {
      const contextWithNumberContext = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': 42,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithNumberContext],
          fullReplace: true,
        })
        expect.fail('Should have rejected number @context')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })

    it('should reject @context as float number', async function () {
      const contextWithFloatContext = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@context': 2.0,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithFloatContext],
          fullReplace: true,
        })
        expect.fail('Should have rejected float @context')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })
  })
})
