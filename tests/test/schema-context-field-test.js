const { expect } = require('chai')
const { Agent, db, document, util } = require('../lib')

describe('@context Property in Schema Context', function () {
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
})
