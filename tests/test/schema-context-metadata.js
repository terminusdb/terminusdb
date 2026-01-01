const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

/**
 * @metadata supports being a string (example URI), JSON array OR a JSON object.
 * The @metadata field, within these constraints, is handled as sys:JSON
 * All @-prefixed keys are converted to @@-prefixed predicates in the triples
 * This enables a consistent handling of JSON-LD keywords, as @id and @type are
 * required by the sys:JSON triple-based representation.
 */

describe('schema-context-metadata', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('@metadata in Schema Context (@type: @context)', function () {
    beforeEach(async function () {
      agent.dbName = `test_metadata_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Metadata Fields', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should store and retrieve @metadata with @id field in schema context', async function () {
      const contextWithMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': {
          '@id': 'my_context_id',
          some_key: 'some_value',
        },
      }

      // Insert schema context with @metadata containing @id
      await document.insert(agent, {
        schema: [contextWithMetadata],
        fullReplace: true,
      })

      // Retrieve and verify
      const result = await document.get(agent, { query: { graph_type: 'schema' } })

      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')
      expect(context).to.exist
      expect(context['@metadata']).to.exist
      expect(context['@metadata']['@id']).to.equal('my_context_id')
      expect(context['@metadata'].some_key).to.equal('some_value')
    })

    it('should store and retrieve @metadata with @type field in schema context', async function () {
      const contextWithMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': {
          '@type': 'CustomMetadataType',
          description: 'metadata description',
        },
      }

      // Insert schema context with @metadata containing @type
      await document.insert(agent, {
        schema: [contextWithMetadata],
        fullReplace: true,
      })

      // Retrieve and verify
      const result = await document.get(agent, { query: { graph_type: 'schema' } })

      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')
      expect(context).to.exist
      expect(context['@metadata']).to.exist
      expect(context['@metadata']['@type']).to.equal('CustomMetadataType')
      expect(context['@metadata'].description).to.equal('metadata description')
    })

    it('should store and retrieve @metadata with @context field in schema context', async function () {
      const contextWithMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': {
          '@context': {
            name: 'http://schema.org/name',
            Person: 'http://schema.org/Person',
          },
          version: '1.0',
        },
      }

      // Insert schema context with @metadata containing @context
      await document.insert(agent, {
        schema: [contextWithMetadata],
        fullReplace: true,
      })

      // Retrieve and verify
      const result = await document.get(agent, { query: { graph_type: 'schema' } })

      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')
      expect(context).to.exist
      expect(context['@metadata']).to.exist
      expect(context['@metadata']['@context']).to.deep.equal({
        name: 'http://schema.org/name',
        Person: 'http://schema.org/Person',
      })
      expect(context['@metadata'].version).to.equal('1.0')
    })

    it('should store and retrieve @metadata with all three special fields (@id, @type, @context)', async function () {
      const contextWithMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': {
          '@id': 'metadata_identifier',
          '@type': 'MetadataContainer',
          '@context': {
            foo: 'http://example.com/foo',
          },
          regular_field: 'regular_value',
        },
      }

      // Insert schema context with @metadata containing all special fields
      await document.insert(agent, {
        schema: [contextWithMetadata],
        fullReplace: true,
      })

      // Retrieve and verify
      const result = await document.get(agent, { query: { graph_type: 'schema' } })

      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')
      expect(context).to.exist
      expect(context['@metadata']).to.exist
      expect(context['@metadata']['@id']).to.equal('metadata_identifier')
      expect(context['@metadata']['@type']).to.equal('MetadataContainer')
      expect(context['@metadata']['@context']).to.deep.equal({
        foo: 'http://example.com/foo',
      })
      expect(context['@metadata'].regular_field).to.equal('regular_value')
    })

    it('should handle nested @id, @type, @context in @metadata', async function () {
      const contextWithMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': {
          nested: {
            '@id': 'nested_id',
            '@type': 'NestedType',
            '@context': { bar: 'http://example.com/bar' },
            data: 'nested_data',
          },
          array: [
            { '@id': 'item1', value: 1 },
            { '@type': 'ItemType', value: 2 },
            { '@context': { baz: 'http://example.com/baz' }, value: 3 },
          ],
        },
      }

      // Insert schema context with nested special fields in @metadata
      await document.insert(agent, {
        schema: [contextWithMetadata],
        fullReplace: true,
      })

      // Retrieve and verify
      const result = await document.get(agent, { query: { graph_type: 'schema' } })

      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')
      expect(context).to.exist
      expect(context['@metadata']).to.exist
      expect(context['@metadata'].nested['@id']).to.equal('nested_id')
      expect(context['@metadata'].nested['@type']).to.equal('NestedType')
      expect(context['@metadata'].nested['@context']).to.deep.equal({
        bar: 'http://example.com/bar',
      })
      expect(context['@metadata'].nested.data).to.equal('nested_data')

      expect(context['@metadata'].array).to.be.an('array').with.lengthOf(3)
      expect(context['@metadata'].array[0]['@id']).to.equal('item1')
      expect(context['@metadata'].array[1]['@type']).to.equal('ItemType')
      expect(context['@metadata'].array[2]['@context']).to.deep.equal({
        baz: 'http://example.com/baz',
      })
    })

    it('should allow any JSON value for @id in @metadata', async function () {
      const testCases = [
        { '@id': 123, desc: 'numeric @id' },
        { '@id': true, desc: 'boolean @id' },
        { '@id': ['array', 'of', 'values'], desc: 'array @id' },
        { '@id': { nested: 'object' }, desc: 'object @id' },
        { '@id': null, desc: 'null @id' },
      ]

      for (const testCase of testCases) {
        const contextWithMetadata = {
          '@type': '@context',
          '@base': 'terminusdb:///data/',
          '@schema': 'terminusdb:///schema#',
          '@metadata': testCase,
        }

        await document.insert(agent, {
          schema: [contextWithMetadata],
          fullReplace: true,
        })

        const result = await document.get(agent, { query: { graph_type: 'schema' } })
        const docs = Array.isArray(result.body) ? result.body : [result.body]
        const context = docs.find((doc) => doc['@type'] === '@context')
        expect(context['@metadata']['@id'], testCase.desc).to.deep.equal(testCase['@id'])
      }
    })

    it('should allow any JSON value for @type in @metadata', async function () {
      const testCases = [
        { '@type': 123, desc: 'numeric @type' },
        { '@type': true, desc: 'boolean @type' },
        { '@type': ['Type1', 'Type2'], desc: 'array @type' },
        { '@type': { complex: 'type' }, desc: 'object @type' },
        { '@type': null, desc: 'null @type' },
      ]

      for (const testCase of testCases) {
        const contextWithMetadata = {
          '@type': '@context',
          '@base': 'terminusdb:///data/',
          '@schema': 'terminusdb:///schema#',
          '@metadata': testCase,
        }

        await document.insert(agent, {
          schema: [contextWithMetadata],
          fullReplace: true,
        })

        const result = await document.get(agent, { query: { graph_type: 'schema' } })
        const docs = Array.isArray(result.body) ? result.body : [result.body]
        const context = docs.find((doc) => doc['@type'] === '@context')
        expect(context['@metadata']['@type'], testCase.desc).to.deep.equal(testCase['@type'])
      }
    })

    it('should allow any JSON value for @context in @metadata', async function () {
      const testCases = [
        { '@context': 'http://example.com/context', desc: 'string @context' },
        { '@context': 123, desc: 'numeric @context' },
        { '@context': true, desc: 'boolean @context' },
        { '@context': ['ctx1', 'ctx2'], desc: 'array @context' },
        { '@context': null, desc: 'null @context' },
      ]

      for (const testCase of testCases) {
        const contextWithMetadata = {
          '@type': '@context',
          '@base': 'terminusdb:///data/',
          '@schema': 'terminusdb:///schema#',
          '@metadata': testCase,
        }

        await document.insert(agent, {
          schema: [contextWithMetadata],
          fullReplace: true,
        })

        const result = await document.get(agent, { query: { graph_type: 'schema' } })
        const docs = Array.isArray(result.body) ? result.body : [result.body]
        const context = docs.find((doc) => doc['@type'] === '@context')
        expect(context['@metadata']['@context'], testCase.desc).to.deep.equal(testCase['@context'])
      }
    })
  })

  describe('@metadata string support', function () {
    beforeEach(async function () {
      agent.dbName = `test_metadata_string_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Metadata String', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should store and retrieve @metadata as string URI', async function () {
      const contextWithStringMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': 'http://example.com/metadata.json',
      }

      await document.insert(agent, {
        schema: [contextWithStringMetadata],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@metadata']).to.equal('http://example.com/metadata.json')
    })
  })

  describe('@metadata array support', function () {
    beforeEach(async function () {
      agent.dbName = `test_metadata_array_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Metadata Array', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should store and retrieve @metadata as empty array', async function () {
      const contextWithEmptyArray = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': [],
      }

      await document.insert(agent, {
        schema: [contextWithEmptyArray],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@metadata']).to.deep.equal([])
    })

    it('should store and retrieve @metadata as array of strings', async function () {
      const contextWithArrayMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': [
          'http://example.com/metadata1.json',
          'http://example.com/metadata2.json',
        ],
      }

      await document.insert(agent, {
        schema: [contextWithArrayMetadata],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@metadata']).to.deep.equal([
        'http://example.com/metadata1.json',
        'http://example.com/metadata2.json',
      ])
    })

    it('should store and retrieve @metadata as array of dicts', async function () {
      const contextWithArrayMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': [
          { version: '1.0', author: 'Alice' },
          { version: '2.0', author: 'Bob' },
        ],
      }

      await document.insert(agent, {
        schema: [contextWithArrayMetadata],
        fullReplace: true,
      })

      const result = await document.get(agent, { query: { graph_type: 'schema' } })
      const docs = Array.isArray(result.body) ? result.body : [result.body]
      const context = docs.find((doc) => doc['@type'] === '@context')

      expect(context).to.exist
      expect(context['@metadata']).to.deep.equal([
        { version: '1.0', author: 'Alice' },
        { version: '2.0', author: 'Bob' },
      ])
    })

    it('should store and retrieve @metadata as mixed array (strings and dicts)', async function () {
      const contextWithMixedArray = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': [
          'http://example.com/base-metadata.json',
          {
            version: '1.0',
            '@id': 'local_metadata',
            '@type': 'MetadataEntry',
          },
          'http://example.com/additional.json',
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
      expect(context['@metadata']).to.deep.equal([
        'http://example.com/base-metadata.json',
        {
          version: '1.0',
          '@id': 'local_metadata',
          '@type': 'MetadataEntry',
        },
        'http://example.com/additional.json',
      ])
    })

    it('should store and retrieve @metadata array with nested @id and @type', async function () {
      const contextWithNestedSpecialKeys = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': [
          {
            '@id': 'metadata_entry_1',
            '@type': ['PrimaryMetadata', 'VersionedMetadata'],
            data: { key: 'value' },
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
      expect(context['@metadata']).to.be.an('array').with.lengthOf(1)
      expect(context['@metadata'][0]['@id']).to.equal('metadata_entry_1')
      expect(context['@metadata'][0]['@type']).to.deep.equal([
        'PrimaryMetadata',
        'VersionedMetadata',
      ])
      expect(context['@metadata'][0].data).to.deep.equal({ key: 'value' })
    })
  })

  describe('@metadata validation - reject invalid types', function () {
    beforeEach(async function () {
      agent.dbName = `test_metadata_validation_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Metadata Validation', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should reject @metadata as boolean', async function () {
      const contextWithBooleanMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': true,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithBooleanMetadata],
          fullReplace: true,
        })
        expect.fail('Should have rejected boolean @metadata')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })

    it('should reject @metadata as null', async function () {
      const contextWithNullMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': null,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithNullMetadata],
          fullReplace: true,
        })
        expect.fail('Should have rejected null @metadata')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })

    it('should reject @metadata as integer number', async function () {
      const contextWithNumberMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': 42,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithNumberMetadata],
          fullReplace: true,
        })
        expect.fail('Should have rejected number @metadata')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })

    it('should reject @metadata as float number', async function () {
      const contextWithFloatMetadata = {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@metadata': 2.0,
      }

      try {
        await document.insert(agent, {
          schema: [contextWithFloatMetadata],
          fullReplace: true,
        })
        expect.fail('Should have rejected float @metadata')
      } catch (e) {
        // The test library wraps HTTP errors in Chai assertions
        expect(e.message).to.include('400')
      }
    })
  })

  describe('@metadata in Class Schema', function () {
    beforeEach(async function () {
      agent.dbName = `test_class_metadata_${Date.now()}_${Math.floor(Math.random() * 10000)}`
      await db.create(agent, { label: 'Test Class Metadata', schema: true })
    })

    afterEach(async function () {
      try {
        await db.delete(agent)
      } catch (e) {
        // Ignore cleanup errors
      }
    })

    it('should store and retrieve @metadata with special fields in Class definitions', async function () {
      const classSchema = {
        '@type': 'Class',
        '@id': 'Person',
        '@metadata': {
          '@id': 'person_metadata_id',
          '@type': 'ClassMetadata',
          '@context': { schema: 'http://schema.org/' },
          version: '1.0',
        },
        name: 'xsd:string',
        age: 'xsd:integer',
      }

      await document.insert(agent, {
        schema: classSchema,
      })

      const result = await document.get(agent, {
        query: { graph_type: 'schema', id: 'Person' },
      })

      expect(result.body['@metadata']).to.exist
      expect(result.body['@metadata']['@id']).to.equal('person_metadata_id')
      expect(result.body['@metadata']['@type']).to.equal('ClassMetadata')
      expect(result.body['@metadata']['@context']).to.deep.equal({ schema: 'http://schema.org/' })
      expect(result.body['@metadata'].version).to.equal('1.0')
    })
  })
})
