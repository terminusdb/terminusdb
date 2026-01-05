const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON @@id and @@type - Pure JSON Storage (No Expansion)', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('@@id stored as-is (no expansion)', function () {
    const schema = [
      {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        jsondoc: 'https://example.com/jsondoc/',
      },
      {
        '@id': 'JSONIdExpansionTest',
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        name: 'xsd:string',
        data: 'sys:JSON',
      },
    ]

    before(async function () {
      await db.create(agent, { label: 'Test @@id pure JSON storage' })
      await document.insert(agent, { schema, fullReplace: true })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should preserve plain string @@id as-is', async function () {
      // sys:JSON stores @@id as pure JSON - no expansion
      const originalData = {
        '@id': 'mydoc',
        name: 'Plain ID Test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONIdExpansionTest',
          name: 'plain-id-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Should preserve as-is (no @base expansion)
      expect(retrieved.body.data['@id']).to.equal('mydoc')
    })

    it('should preserve prefixed @@id as-is', async function () {
      // sys:JSON stores prefixed values as-is - no expansion
      const originalData = {
        '@id': 'jsondoc:mydoc',
        name: 'Prefixed ID Test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONIdExpansionTest',
          name: 'prefixed-id-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Should preserve as-is (no prefix expansion)
      expect(retrieved.body.data['@id']).to.equal('jsondoc:mydoc')
    })

    it('should preserve full HTTP IRIs in @@id', async function () {
      const originalData = {
        '@id': 'https://example.com/resource/123',
        name: 'Full IRI Test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONIdExpansionTest',
          name: 'full-iri-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Full HTTP(S) IRIs should be preserved
      expect(retrieved.body.data['@id']).to.equal('https://example.com/resource/123')
    })

    it('should preserve path-like @@id strings as-is', async function () {
      // sys:JSON preserves complex paths without interpretation
      const originalData = {
        '@id': 'JSONDocument/mydoc',
        name: 'User Path Test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONIdExpansionTest',
          name: 'user-path-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Should preserve as-is (no @base mounting)
      expect(retrieved.body.data['@id']).to.equal('JSONDocument/mydoc')
    })
  })

  describe('@@type stored as-is (no expansion)', function () {
    const schema = [
      {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        jsondoc: 'https://example.com/jsondoc/',
      },
      {
        '@id': 'JSONTypeExpansionTest',
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        name: 'xsd:string',
        data: 'sys:JSON',
      },
    ]

    before(async function () {
      await db.create(agent, { label: 'Test @@type prefix expansion' })
      await document.insert(agent, { schema, fullReplace: true })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should preserve prefixed @@type as-is', async function () {
      // @@type values are stored as-is without expansion
      const originalData = {
        '@type': 'xsd:string',
        value: 'test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTypeExpansionTest',
          name: 'prefixed-type-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Should preserve xsd:string as-is (no expansion)
      expect(retrieved.body.data['@type']).to.equal('xsd:string')
    })

    it('should preserve full HTTP IRIs in @@type', async function () {
      const originalData = {
        '@type': 'https://schema.org/Person',
        name: 'Full Type IRI Test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTypeExpansionTest',
          name: 'full-type-iri-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Full HTTP(S) IRIs should be preserved
      expect(retrieved.body.data['@type']).to.equal('https://schema.org/Person')
    })

    it('should preserve @@type array as-is', async function () {
      // @@type arrays are stored as-is without expansion
      const originalData = {
        '@type': ['jsondoc:Document', 'https://schema.org/Text'],
        value: 'multi-type test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTypeExpansionTest',
          name: 'array-type-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Should preserve array as-is (no expansion)
      expect(retrieved.body.data['@type']).to.be.an('array')
      expect(retrieved.body.data['@type']).to.have.lengthOf(2)
      expect(retrieved.body.data['@type'][0]).to.equal('jsondoc:Document')
      expect(retrieved.body.data['@type'][1]).to.equal('https://schema.org/Text')
    })

    it('should preserve plain string @@type as-is', async function () {
      // Plain strings are stored as-is without expansion
      const originalData = {
        '@type': 'Person',
        name: 'Plain Type Test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTypeExpansionTest',
          name: 'plain-type-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Plain string preserved as-is (no @base expansion)
      expect(retrieved.body.data['@type']).to.equal('Person')
    })
  })

  describe('@@id and @@type in nested objects - pure JSON', function () {
    const schema = [
      {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        jsondoc: 'https://example.com/jsondoc/',
      },
      {
        '@id': 'JSONNestedExpansionTest',
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        name: 'xsd:string',
        data: 'sys:JSON',
      },
    ]

    before(async function () {
      await db.create(agent, { label: 'Test nested pure JSON' })
      await document.insert(agent, { schema, fullReplace: true })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should preserve both @@id and @@type as-is in nested objects', async function () {
      const originalData = {
        users: [
          {
            '@id': 'user1',
            '@type': 'jsondoc:user1',
            name: 'Alice',
          },
          {
            '@id': 'https://example.com/user2',
            '@type': 'https://schema.org/Person',
            name: 'Bob',
          },
        ],
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONNestedExpansionTest',
          name: 'nested-expansion-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // First user: both @@id and @@type preserved as-is
      expect(retrieved.body.data.users[0]['@id']).to.equal('user1')
      expect(retrieved.body.data.users[0]['@type']).to.equal('jsondoc:user1')

      // Second user: both @@id and @@type preserved as-is
      expect(retrieved.body.data.users[1]['@id']).to.equal('https://example.com/user2')
      expect(retrieved.body.data.users[1]['@type']).to.equal('https://schema.org/Person')
    })
  })

  describe('Backward compatibility - non-string @@id and @@type', function () {
    before(async function () {
      await db.create(agent, { label: 'Test backward compat', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONBackwardCompatTest',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          name: 'xsd:string',
          data: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should preserve non-string @@id values (null, number, etc.)', async function () {
      // Non-string values should not be expanded
      const originalData = {
        '@id': 12345,
        name: 'Numeric ID',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONBackwardCompatTest',
          name: 'numeric-id-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Numeric values should be preserved as-is
      expect(retrieved.body.data['@id']).to.equal(12345)
    })

    it('should preserve null @@id and @@type values', async function () {
      const originalData = {
        '@id': null,
        '@type': null,
        name: 'Null values',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONBackwardCompatTest',
          name: 'null-values-test',
          data: originalData,
        },
        author: 'test_author',
      })

      const docId = result.body[0]
      const retrieved = await document.get(agent, { query: { id: docId } })

      // Null values should be preserved
      expect(retrieved.body.data['@id']).to.be.null
      expect(retrieved.body.data['@type']).to.be.null
    })
  })
})
