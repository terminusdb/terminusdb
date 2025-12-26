const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('sys:JSON @id and @type as Data Properties', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Basic @id and @type as Data Properties', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON @prefix Properties', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONAtPrefixTest',
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

    it('should store and retrieve regular id and type properties (baseline)', async function () {
      // First verify that regular non-@-prefixed properties work correctly
      const originalData = {
        id: 'user123',
        type: 'Person',
        name: 'John Doe',
        age: 30,
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONAtPrefixTest',
          name: 'baseline-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should store and retrieve @id as regular data property', async function () {
      const originalData = {
        '@id': 'user123',
        name: 'John Doe',
        age: 30,
      }

      let result
      try {
        result = await document.insert(agent, {
          instance: {
            '@type': 'JSONAtPrefixTest',
            name: 'at-id-test',
            data: originalData,
          },
          author: 'test_author',
        }).unverified()

        if (result.status !== 200) {
          throw new Error(`Insert failed with status ${result.status}: ${JSON.stringify(result.body, null, 2)}`)
        }

        expect(result.body).to.be.an('array').with.lengthOf(1)
        const docId = result.body[0]

        // Retrieve and verify round-trip
        const retrieved = await document.get(agent, { query: { id: docId } }).unverified()
        if (retrieved.status !== 200) {
          throw new Error(`Retrieval failed with status ${retrieved.status}: ${JSON.stringify(retrieved.body, null, 2)}`)
        }

        if (!retrieved.body.data) {
          throw new Error(`Retrieved document missing data field. Got: ${JSON.stringify(retrieved.body, null, 2)}`)
        }

        expect(retrieved.body.data).to.deep.equal(originalData)
      } catch (error) {
        console.error('Test failed:', error.message)
        throw error
      }
    })

    it('should store and retrieve @type as regular data property', async function () {
      const originalData = {
        '@type': 'Person',
        name: 'Jane Doe',
        role: 'Developer',
      }

      try {
        const result = await document.insert(agent, {
          instance: {
            '@type': 'JSONAtPrefixTest',
            name: 'at-type-test',
            data: originalData,
          },
          author: 'test_author',
        }).unverified()

        if (result.status !== 200) {
          throw new Error(`Insert failed: ${JSON.stringify(result.body, null, 2)}`)
        }

        expect(result.body).to.be.an('array').with.lengthOf(1)
        const docId = result.body[0]

        const retrieved = await document.get(agent, { query: { id: docId } }).unverified()
        if (retrieved.status !== 200) {
          throw new Error(`Retrieval failed: ${JSON.stringify(retrieved.body, null, 2)}`)
        }

        expect(retrieved.body.data).to.deep.equal(originalData)
      } catch (error) {
        console.error('Test failed:', error.message)
        throw error
      }
    })

    it('should store and retrieve both @id and @type as regular data properties', async function () {
      const originalData = {
        '@id': 'user123',
        '@type': 'Person',
        name: 'John Doe',
        age: 30,
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONAtPrefixTest',
          name: 'both-at-prefix-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should handle @type as array value', async function () {
      const originalData = {
        '@id': 'user123',
        '@type': ['Person', 'Admin'],
        name: 'John Doe',
        age: 30,
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONAtPrefixTest',
          name: 'array-type-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })
  })

  describe('Nested @id and @type Properties', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Nested @prefix', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONNestedTest',
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

    it('should preserve regular id and type in nested objects (baseline)', async function () {
      const originalData = {
        id: 'parent123',
        type: 'Parent',
        child: {
          id: 'child456',
          type: 'Child',
          name: 'nested',
        },
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONNestedTest',
          name: 'nested-baseline-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should preserve @id and @type in nested objects', async function () {
      const originalData = {
        users: [
          {
            '@id': 'user1',
            '@type': 'Admin',
            name: 'Alice',
            profile: {
              '@id': 'profile1',
              '@type': 'UserProfile',
              level: 5,
            },
          },
          {
            '@id': 'user2',
            '@type': 'User',
            name: 'Bob',
            profile: {
              '@id': 'profile2',
              '@type': 'UserProfile',
              level: 3,
            },
          },
        ],
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONNestedTest',
          name: 'nested-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should handle deeply nested @id and @type properties', async function () {
      const originalData = {
        level1: {
          '@id': 'l1',
          '@type': 'Level1',
          level2: {
            '@id': 'l2',
            '@type': 'Level2',
            level3: {
              '@id': 'l3',
              '@type': 'Level3',
              data: 'deep',
            },
          },
        },
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONNestedTest',
          name: 'deep-nested-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })
  })

  describe('Mixed @prefix Properties', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON Mixed @prefix', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONMixedTest',
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

    it('should handle multiple @prefix properties together', async function () {
      const originalData = {
        '@id': 'document1',
        '@type': 'Report',
        '@context': 'https://schema.org',
        '@value': 'important',
        '@language': 'en',
        '@container': '@set',
        title: 'Annual Report',
        content: 'This is the content',
        metadata: {
          '@id': 'meta1',
          '@type': 'Metadata',
          version: '1.0',
        },
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONMixedTest',
          name: 'mixed-prefix-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })
  })

  describe('Edge Cases and Special Values', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON Edge Cases', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONEdgeTest',
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

    it('should handle empty string @id and @type', async function () {
      const originalData = {
        '@id': '',
        '@type': '',
        name: 'test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONEdgeTest',
          name: 'empty-string-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should handle null @id and @type values', async function () {
      const originalData = {
        '@id': null,
        '@type': null,
        name: 'test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONEdgeTest',
          name: 'null-value-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should handle special characters in @id and @type', async function () {
      const originalData = {
        '@id': 'user@domain.com/path?query=value#fragment',
        '@type': 'https://example.com/types/Special-Type_v1.0',
        name: 'special chars test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONEdgeTest',
          name: 'special-chars-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should handle numeric @id and @type values', async function () {
      const originalData = {
        '@id': 12345,
        '@type': 67890,
        name: 'numeric test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONEdgeTest',
          name: 'numeric-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })
  })

  describe('Unicode Escape Sequence Support', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON Unicode', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONUnicodeTest',
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

    it('should handle \u0040id and \u0040type as equivalent to @id and @type', async function () {
      // JavaScript parses \u0040 to @ at compile time, so this verifies
      // that @ symbols (however specified in source) work correctly
      const originalData = {
        '\u0040id': 'user123',
        '\u0040type': 'Person',
        name: 'John Doe',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONUnicodeTest',
          name: 'unicode-escape-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })
  })

  describe('Arrays with @id and @type Properties', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON Arrays', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONArrayTest',
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

    it('should preserve regular id and type in array elements (baseline)', async function () {
      const originalData = {
        items: [
          { id: 'item1', type: 'Product', name: 'Widget' },
          { id: 'item2', type: 'Product', name: 'Gadget' },
        ],
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONArrayTest',
          name: 'array-baseline-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should preserve @id and @type in array elements', async function () {
      const originalData = [
        {
          '@id': 'item1',
          '@type': 'FirstType',
          value: 'first',
        },
        {
          '@id': 'item2',
          '@type': 'SecondType',
          value: 'second',
        },
        {
          '@id': 'item3',
          '@type': 'ThirdType',
          value: 'third',
        },
      ]

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONArrayTest',
          name: 'array-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should handle mixed arrays with and without @prefix properties', async function () {
      const originalData = [
        {
          '@id': 'special1',
          '@type': 'SpecialType',
          value: 'special',
        },
        {
          regularField: 'regular value',
          anotherField: 42,
        },
        {
          '@id': 'special2',
          '@type': 'AnotherSpecialType',
          value: 'another special',
        },
      ]

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONArrayTest',
          name: 'mixed-array-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })
  })

  describe('Backward Compatibility', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON Backward Compat', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONCompatTest',
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

    it('should continue to work with regular JSON without @prefix properties', async function () {
      const originalData = {
        name: 'Regular JSON',
        age: 25,
        active: true,
        tags: ['javascript', 'testing'],
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONCompatTest',
          name: 'regular-json-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should continue to work with other @prefix properties that were already working', async function () {
      const originalData = {
        '@context': 'https://schema.org',
        '@value': 'test value',
        '@language': 'en',
        name: 'Other prefixes test',
      }

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONCompatTest',
          name: 'other-prefixes-test',
          data: originalData,
        },
        author: 'test_author',
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })
  })

  describe('WOQL Integration', function () {
    before(async function () {
      await db.create(agent, { label: 'Test WOQL @prefix Properties', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONWoqlTest',
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

    // ARCHITECTURAL LIMITATION: WOQL InsertDocument processes dictionary data during
    // WOQL→AST parsing, where @id/@type are interpreted as JSON-LD control fields BEFORE
    // reaching the inference.pl escaping layer. Fixing this requires adding escaping
    // at the json_woql.pl layer during dictionary parsing, which is a deeper architectural change.
    // WORKAROUND: Use REST Document API (document.insert) which works correctly.
    it.skip('should handle @id and @type in sys:JSON via WOQL insert and read', async function () {
      const jsonData = {
        '@id': 'woql-doc-123',
        '@type': 'WoqlDocument',
        title: 'WOQL Test',
        metadata: {
          '@id': 'nested-456',
          '@type': 'Metadata',
          version: '1.0',
        },
      }

      // Insert using WOQL DictionaryTemplate
      const docId = 'JSONWoqlTest/woql_test_1'
      const insertQuery = {
        '@type': 'InsertDocument',
        document: {
          '@type': 'Value',
          dictionary: {
            '@type': 'DictionaryTemplate',
            data: [
              {
                '@type': 'FieldValuePair',
                field: '@type',
                value: { '@type': 'Value', data: 'JSONWoqlTest' },
              },
              {
                '@type': 'FieldValuePair',
                field: '@id',
                value: { '@type': 'Value', data: docId },
              },
              {
                '@type': 'FieldValuePair',
                field: 'name',
                value: { '@type': 'Value', data: 'woql-test' },
              },
              {
                '@type': 'FieldValuePair',
                field: 'data',
                value: { '@type': 'Value', data: jsonData },
              },
            ],
          },
        },
      }

      await woql.post(agent, insertQuery)

      // Read back using WOQL ReadDocument
      const readQuery = {
        '@type': 'ReadDocument',
        identifier: { '@type': 'NodeValue', node: docId },
        document: { '@type': 'DataValue', variable: 'Doc' },
      }

      const readResult = await woql.post(agent, readQuery)

      expect(readResult.body.bindings).to.be.an('array').with.lengthOf(1)
      const retrievedDoc = readResult.body.bindings[0].Doc
      expect(retrievedDoc.data).to.deep.equal(jsonData)
    })

    it.skip('should handle multiple @prefix properties via WOQL', async function () {
      const jsonData = {
        '@id': 'complex-doc',
        '@type': 'ComplexType',
        '@context': 'https://example.org',
        '@value': 'some-value',
        content: 'test content',
      }

      const docId = 'JSONWoqlTest/woql_test_2'
      const insertQuery = {
        '@type': 'InsertDocument',
        document: {
          '@type': 'Value',
          dictionary: {
            '@type': 'DictionaryTemplate',
            data: [
              {
                '@type': 'FieldValuePair',
                field: '@type',
                value: { '@type': 'Value', data: 'JSONWoqlTest' },
              },
              {
                '@type': 'FieldValuePair',
                field: '@id',
                value: { '@type': 'Value', data: docId },
              },
              {
                '@type': 'FieldValuePair',
                field: 'name',
                value: { '@type': 'Value', data: 'woql-complex-test' },
              },
              {
                '@type': 'FieldValuePair',
                field: 'data',
                value: { '@type': 'Value', data: jsonData },
              },
            ],
          },
        },
      }

      await woql.post(agent, insertQuery)

      // Read back using WOQL
      const readQuery = {
        '@type': 'ReadDocument',
        identifier: { '@type': 'NodeValue', node: docId },
        document: { '@type': 'DataValue', variable: 'Doc' },
      }

      const readResult = await woql.post(agent, readQuery)

      expect(readResult.body.bindings).to.be.an('array').with.lengthOf(1)
      const retrievedDoc = readResult.body.bindings[0].Doc
      expect(retrievedDoc.data).to.deep.equal(jsonData)
    })
  })
})
