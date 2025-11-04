const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('sys:JSON document cleanup verification', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Verify complete triple cleanup after document deletion', function () {
    beforeEach(async function () {
      await db.create(agent, { label: 'Test JSON Cleanup', schema: true })

      const schema = {
        '@type': 'Class',
        '@key': {
          '@type': 'Random',
        },
        '@id': 'TestDoc',
        data: {
          '@class': 'sys:JSON',
          '@type': 'Optional',
        },
      }

      const result = await document.insert(agent, { schema }).unverified()
      expect(result.status).to.equal(200)
    })

    afterEach(async function () {
      await db.delete(agent)
    })

    /**
     * Helper function to get all triples in the graph
     */
    async function getAllTriples () {
      const query = {
        '@type': 'Triple',
        subject: {
          '@type': 'NodeValue',
          variable: '1_subject',
        },
        predicate: {
          '@type': 'NodeValue',
          variable: '2_predicate',
        },
        object: {
          '@type': 'Value',
          variable: '3_object',
        },
      }

      const result = await woql.post(agent, query).unverified()
      return result.body.bindings || []
    }

    /**
     * Helper to filter out schema and system triples
     */
    function filterSystemTriples (bindings) {
      return bindings.filter((binding) => {
        const subject = binding['1_subject']
        const predicate = binding['2_predicate']

        // Filter out schema definitions and system triples
        if (subject === 'TestDoc') return false
        if (predicate === 'rdf:type' && binding['3_object'] === 'sys:Class') return false
        if (predicate === 'sys:key') return false
        if (predicate === 'sys:documentation') return false
        if (subject.startsWith('TestDoc/')) return false // schema properties

        return true
      })
    }

    it('should remove all triples after deleting document with nested objects and arrays', async function () {
      const complexDoc = {
        '@type': 'TestDoc',
        data: {
          name: 'Root',
          metadata: {
            created: '2024-01-01',
            tags: ['important', 'test', 'nested'],
            counts: {
              views: 42,
              likes: 17,
            },
          },
          items: [
            { id: 1, name: 'Item One', active: true },
            { id: 2, name: 'Item Two', active: false },
            { id: 3, name: 'Item Three', active: true, nested: { level: 2 } },
          ],
          emptyArray: [],
          emptyObject: {},
          nullValue: null,
          boolValue: true,
        },
      }

      // Insert document
      const insertResult = await document.insert(agent, { instance: complexDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      // Verify triples exist (should have many from the nested structure)
      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      // Delete document
      const deleteResult = await document.delete(agent, { query: { id: docId } })
      expect(deleteResult.status).to.be.oneOf([200, 204])

      // Verify ALL triples are removed
      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(
        0,
        `Found ${afterTriples.length} remaining triples: ${JSON.stringify(afterTriples, null, 2)}`,
      )
    })

    it('should remove all triples after deleting document with arrays of objects', async function () {
      const arrayDoc = {
        '@type': 'TestDoc',
        data: {
          users: [
            { name: 'Alice', age: 30, roles: ['admin', 'user'] },
            { name: 'Bob', age: 25, roles: ['user'] },
            { name: 'Charlie', age: 35, roles: ['moderator', 'user', 'guest'] },
          ],
          matrix: [
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9],
          ],
        },
      }

      const insertResult = await document.insert(agent, { instance: arrayDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(
        0,
        `Found ${afterTriples.length} remaining triples after deletion`,
      )
    })

    it('should remove all triples after deleting document with objects containing arrays', async function () {
      const objectDoc = {
        '@type': 'TestDoc',
        data: {
          config: {
            servers: ['server1.example.com', 'server2.example.com', 'server3.example.com'],
            ports: [8080, 8081, 8082, 8083],
            settings: {
              timeout: 5000,
              retries: 3,
              endpoints: ['/api/v1', '/api/v2', '/graphql'],
            },
          },
        },
      }

      const insertResult = await document.insert(agent, { instance: objectDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, `Remaining triples: ${JSON.stringify(afterTriples)}`)
    })

    it('should handle empty structures correctly', async function () {
      const emptyDoc = {
        '@type': 'TestDoc',
        data: {
          emptyArray: [],
          emptyObject: {},
          emptyNested: {
            level1: {
              emptyArray: [],
              level2: {
                emptyObject: {},
              },
            },
          },
          arrayWithEmpty: [{}, { empty: [] }, { nested: { empty: {} } }],
        },
      }

      const insertResult = await document.insert(agent, { instance: emptyDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Empty structures left triples')
    })

    it('should handle mixed value types correctly', async function () {
      const mixedDoc = {
        '@type': 'TestDoc',
        data: {
          string: 'test string',
          number: 42.5,
          integer: 100,
          boolean: true,
          nullValue: null,
          array: ['string', 123, true, null, { nested: 'value' }],
          object: {
            str: 'value',
            num: 3.14,
            bool: false,
            nil: null,
          },
        },
      }

      const insertResult = await document.insert(agent, { instance: mixedDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Mixed types left triples')
    })

    it('should cleanup when deleting one of two documents', async function () {
      const doc1 = {
        '@type': 'TestDoc',
        data: {
          id: 'doc1',
          content: { items: [1, 2, 3], nested: { value: 'first' } },
        },
      }

      const doc2 = {
        '@type': 'TestDoc',
        data: {
          id: 'doc2',
          content: { items: [4, 5, 6], nested: { value: 'second' } },
        },
      }

      // Insert both documents
      const result1 = await document.insert(agent, { instance: doc1 })
      expect(result1.status).to.equal(200)
      const docId1 = result1.body[0]

      const result2 = await document.insert(agent, { instance: doc2 })
      expect(result2.status).to.equal(200)
      const docId2 = result2.body[0]

      // Verify both documents created triples
      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      // Delete first document
      await document.delete(agent, { query: { id: docId1 } })

      // Should still have triples from doc2
      const afterFirst = filterSystemTriples(await getAllTriples())
      expect(afterFirst.length).to.be.greaterThan(0, 'Doc2 triples should remain')

      // Delete second document
      await document.delete(agent, { query: { id: docId2 } })

      // Now all should be cleaned up
      const afterBoth = filterSystemTriples(await getAllTriples())
      expect(afterBoth).to.be.an('array').with.lengthOf(0, 'All triples should be removed after deleting both docs')
    })

    it('should cleanup when deleting first of three documents, then the other two', async function () {
      const doc1 = {
        '@type': 'TestDoc',
        data: {
          id: 'first',
          tags: ['alpha', 'beta'],
          nested: { level: 1, data: [{ x: 1 }, { x: 2 }] },
        },
      }

      const doc2 = {
        '@type': 'TestDoc',
        data: {
          id: 'second',
          tags: ['gamma', 'delta'],
          nested: { level: 2, data: [{ y: 3 }, { y: 4 }] },
        },
      }

      const doc3 = {
        '@type': 'TestDoc',
        data: {
          id: 'third',
          tags: ['epsilon', 'zeta'],
          nested: { level: 3, data: [{ z: 5 }, { z: 6 }] },
        },
      }

      // Insert all three documents
      const result1 = await document.insert(agent, { instance: doc1 })
      expect(result1.status).to.equal(200)
      const docId1 = result1.body[0]

      const result2 = await document.insert(agent, { instance: doc2 })
      expect(result2.status).to.equal(200)
      const docId2 = result2.body[0]

      const result3 = await document.insert(agent, { instance: doc3 })
      expect(result3.status).to.equal(200)
      const docId3 = result3.body[0]

      // Verify triples exist
      const initialTriples = filterSystemTriples(await getAllTriples())
      expect(initialTriples.length).to.be.greaterThan(0)

      // Delete first document
      await document.delete(agent, { query: { id: docId1 } })

      const afterFirstDelete = filterSystemTriples(await getAllTriples())
      expect(afterFirstDelete.length).to.be.greaterThan(0, 'Doc2 and Doc3 triples should remain')

      // Delete remaining two documents
      await document.delete(agent, { query: { id: docId2 } })
      await document.delete(agent, { query: { id: docId3 } })

      // Verify complete cleanup
      const finalTriples = filterSystemTriples(await getAllTriples())
      expect(finalTriples).to.be.an('array').with.lengthOf(0, 'All triples should be removed after deleting all docs')
    })

    it('should cleanup deeply nested structures', async function () {
      const deepDoc = {
        '@type': 'TestDoc',
        data: {
          level1: {
            level2: {
              level3: {
                level4: {
                  level5: {
                    data: 'deep value',
                    array: [1, 2, 3, 4, 5],
                    objects: [
                      { a: { b: { c: 'nested' } } },
                      { x: { y: { z: 'another' } } },
                    ],
                  },
                },
              },
            },
          },
        },
      }

      const insertResult = await document.insert(agent, { instance: deepDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Deep nesting left triples')
    })

    it('should cleanup document with large arrays', async function () {
      const largeArrayDoc = {
        '@type': 'TestDoc',
        data: {
          numbers: Array.from({ length: 100 }, (_, i) => i),
          objects: Array.from({ length: 50 }, (_, i) => ({
            id: i,
            name: `Item ${i}`,
            tags: [`tag${i}`, `category${i % 10}`],
          })),
        },
      }

      const insertResult = await document.insert(agent, { instance: largeArrayDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Large arrays left triples')
    })

    it('should cleanup document with complex schema-like structure', async function () {
      const schemaDoc = {
        '@type': 'TestDoc',
        data: {
          schema: {
            type: 'object',
            properties: {
              name: { type: 'string', minLength: 1 },
              age: { type: 'number', minimum: 0 },
              email: { type: 'string', format: 'email' },
              tags: {
                type: 'array',
                items: { type: 'string' },
              },
              address: {
                type: 'object',
                properties: {
                  street: { type: 'string' },
                  city: { type: 'string' },
                  country: { type: 'string' },
                },
                required: ['city', 'country'],
              },
            },
            required: ['name', 'email'],
          },
        },
      }

      const insertResult = await document.insert(agent, { instance: schemaDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Schema-like structure left triples')
    })

    it('should cleanup multiple versions of same document structure', async function () {
      const createDoc = (version) => ({
        '@type': 'TestDoc',
        data: {
          version,
          items: [
            { id: 1 + version * 100, value: `v${version}-1` },
            { id: 2 + version * 100, value: `v${version}-2` },
          ],
          metadata: {
            timestamp: `2024-01-0${version}`,
            nested: { level: version },
          },
        },
      })

      // Insert three versions
      const v1Result = await document.insert(agent, { instance: createDoc(1) })
      expect(v1Result.status).to.equal(200)
      const v1Id = v1Result.body[0]

      const v2Result = await document.insert(agent, { instance: createDoc(2) })
      expect(v2Result.status).to.equal(200)
      const v2Id = v2Result.body[0]

      const v3Result = await document.insert(agent, { instance: createDoc(3) })
      expect(v3Result.status).to.equal(200)
      const v3Id = v3Result.body[0]

      // Verify triples exist
      const initialTriples = filterSystemTriples(await getAllTriples())
      expect(initialTriples.length).to.be.greaterThan(0)

      // Delete version 1
      await document.delete(agent, { query: { id: v1Id } })

      const afterV1 = filterSystemTriples(await getAllTriples())
      expect(afterV1.length).to.be.greaterThan(0, 'V2 and V3 should remain')

      // Delete versions 2 and 3
      await document.delete(agent, { query: { id: v2Id } })
      await document.delete(agent, { query: { id: v3Id } })

      const finalTriples = filterSystemTriples(await getAllTriples())
      expect(finalTriples).to.be.an('array').with.lengthOf(0, 'All version triples should be removed')
    })

    it('should cleanup document with explicit @id (not random key)', async function () {
      const explicitDoc = {
        '@id': 'TestDoc/explicit-id-123',
        '@type': 'TestDoc',
        data: {
          content: 'Document with explicit ID',
          nested: { values: [1, 2, 3] },
        },
      }

      const insertResult = await document.insert(agent, { instance: explicitDoc })
      expect(insertResult.status).to.equal(200)

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: 'TestDoc/explicit-id-123' } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Explicit ID doc left triples')
    })

    it('should cleanup document with special characters in JSON keys', async function () {
      const specialDoc = {
        '@type': 'TestDoc',
        data: {
          'key with spaces': 'value1',
          'key-with-dashes': 'value2',
          'key.with.dots': 'value3',
          key_with_underscores: 'value4',
          CamelCaseKey: 'value5',
          'key@with#special$chars%': 'value6',
          数字键: '中文值', // Unicode keys and values
          'emoji🔥key': 'emoji😀value',
        },
      }

      const insertResult = await document.insert(agent, { instance: specialDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Special char keys left triples')
    })

    it('should cleanup arrays with duplicate primitive values', async function () {
      const duplicateDoc = {
        '@type': 'TestDoc',
        data: {
          numbers: [1, 1, 1, 2, 2, 2, 3, 3, 3],
          strings: ['a', 'a', 'b', 'b', 'c', 'c'],
          bools: [true, true, false, false, true],
          nulls: [null, null, null],
          mixed: [1, 1, 'a', 'a', null, null, true, true],
        },
      }

      const insertResult = await document.insert(agent, { instance: duplicateDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Duplicate values left triples')
    })

    it('should cleanup nested arrays (multi-dimensional)', async function () {
      const nestedArrayDoc = {
        '@type': 'TestDoc',
        data: {
          matrix2d: [
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9],
          ],
          matrix3d: [
            [
              [1, 2],
              [3, 4],
            ],
            [
              [5, 6],
              [7, 8],
            ],
          ],
          jagged: [[1], [2, 3], [4, 5, 6], [7, 8, 9, 10]],
        },
      }

      const insertResult = await document.insert(agent, { instance: nestedArrayDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Nested arrays left triples')
    })

    it('should cleanup arrays with mixed nulls and values', async function () {
      const nullDoc = {
        '@type': 'TestDoc',
        data: {
          sparseArray: [1, null, 2, null, null, 3],
          allNulls: [null, null, null],
          objectsWithNulls: [{ a: 1, b: null }, { a: null, b: 2 }, { a: null, b: null }],
        },
      }

      const insertResult = await document.insert(agent, { instance: nullDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Null values left triples')
    })

    it('should cleanup unicode and emoji content', async function () {
      const unicodeDoc = {
        '@type': 'TestDoc',
        data: {
          chinese: '你好世界',
          arabic: 'مرحبا بالعالم',
          emoji: '🚀🔥💯👍',
          mixed: 'Hello 世界 🌍',
          rtl: 'שלום עולם',
          symbols: '©️®️™️€£¥',
        },
      }

      const insertResult = await document.insert(agent, { instance: unicodeDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Unicode content left triples')
    })

    it('should cleanup numeric edge cases', async function () {
      const numericDoc = {
        '@type': 'TestDoc',
        data: {
          zero: 0,
          negativeZero: -0,
          maxSafeInt: 9007199254740991,
          minSafeInt: -9007199254740991,
          verySmall: 0.000000000001,
          veryLarge: 999999999999999,
          scientific: 1.23e10,
          negative: -123.456,
        },
      }

      const insertResult = await document.insert(agent, { instance: numericDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Numeric edge cases left triples')
    })

    it('should cleanup very long strings', async function () {
      const longStringDoc = {
        '@type': 'TestDoc',
        data: {
          short: 'x',
          medium: 'x'.repeat(100),
          long: 'x'.repeat(1000),
          veryLong: 'x'.repeat(10000),
          longWithUnicode: '🔥'.repeat(1000),
        },
      }

      const insertResult = await document.insert(agent, { instance: longStringDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Long strings left triples')
    })

    it('should cleanup document updated then deleted', async function () {
      const doc = {
        '@type': 'TestDoc',
        data: {
          version: 1,
          content: { items: [1, 2, 3] },
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      // Get and update
      const getResult = await document.get(agent, { query: { id: docId } })
      const fullDoc = getResult.body
      fullDoc.data = { version: 2, content: { items: [4, 5, 6] }, newField: 'added' }

      await document.replace(agent, { instance: fullDoc })

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      // Delete - should cleanup both old and new versions
      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Updated doc left triples')
    })

    it('should cleanup objects with every JSON type as siblings', async function () {
      const allTypesDoc = {
        '@type': 'TestDoc',
        data: {
          stringField: 'text',
          numberField: 42,
          floatField: 3.14,
          boolTrue: true,
          boolFalse: false,
          nullField: null,
          arrayField: [1, 2, 3],
          objectField: { nested: 'value' },
          emptyArray: [],
          emptyObject: {},
          nestedArray: [[1, 2], [3, 4]],
          nestedObject: { level1: { level2: { level3: 'deep' } } },
        },
      }

      const insertResult = await document.insert(agent, { instance: allTypesDoc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'All types doc left triples')
    })

    it('should cleanup when document shares JSON with another, then gets updated and deleted', async function () {
      const sharedContent = { items: [1, 2, 3], config: { timeout: 30 } }

      const doc1 = {
        '@type': 'TestDoc',
        data: sharedContent,
      }

      const doc2 = {
        '@type': 'TestDoc',
        data: sharedContent, // Same shared content
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Update doc1 to different content (orphaning shared ref)
      const get1 = await document.get(agent, { query: { id: doc1Id } })
      get1.body.data = { different: 'content', new: [4, 5, 6] }
      await document.replace(agent, { instance: get1.body })

      // Delete doc1 (should cleanup new content, shared content stays for doc2)
      await document.delete(agent, { query: { id: doc1Id } })

      // Verify doc2 still intact
      const get2 = await document.get(agent, { query: { id: doc2Id } })
      expect(get2.status).to.equal(200)
      expect(get2.body.data).to.deep.equal(sharedContent)

      // Delete doc2 (now shared content should be cleaned up)
      await document.delete(agent, { query: { id: doc2Id } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Shared-then-updated left triples')
    })

    it('should cleanup multiple sequential updates before deletion', async function () {
      const doc = {
        '@type': 'TestDoc',
        data: { version: 1, content: [1, 2, 3] },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      // Multiple updates creating orphaned intermediate states
      for (let i = 2; i <= 5; i++) {
        const getDoc = await document.get(agent, { query: { id: docId } })
        getDoc.body.data = { version: i, content: Array.from({ length: i }, (_, j) => j) }
        await document.replace(agent, { instance: getDoc.body })
      }

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      // Delete should cleanup all versions
      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Multiple updates left triples')
    })

    it('should cleanup when same JSON value appears in multiple properties', async function () {
      const sharedValue = { nested: { deep: 'value' } }

      const doc = {
        '@type': 'TestDoc',
        data: {
          prop1: sharedValue,
          prop2: sharedValue, // Same object reference
          prop3: {
            inner: sharedValue, // Nested reference to same object
          },
          prop4: [sharedValue, sharedValue], // Array of same object
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Multi-property sharing left triples')
    })

    it('should cleanup when deleting in reverse order of insertion', async function () {
      const docIds = []

      // Insert 5 documents with increasingly complex nested data
      for (let i = 1; i <= 5; i++) {
        const doc = {
          '@type': 'TestDoc',
          data: {
            id: i,
            nested: {
              level: i,
              data: Array.from({ length: i * 10 }, (_, j) => ({ index: j, value: j * i })),
            },
          },
        }

        const insertResult = await document.insert(agent, { instance: doc })
        expect(insertResult.status).to.equal(200)
        docIds.push(insertResult.body[0])
      }

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      // Delete in reverse order (LIFO)
      for (let i = docIds.length - 1; i >= 0; i--) {
        await document.delete(agent, { query: { id: docIds[i] } })
      }

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Reverse order deletion left triples')
    })

    it('should cleanup very similar but distinct JSON structures', async function () {
      // Test structures that might have similar hashes but are distinct
      const doc1 = {
        '@type': 'TestDoc',
        data: { a: 1, b: 2, c: 3, d: 4 },
      }

      const doc2 = {
        '@type': 'TestDoc',
        data: { a: 1, b: 2, c: 3, d: 5 }, // Only last value differs
      }

      const doc3 = {
        '@type': 'TestDoc',
        data: { a: 1, b: 2, c: 4, d: 4 }, // Middle value differs
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      const insert3 = await document.insert(agent, { instance: doc3 })
      expect(insert3.status).to.equal(200)
      const doc3Id = insert3.body[0]

      await document.delete(agent, { query: { id: doc1Id } })
      await document.delete(agent, { query: { id: doc2Id } })
      await document.delete(agent, { query: { id: doc3Id } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Similar structures left triples')
    })

    it('should cleanup arrays with shared tails (cons-style sharing)', async function () {
      // In content-addressed systems, [2,3,4] might be a tail of [1,2,3,4]
      const doc1 = {
        '@type': 'TestDoc',
        data: { array: [1, 2, 3, 4, 5] },
      }

      const doc2 = {
        '@type': 'TestDoc',
        data: { array: [2, 3, 4, 5] }, // Could share tail with doc1
      }

      const doc3 = {
        '@type': 'TestDoc',
        data: { array: [3, 4, 5] }, // Could share tail with doc2
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      const insert3 = await document.insert(agent, { instance: doc3 })
      expect(insert3.status).to.equal(200)
      const doc3Id = insert3.body[0]

      // Delete in middle-first order to stress ref counting
      await document.delete(agent, { query: { id: doc2Id } })
      await document.delete(agent, { query: { id: doc1Id } })
      await document.delete(agent, { query: { id: doc3Id } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Shared tails left triples')
    })

    it('should cleanup when property keys are reused with different values', async function () {
      const doc = {
        '@type': 'TestDoc',
        data: {
          config: { setting1: 'a', setting2: 'b', setting3: 'c' },
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      // Update to same keys, different values
      const getDoc = await document.get(agent, { query: { id: docId } })
      getDoc.body.data = {
        config: { setting1: 'x', setting2: 'y', setting3: 'z' }, // Same keys, different values
      }
      await document.replace(agent, { instance: getDoc.body })

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Reused keys left triples')
    })

    it('should cleanup deeply nested identical substructures', async function () {
      const repeatedUnit = { x: 1, y: 2, z: [true, false] }

      const doc = {
        '@type': 'TestDoc',
        data: {
          level1: {
            a: repeatedUnit,
            b: repeatedUnit,
            nested: {
              level2: {
                c: repeatedUnit,
                d: repeatedUnit,
                deeper: {
                  level3: {
                    e: repeatedUnit,
                    f: repeatedUnit,
                  },
                },
              },
            },
          },
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Repeated substructures left triples')
    })

    it('should cleanup mixed arrays and objects with overlapping content', async function () {
      const sharedArray = [10, 20, 30]
      const sharedObject = { key: 'value', num: 42 }

      const doc = {
        '@type': 'TestDoc',
        data: {
          arr1: sharedArray,
          arr2: sharedArray,
          obj1: sharedObject,
          obj2: sharedObject,
          mixed: [sharedArray, sharedObject, sharedArray, sharedObject],
          nested: {
            arrays: [sharedArray, sharedArray],
            objects: [sharedObject, sharedObject],
          },
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Mixed overlapping content left triples')
    })

    it('should cleanup when documents share partial overlapping structures', async function () {
      const commonPart = { shared: 'data', values: [1, 2, 3] }

      const doc1 = {
        '@type': 'TestDoc',
        data: {
          unique1: 'only in doc1',
          common: commonPart,
          nested: { uniqueNested1: 'data' },
        },
      }

      const doc2 = {
        '@type': 'TestDoc',
        data: {
          unique2: 'only in doc2',
          common: commonPart, // Shared
          nested: { uniqueNested2: 'other' },
        },
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Delete both
      await document.delete(agent, { query: { id: doc1Id } })
      await document.delete(agent, { query: { id: doc2Id } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Partial overlap left triples')
    })

    it('should cleanup array with 50 member objects', async function () {
      const doc = {
        '@type': 'TestDoc',
        data: {
          users: Array.from({ length: 50 }, (_, i) => ({
            id: i,
            name: `User ${i}`,
            email: `user${i}@example.com`,
            active: i % 2 === 0,
            roles: [`role${i % 3}`, `role${i % 5}`],
            metadata: {
              created: `2024-${String(i % 12 + 1).padStart(2, '0')}-01`,
              score: i * 10,
            },
          })),
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, '50 member objects left triples')
    })

    it('should cleanup array with 100 member objects', async function () {
      const doc = {
        '@type': 'TestDoc',
        data: {
          products: Array.from({ length: 100 }, (_, i) => ({
            productId: `PROD-${String(i).padStart(4, '0')}`,
            name: `Product ${i}`,
            price: (i * 9.99).toFixed(2),
            inStock: i % 3 !== 0,
            categories: [`cat${i % 10}`, `cat${i % 7}`],
            tags: Array.from({ length: i % 5 + 1 }, (_, j) => `tag${j}`),
          })),
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, '100 member objects left triples')
    })

    it('should cleanup multiple documents each with 30 member objects', async function () {
      const docIds = []

      // Create 3 documents, each with 30 complex objects
      for (let docNum = 0; docNum < 3; docNum++) {
        const doc = {
          '@type': 'TestDoc',
          data: {
            items: Array.from({ length: 30 }, (_, i) => ({
              id: `doc${docNum}-item${i}`,
              data: {
                value: i * docNum,
                nested: {
                  deep: {
                    values: [i, i * 2, i * 3],
                  },
                },
              },
              tags: Array.from({ length: 5 }, (_, j) => `tag${docNum}-${j}`),
            })),
          },
        }

        const insertResult = await document.insert(agent, { instance: doc })
        expect(insertResult.status).to.equal(200)
        docIds.push(insertResult.body[0])
      }

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      // Delete all documents
      for (const docId of docIds) {
        await document.delete(agent, { query: { id: docId } })
      }

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, 'Multiple docs with many objects left triples')
    })

    it('should cleanup array with 75 objects containing nested arrays', async function () {
      const doc = {
        '@type': 'TestDoc',
        data: {
          records: Array.from({ length: 75 }, (_, i) => ({
            recordId: i,
            measurements: Array.from({ length: 10 }, (_, j) => ({
              timestamp: `2024-01-01T${String(i % 24).padStart(2, '0')}:${String(j * 6).padStart(2, '0')}:00Z`,
              value: i * j * 0.1,
              status: j % 3 === 0 ? 'ok' : 'warn',
            })),
            summary: {
              min: i * 0.1,
              max: i * 10,
              avg: i * 5,
              count: 10,
            },
          })),
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, '75 objects with nested arrays left triples')
    })

    it('should cleanup array with 200 simple objects', async function () {
      this.timeout(15000) // Give extra time for large array

      const doc = {
        '@type': 'TestDoc',
        data: {
          events: Array.from({ length: 200 }, (_, i) => ({
            id: i,
            type: `event_type_${i % 10}`,
            timestamp: Date.now() + i * 1000,
            data: { value: i, doubled: i * 2 },
          })),
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, '200 simple objects left triples')
    })

    it('should cleanup document with arrays at multiple nesting levels containing many objects', async function () {
      const doc = {
        '@type': 'TestDoc',
        data: {
          departments: Array.from({ length: 5 }, (_, deptIdx) => ({
            deptId: deptIdx,
            name: `Department ${deptIdx}`,
            teams: Array.from({ length: 10 }, (_, teamIdx) => ({
              teamId: `${deptIdx}-${teamIdx}`,
              name: `Team ${teamIdx}`,
              members: Array.from({ length: 15 }, (_, memberIdx) => ({
                memberId: `${deptIdx}-${teamIdx}-${memberIdx}`,
                name: `Person ${memberIdx}`,
                role: ['lead', 'developer', 'tester'][memberIdx % 3],
              })),
            })),
          })),
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(
        0,
        'Multi-level nested arrays with many objects left triples',
      )
    })

    it('should cleanup when two documents share array with 40 member objects', async function () {
      // Create a shared array with 40 objects
      const sharedArray = Array.from({ length: 40 }, (_, i) => ({
        id: i,
        value: `shared-${i}`,
        props: { a: i, b: i * 2, c: i * 3 },
      }))

      const doc1 = {
        '@type': 'TestDoc',
        data: { shared: sharedArray, unique1: 'doc1-data' },
      }

      const doc2 = {
        '@type': 'TestDoc',
        data: { shared: sharedArray, unique2: 'doc2-data' },
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Delete first doc - shared array should survive
      await document.delete(agent, { query: { id: doc1Id } })

      const afterFirst = filterSystemTriples(await getAllTriples())
      expect(afterFirst.length).to.be.greaterThan(0, 'Should still have doc2 triples')

      // Delete second doc - now everything should be cleaned up
      await document.delete(agent, { query: { id: doc2Id } })

      const afterBoth = filterSystemTriples(await getAllTriples())
      expect(afterBoth).to.be.an('array').with.lengthOf(0, 'Shared 40-object array left triples')
    })

    it('should cleanup array with 60 objects that have identical nested structures', async function () {
      const commonStructure = {
        config: { timeout: 30, retries: 3 },
        metadata: { version: '1.0', stable: true },
      }

      const doc = {
        '@type': 'TestDoc',
        data: {
          nodes: Array.from({ length: 60 }, (_, i) => ({
            nodeId: i,
            common: commonStructure, // Same structure repeated 60 times
            unique: { index: i, label: `node-${i}` },
          })),
        },
      }

      const insertResult = await document.insert(agent, { instance: doc })
      expect(insertResult.status).to.equal(200)
      const docId = insertResult.body[0]

      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)

      await document.delete(agent, { query: { id: docId } })

      const afterTriples = filterSystemTriples(await getAllTriples())
      expect(afterTriples).to.be.an('array').with.lengthOf(0, '60 objects with shared substructure left triples')
    })

    it('should cleanup complex document with nested arrays of objects in bulk delete', async function () {
      // Create a complex structure similar to SBOM but inline (no external dependency)
      // This includes arrays of objects with nested properties, licenses, hashes, etc.
      const complexData = {
        bomFormat: 'CycloneDX',
        specVersion: '1.2',
        version: 1,
        metadata: {
          timestamp: '2024-01-01T00:00:00Z',
          tools: [
            {
              vendor: 'TestVendor',
              name: 'test-tool',
              version: 'v1.0.0',
              hashes: [
                { alg: 'SHA-256', content: 'abc123def456' },
                { alg: 'SHA-512', content: 'xyz789uvw012' },
              ],
            },
          ],
        },
        components: Array.from({ length: 50 }, (_, i) => ({
          'bom-ref': `pkg:golang/github.com/test/package${i}@v1.0.${i}`,
          type: 'library',
          name: `github.com/test/package${i}`,
          version: `v1.0.${i}`,
          scope: 'required',
          hashes: [
            { alg: 'SHA-256', content: `hash${i}abc123` },
          ],
          licenses: [
            {
              license: {
                id: i % 3 === 0 ? 'MIT' : i % 3 === 1 ? 'Apache-2.0' : 'BSD-3-Clause',
                url: `https://spdx.org/licenses/${i % 3 === 0 ? 'MIT' : i % 3 === 1 ? 'Apache-2.0' : 'BSD-3-Clause'}.html`,
              },
            },
          ],
          purl: `pkg:golang/github.com/test/package${i}@v1.0.${i}`,
          externalReferences: [
            {
              url: `https://github.com/test/package${i}`,
              type: 'vcs',
            },
          ],
          properties: [
            { name: `property${i}`, value: `value${i}` },
            { name: 'build', value: 'production' },
          ],
        })),
        dependencies: Array.from({ length: 50 }, (_, i) => ({
          ref: `pkg:golang/github.com/test/package${i}@v1.0.${i}`,
          dependsOn: i > 0 ? [`pkg:golang/github.com/test/package${i - 1}@v1.0.${i - 1}`] : [],
        })),
      }

      // Insert first copy
      const doc1 = {
        '@type': 'TestDoc',
        data: complexData,
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      // Insert second copy (same JSON, so content-addressed storage will deduplicate)
      const doc2 = {
        '@type': 'TestDoc',
        data: complexData,
      }

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Insert third document with PARTIAL overlap (shares some components but not all)
      const partialOverlapData = {
        bomFormat: 'CycloneDX',
        specVersion: '1.2',
        version: 1,
        metadata: complexData.metadata, // Share metadata
        components: [
          complexData.components[0], // Share first component
          complexData.components[1], // Share second component
          {
            // Unique component
            'bom-ref': 'pkg:golang/github.com/unique/package@v1.0.0',
            type: 'library',
            name: 'github.com/unique/package',
            version: 'v1.0.0',
            scope: 'required',
            hashes: [{ alg: 'SHA-256', content: 'unique-hash-xyz' }],
          },
        ],
        dependencies: [complexData.dependencies[0]], // Share first dependency
      }

      const doc3 = {
        '@type': 'TestDoc',
        data: partialOverlapData,
      }

      const insert3 = await document.insert(agent, { instance: doc3 })
      expect(insert3.status).to.equal(200)
      const doc3Id = insert3.body[0]

      // Verify triples exist with 3 documents
      const beforeTriples = filterSystemTriples(await getAllTriples())
      expect(beforeTriples.length).to.be.greaterThan(0)
      // CRITICAL: Delete BOTH doc1 and doc2 in a SINGLE bulk operation
      // Doc3 with partial overlap should remain intact
      const bulkDeleteResponse = await agent
        .delete(`/api/document/${agent.orgName}/${agent.dbName}`)
        .query({ author: 'test_author', message: 'bulk delete doc1 and doc2' })
        .send([doc1Id, doc2Id])

      expect(bulkDeleteResponse.status).to.be.oneOf([200, 204])

      // After deleting doc1 and doc2, doc3 should still exist with its data
      const afterBulkDelete = filterSystemTriples(await getAllTriples())
      expect(afterBulkDelete.length).to.be.greaterThan(0, 'Doc3 should still have triples')

      // Verify doc3 is still accessible and has correct data
      const getDoc3 = await document.get(agent, { query: { id: doc3Id } })
      expect(getDoc3.status).to.equal(200)
      expect(getDoc3.body.data.components).to.have.lengthOf(3)
      expect(getDoc3.body.data.components[0]).to.deep.equal(complexData.components[0])

      // Now delete doc3 - everything should be cleaned up
      await document.delete(agent, { query: { id: doc3Id } })

      // THIS IS THE CRITICAL TEST - Should have ZERO triples remaining after deleting ALL docs
      const afterAll = filterSystemTriples(await getAllTriples())
      expect(afterAll).to.be.an('array').with.lengthOf(
        0,
        `Complex nested structure left ${afterAll.length} triples after deleting all docs`,
      )
    })

    it('should cleanup 2 docs with shared array + 1 doc with partial overlap (minimal)', async function () {
      // Minimal test: just a simple array with 3 numbers
      const simpleData = {
        items: [[1, 2], 3],
      }

      // Doc1 with full array
      const doc1 = {
        '@type': 'TestDoc',
        data: simpleData,
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      // Doc2 with same data (should share content)
      const doc2 = {
        '@type': 'TestDoc',
        data: simpleData,
      }

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Doc3 with partial overlap (shares the [1,2] array object, not the 3)
      const doc3 = {
        '@type': 'TestDoc',
        data: {
          items: [[1, 2]], // Shares the inner [1,2] array object with Doc1/Doc2, should be kept
        },
      }

      const insert3 = await document.insert(agent, { instance: doc3 })
      expect(insert3.status).to.equal(200)
      const doc3Id = insert3.body[0]

      // BULK DELETE doc1 and doc2
      const bulkDeleteResponse = await agent
        .delete(`/api/document/${agent.orgName}/${agent.dbName}`)
        .query({ author: 'test_author', message: 'bulk delete' })
        .send([doc1Id, doc2Id])

      expect(bulkDeleteResponse.status).to.be.oneOf([200, 204])

      // Verify doc3 is still accessible
      const getDoc3 = await document.get(agent, { query: { id: doc3Id } })
      expect(getDoc3.status).to.equal(200)
      expect(getDoc3.body.data.items).to.have.lengthOf(1)
      expect(getDoc3.body.data.items[0]).to.deep.equal([1, 2])

      // Now delete doc3
      await document.delete(agent, { query: { id: doc3Id } })

      const afterAll = filterSystemTriples(await getAllTriples())

      expect(afterAll).to.be.an('array').with.lengthOf(
        0,
        `Minimal case left ${afterAll.length} triples after deleting all docs`,
      )
    })

    it('should bulk delete 2 docs with identical shared content (simplest)', async function () {
      // Simplest possible test: 2 docs with identical content, bulk delete both
      const simpleData = {
        items: [[1, 2], 3],
      }

      // Doc1
      const doc1 = {
        '@type': 'TestDoc',
        data: simpleData,
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]
      // Doc2 with same data (should share content)
      const doc2 = {
        '@type': 'TestDoc',
        data: simpleData,
      }

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Bulk delete both docs
      const bulkDeleteResponse = await agent
        .delete(`/api/document/${agent.orgName}/${agent.dbName}`)
        .query({ author: 'test_author', message: 'bulk delete both' })
        .send([doc1Id, doc2Id])

      expect(bulkDeleteResponse.status).to.be.oneOf([200, 204])

      const afterDelete = filterSystemTriples(await getAllTriples())

      expect(afterDelete).to.be.an('array').with.lengthOf(
        0,
        `Simplest case: bulk delete 2 docs with shared content left ${afterDelete.length} triples`,
      )
    })
  })
})
