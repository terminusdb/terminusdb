const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON Advanced Deletion Scenarios', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('variant 6: Three-way sharing (N-way reference counting)', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Three-way Sharing', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should preserve shared JSON until all three documents are deleted', async function () {
      const sharedArray = [1, 2, 3]

      // Create three documents sharing the same array
      const doc1 = { '@type': 'Document', json: sharedArray }
      const doc2 = { '@type': 'Document', json: sharedArray }
      const doc3 = { '@type': 'Document', json: sharedArray }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      const insert3 = await document.insert(agent, { instance: doc3 })
      expect(insert3.status).to.equal(200)
      const doc3Id = insert3.body[0]

      // Delete doc1 - array should survive (doc2 and doc3 still reference it)
      const delete1 = await document.delete(agent, { query: { id: doc1Id } })
      expect(delete1.status).to.be.oneOf([200, 204])

      // Verify doc2 and doc3 still have valid array
      const getDoc2 = await document.get(agent, { query: { id: doc2Id } })
      expect(getDoc2.status).to.equal(200)
      expect(getDoc2.body.json).to.deep.equal([1, 2, 3])

      const getDoc3 = await document.get(agent, { query: { id: doc3Id } })
      expect(getDoc3.status).to.equal(200)
      expect(getDoc3.body.json).to.deep.equal([1, 2, 3])

      // Delete doc2 - array should still survive (doc3 still references it)
      const delete2 = await document.delete(agent, { query: { id: doc2Id } })
      expect(delete2.status).to.be.oneOf([200, 204])

      // Verify doc3 still has valid array
      const getDoc3After = await document.get(agent, { query: { id: doc3Id } })
      expect(getDoc3After.status).to.equal(200)
      expect(getDoc3After.body.json).to.deep.equal([1, 2, 3])

      // Delete doc3 - now array should be deleted
      const delete3 = await document.delete(agent, { query: { id: doc3Id } })
      expect(delete3.status).to.be.oneOf([200, 204])

      // Verify all documents are gone
      const getFinal = await document.get(agent).unverified()
      expect(getFinal.status).to.equal(200)
      expect(getFinal.text.trim()).to.equal('')
    })
  })

  describe('variant 7: Partial/Overlapping sharing (Cons list tails)', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Partial Sharing', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should handle documents sharing array tails correctly', async function () {
      // Note: In TerminusDB's content-addressed system, [2,3,4] is a tail of [1,2,3,4]
      // This test verifies that deleting documents with overlapping arrays works correctly

      const doc1 = { '@type': 'Document', json: [1, 2, 3, 4] }
      const doc2 = { '@type': 'Document', json: [2, 3, 4] }
      const doc3 = { '@type': 'Document', json: [3, 4] }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      const insert3 = await document.insert(agent, { instance: doc3 })
      expect(insert3.status).to.equal(200)
      const doc3Id = insert3.body[0]

      // Delete doc1 - doc2 and doc3 should remain intact
      const delete1 = await document.delete(agent, { query: { id: doc1Id } })
      expect(delete1.status).to.be.oneOf([200, 204])

      const getDoc2 = await document.get(agent, { query: { id: doc2Id } })
      expect(getDoc2.status).to.equal(200)
      expect(getDoc2.body.json).to.deep.equal([2, 3, 4])

      const getDoc3 = await document.get(agent, { query: { id: doc3Id } })
      expect(getDoc3.status).to.equal(200)
      expect(getDoc3.body.json).to.deep.equal([3, 4])

      // Delete doc2 - doc3 should still be intact
      const delete2 = await document.delete(agent, { query: { id: doc2Id } })
      expect(delete2.status).to.be.oneOf([200, 204])

      const getDoc3After = await document.get(agent, { query: { id: doc3Id } })
      expect(getDoc3After.status).to.equal(200)
      expect(getDoc3After.body.json).to.deep.equal([3, 4])

      // Delete doc3 - all should be gone
      const delete3 = await document.delete(agent, { query: { id: doc3Id } })
      expect(delete3.status).to.be.oneOf([200, 204])

      const getFinal = await document.get(agent).unverified()
      expect(getFinal.status).to.equal(200)
      expect(getFinal.text.trim()).to.equal('')
    })
  })

  describe('variant 8: Mixed content-addressed and direct references', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Mixed References', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should handle documents with both shared and unique JSON', async function () {
      const sharedArray = [1, 2, 3]

      const doc1 = {
        '@type': 'Document',
        json: {
          shared: sharedArray,
          direct: { inline: 'data1' },
        },
      }

      const doc2 = {
        '@type': 'Document',
        json: {
          shared: sharedArray,
          different: { other: 'data2' },
        },
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Delete doc1 - shared array should survive, direct data deleted
      const delete1 = await document.delete(agent, { query: { id: doc1Id } })
      expect(delete1.status).to.be.oneOf([200, 204])

      // Verify doc2 still has shared array
      const getDoc2 = await document.get(agent, { query: { id: doc2Id } })
      expect(getDoc2.status).to.equal(200)
      expect(getDoc2.body.json.shared).to.deep.equal([1, 2, 3])
      expect(getDoc2.body.json.different).to.deep.equal({ other: 'data2' })

      // Clean up
      await document.delete(agent, { query: { id: doc2Id } })
    })
  })

  describe('variant 9: Deeply nested shared structures', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Deep Nesting', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should preserve entire nested structure when shared', async function () {
      const deepShared = {
        level1: {
          level2: {
            level3: {
              data: [1, 2, 3],
            },
          },
        },
      }

      const doc1 = { '@type': 'Document', json: deepShared }
      const doc2 = { '@type': 'Document', json: deepShared }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Delete doc1 - entire nested structure should survive
      const delete1 = await document.delete(agent, { query: { id: doc1Id } })
      expect(delete1.status).to.be.oneOf([200, 204])

      // Verify doc2 still has complete nested structure
      const getDoc2 = await document.get(agent, { query: { id: doc2Id } })
      expect(getDoc2.status).to.equal(200)
      expect(getDoc2.body.json).to.deep.equal(deepShared)

      // Clean up
      await document.delete(agent, { query: { id: doc2Id } })
    })
  })

  describe('variant 10: Same document, multiple references to same array', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Multiple References', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should handle single document with multiple references to shared JSON', async function () {
      const sharedArray = [1, 2, 3]

      const doc1 = {
        '@type': 'Document',
        json: {
          a: sharedArray,
          b: sharedArray, // Same array referenced twice in one document
        },
      }

      const doc2 = {
        '@type': 'Document',
        json: { x: sharedArray },
      }

      const insert1 = await document.insert(agent, { instance: doc1 })
      expect(insert1.status).to.equal(200)
      const doc1Id = insert1.body[0]

      const insert2 = await document.insert(agent, { instance: doc2 })
      expect(insert2.status).to.equal(200)
      const doc2Id = insert2.body[0]

      // Delete doc1 - array should survive (doc2 still needs it)
      const delete1 = await document.delete(agent, { query: { id: doc1Id } })
      expect(delete1.status).to.be.oneOf([200, 204])

      // Verify doc2 still has array
      const getDoc2 = await document.get(agent, { query: { id: doc2Id } })
      expect(getDoc2.status).to.equal(200)
      expect(getDoc2.body.json.x).to.deep.equal([1, 2, 3])

      // Clean up
      await document.delete(agent, { query: { id: doc2Id } })
    })
  })

  describe('variant 11: Update then delete', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Update Delete', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should clean up old JSON when document is updated, then delete new JSON on deletion', async function () {
      const oldArray = [1, 2, 3]
      const newArray = [4, 5, 6]

      const doc = { '@type': 'Document', json: oldArray }

      const insert = await document.insert(agent, { instance: doc })
      expect(insert.status).to.equal(200)
      const docId = insert.body[0]

      // Get the document to include @id
      const getDoc = await document.get(agent, { query: { id: docId } })
      expect(getDoc.status).to.equal(200)
      const fullDoc = getDoc.body

      // Update to new JSON value
      fullDoc.json = newArray
      const update = await document.replace(agent, { instance: fullDoc })
      expect(update.status).to.equal(200)

      // Verify new value
      const getUpdated = await document.get(agent, { query: { id: docId } })
      expect(getUpdated.status).to.equal(200)
      expect(getUpdated.body.json).to.deep.equal([4, 5, 6])

      // Delete document - both old and new JSON should be cleaned up
      const deleteDoc = await document.delete(agent, { query: { id: docId } })
      expect(deleteDoc.status).to.be.oneOf([200, 204])

      // Verify document is gone
      const getFinal = await document.get(agent).unverified()
      expect(getFinal.status).to.equal(200)
      expect(getFinal.text.trim()).to.equal('')
    })
  })

  describe('variant 12: Deletion order independence', function () {
    beforeEach(async function () {
      await db.create(agent, { label: 'Test Deletion Order', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    afterEach(async function () {
      await db.delete(agent)
    })

    it('should produce same result regardless of deletion order (A then B)', async function () {
      const sharedArray = [1, 2, 3]

      const docA = { '@type': 'Document', json: sharedArray }
      const docB = { '@type': 'Document', json: sharedArray }

      const insertA = await document.insert(agent, { instance: docA })
      expect(insertA.status).to.equal(200)
      const docAId = insertA.body[0]

      const insertB = await document.insert(agent, { instance: docB })
      expect(insertB.status).to.equal(200)
      const docBId = insertB.body[0]

      // Delete A first
      const deleteA = await document.delete(agent, { query: { id: docAId } })
      expect(deleteA.status).to.be.oneOf([200, 204])

      // B should still have array
      const getB = await document.get(agent, { query: { id: docBId } })
      expect(getB.status).to.equal(200)
      expect(getB.body.json).to.deep.equal([1, 2, 3])

      // Delete B
      const deleteB = await document.delete(agent, { query: { id: docBId } })
      expect(deleteB.status).to.be.oneOf([200, 204])

      // All gone
      const getFinal = await document.get(agent).unverified()
      expect(getFinal.status).to.equal(200)
      expect(getFinal.text.trim()).to.equal('')
    })

    it('should produce same result regardless of deletion order (B then A)', async function () {
      const sharedArray = [1, 2, 3]

      const docA = { '@type': 'Document', json: sharedArray }
      const docB = { '@type': 'Document', json: sharedArray }

      const insertA = await document.insert(agent, { instance: docA })
      expect(insertA.status).to.equal(200)
      const docAId = insertA.body[0]

      const insertB = await document.insert(agent, { instance: docB })
      expect(insertB.status).to.equal(200)
      const docBId = insertB.body[0]

      // Delete B first (opposite order from previous test)
      const deleteB = await document.delete(agent, { query: { id: docBId } })
      expect(deleteB.status).to.be.oneOf([200, 204])

      // A should still have array
      const getA = await document.get(agent, { query: { id: docAId } })
      expect(getA.status).to.equal(200)
      expect(getA.body.json).to.deep.equal([1, 2, 3])

      // Delete A
      const deleteA = await document.delete(agent, { query: { id: docAId } })
      expect(deleteA.status).to.be.oneOf([200, 204])

      // All gone
      const getFinal = await document.get(agent).unverified()
      expect(getFinal.status).to.equal(200)
      expect(getFinal.text.trim()).to.equal('')
    })
  })

  describe('variant 13a: Complex nested document (cache effectiveness)', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Complex Nesting', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should efficiently delete document with deeply nested shared structures', async function () {
      this.timeout(10000)

      // Create a document with many levels of nesting and internal sharing
      // This creates many content-addressed nodes that reference each other
      const sharedArray = [1, 2, 3, 4, 5]
      const sharedObj = { config: { timeout: 30 }, data: sharedArray }

      const complexDoc = {
        '@type': 'Document',
        json: {
          level1: {
            a: sharedObj,
            b: sharedObj, // Same object referenced twice
            c: sharedArray, // Same array referenced
            nested: {
              level2: {
                x: sharedObj, // Referenced again
                y: sharedArray, // Referenced again
                z: { deep: sharedArray }, // Wrapped reference
                more: {
                  level3: {
                    p: sharedObj,
                    q: sharedArray,
                    r: { wrapper: sharedObj },
                  },
                },
              },
            },
          },
          level1_copy: {
            a: sharedObj, // All the same references again
            b: sharedArray,
          },
        },
      }

      const startTime = Date.now()
      const insert = await document.insert(agent, { instance: complexDoc })
      const insertTime = Date.now() - startTime
      expect(insert.status).to.equal(200)
      const docId = insert.body[0]

      // Delete the complex document
      const deleteStart = Date.now()
      const deleteDoc = await document.delete(agent, { query: { id: docId } })
      const deleteTime = Date.now() - deleteStart

      expect(deleteDoc.status).to.be.oneOf([200, 204])

      // With caching, deletion should be reasonably fast despite complexity
      // Without caching, each shared node would be checked multiple times
      console.log(`Insert time: ${insertTime}ms, Delete time: ${deleteTime}ms`)
      expect(deleteTime).to.be.lessThan(1000) // Should be under 1 second

      // Verify document is gone
      const getFinal = await document.get(agent).unverified()
      expect(getFinal.status).to.equal(200)
      expect(getFinal.text.trim()).to.equal('')
    })
  })

  describe('variant 13: Large shared arrays (performance check)', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Large Arrays', schema: true })
      await document.insert(agent, {
        schema: {
          '@id': 'Document',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          json: 'sys:JSON',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should handle large arrays with multiple references efficiently', async function () {
      this.timeout(10000) // Give it more time for large data

      // Create a large array
      const largeArray = Array.from({ length: 1000 }, (_, i) => i)

      // Create 10 documents sharing this array
      const docIds = []
      for (let i = 0; i < 10; i++) {
        const doc = { '@type': 'Document', json: largeArray }
        const insert = await document.insert(agent, { instance: doc })
        expect(insert.status).to.equal(200)
        docIds.push(insert.body[0])
      }

      // Delete them one by one
      for (let i = 0; i < docIds.length; i++) {
        const startTime = Date.now()
        const deleteDoc = await document.delete(agent, { query: { id: docIds[i] } })
        const duration = Date.now() - startTime

        expect(deleteDoc.status).to.be.oneOf([200, 204])
        // Each deletion should be reasonably fast (< 1 second)
        expect(duration).to.be.lessThan(1000)

        // If not the last one, verify remaining docs still have the array
        if (i < docIds.length - 1) {
          const getNext = await document.get(agent, { query: { id: docIds[i + 1] } })
          expect(getNext.status).to.equal(200)
          expect(getNext.body.json).to.have.lengthOf(1000)
        }
      }

      // Verify all are gone
      const getFinal = await document.get(agent).unverified()
      expect(getFinal.status).to.equal(200)
      expect(getFinal.text.trim()).to.equal('')
    })
  })
})
