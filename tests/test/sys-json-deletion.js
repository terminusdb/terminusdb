const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON deletion variants', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('variant 1: Simple objects without arrays', function () {
    let doc1Id // eslint-disable-line no-unused-vars

    before(async function () {
      await db.create(agent, { label: 'Test Simple Objects', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'SimpleDoc',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          name: 'xsd:string',
          data: 'sys:JSON',
        },
      })

      // Simple objects - no arrays
      const sharedJSON = { config: { timeout: 30, retries: 3 } }

      const result = await document.insert(agent, {
        instance: [
          { '@type': 'SimpleDoc', name: 'simple1', data: sharedJSON },
          { '@type': 'SimpleDoc', name: 'simple2', data: sharedJSON },
        ],
      })

      doc1Id = result.body[0]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should allow deleting documents with simple objects', async function () {
      const result = await document.delete(agent, { query: { id: doc1Id } }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        console.log('✗ UNEXPECTED: Simple object deletion failed')
        console.log('Status:', result.status)
        console.log('Error:', JSON.stringify(result.body, null, 2))
        throw new Error('Simple objects should work')
      }

      expect(result.status).to.be.oneOf([200, 204])
    })
  })

  describe('variant 2: Objects with arrays', function () {
    let doc1Id

    before(async function () {
      await db.create(agent, { label: 'Test Arrays', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'ArrayDoc',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          name: 'xsd:string',
          data: 'sys:JSON',
        },
      })

      // Objects with arrays - triggers Cons list sharing
      const sharedJSON = {
        config: {
          servers: ['server1', 'server2', 'server3'],
          retries: 3,
        },
      }

      const result = await document.insert(agent, {
        instance: [
          { '@type': 'ArrayDoc', name: 'array1', data: sharedJSON },
          { '@type': 'ArrayDoc', name: 'array2', data: sharedJSON },
        ],
      })

      doc1Id = result.body[0]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('can delete documents with shared arrays', async function () {
      const result = await document.delete(agent, { query: { id: doc1Id } }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        console.log('✓ BUG: Array deletion failed ')
        console.log('Status:', result.status)
        console.log('Error:', JSON.stringify(result.body, null, 2))

        if (JSON.stringify(result.body).includes('deleted_object_still_referenced')) {
          console.log('✓ CONFIRMED: deleted_object_still_referenced error')
          if (JSON.stringify(result.body).includes('Cons/SHA1')) {
            console.log('✓ CONFIRMED: Cons list node shared between documents')
          }
        }

        // This is the expected bug - test passes when it fails
        throw new Error('Array deletion failed')
      } else {
        expect(result.status).to.be.oneOf([200, 204])
      }
    })
  })

  describe('variant 3: Nested structures with multiple arrays', function () {
    let doc1Id

    before(async function () {
      await db.create(agent, { label: 'Test Nested Arrays', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'NestedDoc',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          name: 'xsd:string',
          data: 'sys:JSON',
        },
      })

      // Complex nested structure with multiple arrays
      const sharedJSON = {
        parameters: {
          required: ['field1', 'field2', 'field3'],
          optional: ['opt1', 'opt2'],
        },
        settings: {
          flags: [true, false, true],
        },
      }

      const result = await document.insert(agent, {
        instance: [
          { '@type': 'NestedDoc', name: 'nested1', data: sharedJSON },
          { '@type': 'NestedDoc', name: 'nested2', data: sharedJSON },
        ],
      })

      doc1Id = result.body[0]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('can delete documents with multiple nested arrays', async function () {
      const result = await document.delete(agent, { query: { id: doc1Id } }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        console.log('✓ BUG CONFIRMED: Nested array deletion failed')
        console.log('Status:', result.status)
        console.log('Error body:', JSON.stringify(result.body, null, 2))

        // Expected to fail with Cons list sharing
        throw new Error('Nested array deletion failed')
      } else {
        expect(result.status).to.equal(204)
      }
    })
  })

  describe('variant 4: Empty arrays vs non-empty arrays', function () {
    let emptyArrayDoc, nonEmptyArrayDoc

    before(async function () {
      await db.create(agent, { label: 'Test Empty Arrays', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'EmptyArrayDoc',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          name: 'xsd:string',
          data: 'sys:JSON',
        },
      })

      const result = await document.insert(agent, {
        instance: [
          {
            '@type': 'EmptyArrayDoc',
            name: 'empty',
            data: { items: [] },
          },
          {
            '@type': 'EmptyArrayDoc',
            name: 'non-empty',
            data: { items: ['item1'] },
          },
        ],
      })

      emptyArrayDoc = result.body[0]
      nonEmptyArrayDoc = result.body[1]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should handle empty array deletion', async function () {
      const result = await document.delete(agent, { query: { id: emptyArrayDoc } }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        console.log('Note: Empty array deletion failed')
        console.log('Status:', result.status)
        throw new Error('Empty array deletion failed')
      } else {
        expect(result.status).to.be.oneOf([200, 204])
      }
    })

    it('should handle non-empty array deletion', async function () {
      const result = await document.delete(agent, { query: { id: nonEmptyArrayDoc } }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        console.log('Note: Non-empty array deletion failed')
        console.log('Status:', result.status)
        throw new Error('Non-empty array deletion failed')
      } else {
        expect(result.status).to.be.oneOf([200, 204])
      }
    })
  })
})
