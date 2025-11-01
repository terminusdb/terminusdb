const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON Type Support', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('All JSON types should be supported', function () {
    before(async function () {
      await db.create(agent, { label: 'Test JSON Types', schema: true })

      await document.insert(agent, {
        schema: {
          '@id': 'JSONTest',
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

    it('should accept JSON objects and retrieve them correctly', async function () {
      const originalData = { key: 'value', nested: { prop: 123 } }
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'object-test',
          data: originalData,
        },
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should accept JSON arrays and retrieve them correctly', async function () {
      const originalData = ['item1', 'item2', 'item3']
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'array-test',
          data: originalData,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Array rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should accept nested arrays in JSON and retrieve them correctly', async function () {
      const originalData = ['a', 'b', ['c', 'd']]
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'nested-array-test',
          data: originalData,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Nested arrays rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should accept objects in arrays and retrieve them correctly', async function () {
      const originalData = ['a', 'b', { key: 'value' }]
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'array-object-test',
          data: originalData,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Objects in arrays rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should accept JSON strings and retrieve them correctly', async function () {
      const originalData = 'plain string value'
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'string-test',
          data: originalData,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`String rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.equal(originalData)
    })

    it('should accept JSON numbers and retrieve them correctly', async function () {
      const originalData = 42
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'number-test',
          data: originalData,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Number rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.equal(originalData)
    })

    it('should accept JSON booleans and retrieve them correctly', async function () {
      const originalData = true
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'boolean-test',
          data: originalData,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Boolean rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.equal(originalData)
    })

    it('should accept array with numeric-key objects and retrieve correctly', async function () {
      // eslint-disable-next-line quote-props
      const originalData = [{ '1234': 11234.223 }] // Quoted to test numeric string keys
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'numeric-key-test',
          data: originalData,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Numeric-key object rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
      const docId = result.body[0]

      // Retrieve and verify round-trip
      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.body.data).to.deep.equal(originalData)
    })

    it('should accept JSON null within objects (workaround)', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'nested-null-test',
          data: {
            value: null,
            other: 'data',
            nested: {
              field: null,
              active: false,
            },
          },
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Nested null rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)

      // Verify we can retrieve it
      const doc = await document.get(agent, { query: { id: result.body[0] } })
      expect(doc.body.data.value).to.equal(null)
      expect(doc.body.data.nested.field).to.equal(null)
    })

    it('DOES NOT accept null as top-level sys:JSON value (known limitation - use nested null instead)', async function () {
      // KNOWN LIMITATION: Top-level null is treated as "field not present" in Prolog dictionaries
      // WORKAROUND: Use nested null (see test above)
      // Example: Instead of { data: null }, use { data: { value: null } }

      // Use Optional or Set instead. This will not be changed unless there is a compelling reason to

      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'null-test',
          data: null,
        },
      }).unverified()

      expect(result.status).to.equal(400)
    })
  })
})
