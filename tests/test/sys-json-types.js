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

    it('should accept JSON objects (currently works)', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'object-test',
          data: { key: 'value', nested: { prop: 123 } },
        },
      })

      expect(result.body).to.be.an('array').with.lengthOf(1)
    })

    it('should accept JSON arrays', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'array-test',
          data: ['item1', 'item2', 'item3'],
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Array rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
    })

    it('should accept nested arrays in JSON', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'nested-array-test',
          data: ['a', 'b', ['c', 'd']],
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Nested arrays rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
    })

    it('should accept objects in arrays', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'array-object-test',
          data: ['a', 'b', { key: 'value' }],
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Objects in arrays rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
    })

    it('should accept JSON strings', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'string-test',
          data: 'plain string value',
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`String rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
    })

    it('should accept JSON numbers', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'number-test',
          data: 42,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Number rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
    })

    it('should accept JSON booleans', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'JSONTest',
          name: 'boolean-test',
          data: true,
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Boolean rejected: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
      expect(result.body).to.be.an('array').with.lengthOf(1)
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
