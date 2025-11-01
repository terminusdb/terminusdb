const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON Bug - Inheritance Example', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Inheritance with sys:JSON properties', function () {
    let parentDocId
    let childDocId

    before(async function () {
      await db.create(agent, { label: 'Test Inheritance', schema: true })

      // Create base entity class
      await document.insert(agent, {
        schema: {
          '@type': 'Class',
          '@id': 'Entity',
          '@key': { '@type': 'Random' },
          name: 'xsd:string',
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should create parent class with sys:JSON property', async function () {
      await document.insert(agent, {
        schema: {
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          '@id': 'NewProperties',
          '@inherits': ['Entity'],
          JSON: {
            '@class': 'sys:JSON',
            '@type': 'Optional',
          },
        },
      })
    })

    it('should create document of parent class with JSON data', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'NewProperties',
          name: 'Parent Document',
          JSON: {
            config: {
              enabled: true,
              options: ['opt1', 'opt2'],
            },
          },
        },
      })
      parentDocId = result.body[0]
      expect(parentDocId).to.be.a('string')
    })

    it('should create child class inheriting from parent with JSON', async function () {
      const result = await document.insert(agent, {
        schema: {
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          '@id': 'Test',
          '@inherits': ['NewProperties'],
          additionalField: 'xsd:string',
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Child class creation failed: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)
    })

    it('should create document of child class', async function () {
      const result = await document.insert(agent, {
        instance: {
          '@type': 'Test',
          name: 'Child Document',
          additionalField: 'extra data',
          JSON: {
            config: {
              enabled: false,
              options: ['opt3'],
            },
          },
        },
      }).unverified()

      if (result.status !== 200) {
        throw new Error(`Child document creation failed: ${JSON.stringify(result.body)}`)
      }

      childDocId = result.body[0]
      expect(result.status).to.equal(200)
    })

    it('should allow deleting child class after removing documents', async function () {
      // First delete the child document
      let result = await document.delete(agent, {
        query: { id: childDocId },
      }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        throw new Error(`Child document deletion failed: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.be.oneOf([200, 204])

      // Now delete the child class schema
      result = await document.delete(agent, {
        query: { graph_type: 'schema', id: 'Test' },
      }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        throw new Error(`Child class deletion failed: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.be.oneOf([200, 204])
    })
  })

  describe('Schema validation after document creation', function () {
    before(async function () {
      await db.create(agent, { label: 'Test Schema Validation', schema: true })

      // Create schema
      await document.insert(agent, {
        schema: {
          '@id': 'TestClass',
          '@type': 'Class',
          '@key': { '@type': 'Random' },
          data: 'sys:JSON',
        },
      })

      // Insert document
      await document.insert(agent, {
        instance: {
          '@type': 'TestClass',
          data: { test: 'value' },
        },
      })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should allow re-saving schema without changes after document insertion', async function () {
      try {
        // Get current schema
        const schema = await document.get(agent, {
          query: { graph_type: 'schema', id: 'TestClass' },
        })

        // Try to save it back (no changes)
        const result = await document.replace(agent, {
          schema: schema.body,
        }).unverified()

        if (result.status !== 200) {
          console.log('✗ BUG: Schema validation failed')
          console.log('Status:', result.status)
          console.log('Error body:', JSON.stringify(result.body, null, 2))
          if (JSON.stringify(result.body).includes('instance_not_of_class')) {
            console.log('CONFIRMED: instance_not_of_class error on schema re-validation')
          }
          throw new Error(`Schema re-save failed: ${JSON.stringify(result.body)}`)
        }
      } catch (err) {
        if (err.response) {
          console.log('✗ BUG: Schema validation failed')
          console.log('Error body:', JSON.stringify(err.response.body, null, 2))
        }
        throw err
      }
    })
  })
})
