const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON Bug - Habilidad Example (Nested JSON with shared arrays)', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('can delete documents with similar JSON function descriptions', function () {
    let doc1Id
    let doc2Id

    before(async function () {
      await db.create(agent, { label: 'Test Habilidad', schema: true })

      // Exact schema from user report
      await document.insert(agent, {
        schema: {
          '@documentation': {
            '@comment': 'The cardinality of these with respect to chats is many to many.',
            '@properties': {
              json: 'function description.',
              nombre: 'a name that helps to explain.',
            },
          },
          '@id': 'Habilidad',
          '@key': {
            '@fields': ['nombre'],
            '@type': 'Lexical',
          },
          '@metadata': {
            order_by: ['nombre', 'json'],
          },
          '@type': 'Class',
          json: 'sys:JSON',
          nombre: 'xsd:string',
        },
      })

      // Create two documents with OVERLAPPING nested JSON
      // They share the same "properties" object structure
      const result = await document.insert(agent, {
        instance: [
          {
            '@type': 'Habilidad',
            nombre: 'example1',
            json: {
              function: {
                description: 'Add a new product to the inventory.',
                parameters: {
                  type: 'object',
                  properties: {
                    Nombre: {
                      description: 'Product name.',
                      type: 'string',
                    },
                    Cantidad: {
                      description: 'Quantity.',
                      type: 'number',
                    },
                    Archivo: {
                      description: 'Media file.',
                      type: 'string',
                    },
                  },
                  required: ['Nombre', 'Cantidad', 'Archivo'],
                },
              },
              type: 'function',
            },
          },
          {
            '@type': 'Habilidad',
            nombre: 'example2',
            json: {
              function: {
                description: 'Update an existing product in the inventory.',
                parameters: {
                  type: 'object',
                  properties: {
                    Nombre: {
                      description: 'Product name.', // SAME as example1
                      type: 'string',
                    },
                    Cantidad: {
                      description: 'Quantity.', // SAME as example1
                      type: 'number',
                    },
                    Archivo: {
                      description: 'Media file.', // SAME as example1
                      type: 'string',
                    },
                  },
                  required: ['Nombre', 'Cantidad', 'Archivo'], // SAME array as example1
                },
              },
              type: 'function',
            },
          },
        ],
      })

      doc1Id = result.body[0]
      doc2Id = result.body[1]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should delete example1 without affecting example2', async function () {
      try {
        const deleteResult = await document.delete(agent, { query: { id: doc1Id } }).unverified()

        if (deleteResult.status !== 200 && deleteResult.status !== 204) {
          console.log('✗ BUG: Deletion failed')
          console.log('Status:', deleteResult.status)
          console.log('Error body:', JSON.stringify(deleteResult.body, null, 2))
          console.log('Error text:', deleteResult.text)

          if (JSON.stringify(deleteResult.body).includes('deleted_object_still_referenced')) {
            console.log('CONFIRMED: deleted_object_still_referenced error')
          }
          throw new Error(`Deletion failed with status ${deleteResult.status}: ${JSON.stringify(deleteResult.body)}`)
        }

        // Verify example2 still exists
        const doc2 = await document.get(agent, { query: { id: doc2Id } })
        expect(doc2.body.nombre).to.equal('example2')
        expect(doc2.body.json).to.exist
      } catch (err) {
        if (err.response) {
          console.log('✗ BUG: Deletion failed')
          console.log('Status:', err.response.status)
          console.log('Error body:', JSON.stringify(err.response.body, null, 2))
          console.log('Error text:', err.response.text)
        }
        throw err
      }
    })

    it('should delete example2 successfully', async function () {
      try {
        const deleteResult = await document.delete(agent, { query: { id: doc2Id } }).unverified()

        if (deleteResult.status !== 200 && deleteResult.status !== 204) {
          console.log('✗ BUG: Deletion failed')
          console.log('Status:', deleteResult.status)
          console.log('Error body:', JSON.stringify(deleteResult.body, null, 2))
          throw new Error(`Deletion failed with status ${deleteResult.status}: ${JSON.stringify(deleteResult.body)}`)
        }
      } catch (err) {
        if (err.response) {
          console.log('✗ BUG: Deletion failed')
          console.log('Status:', err.response.status)
          console.log('Error body:', JSON.stringify(err.response.body, null, 2))
        }
        throw err
      }
    })
  })
})
