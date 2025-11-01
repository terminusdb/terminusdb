const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('sys:JSON Bug - Subdocument Sharing Example', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('Subdocuments with JSON properties', function () {
    let docAId
    let docBId

    before(async function () {
      await db.create(agent, { label: 'Test Subdocument', schema: true })

      // Create schema with shared subdocument class containing JSON
      await document.insert(agent, {
        schema: [
          {
            '@id': 'C',
            '@type': 'Class',
            '@key': { '@type': 'Random' },
            name: 'xsd:string',
          },
          {
            '@id': 'subdocumentSharedClass',
            '@key': {
              '@fields': ['linkedDocument'],
              '@type': 'Lexical',
            },
            '@subdocument': [],
            '@type': 'Class',
            quantity: 'xsd:decimal',
            linkedDocument: 'C',
            metadata: 'sys:JSON', // JSON field in subdocument
          },
          {
            '@id': 'A',
            '@type': 'Class',
            '@key': { '@type': 'Random' },
            name: 'xsd:string',
            subdocumentPropertyA: {
              '@class': 'subdocumentSharedClass',
              '@type': 'Set',
            },
          },
          {
            '@id': 'B',
            '@type': 'Class',
            '@key': { '@type': 'Random' },
            name: 'xsd:string',
            subdocumentPropertyB: {
              '@class': 'subdocumentSharedClass',
              '@type': 'Set',
            },
          },
        ],
      })

      // Create linked document
      const cResult = await document.insert(agent, {
        instance: {
          '@type': 'C',
          name: 'linkedC',
        },
      })
      const cId = cResult.body[0]

      // Create document A with subdocument containing JSON
      const aResult = await document.insert(agent, {
        instance: {
          '@type': 'A',
          name: 'documentA',
          subdocumentPropertyA: [
            {
              '@type': 'subdocumentSharedClass',
              linkedDocument: cId,
              quantity: 10,
              metadata: {
                source: 'import',
                timestamp: '2023-01-01',
                tags: ['important', 'verified'],
              },
            },
          ],
        },
      })
      docAId = aResult.body[0]

      // Create document B with similar subdocument (potentially sharing JSON)
      const bResult = await document.insert(agent, {
        instance: {
          '@type': 'B',
          name: 'documentB',
          subdocumentPropertyB: [
            {
              '@type': 'subdocumentSharedClass',
              linkedDocument: cId,
              quantity: 20,
              metadata: {
                source: 'import', // Same as A
                timestamp: '2023-01-02',
                tags: ['important', 'verified'], // Same array as A
              },
            },
          ],
        },
      })
      docBId = bResult.body[0]
    })

    after(async function () {
      await db.delete(agent)
    })

    it('should allow editing document A without affecting document B', async function () {
      const docA = await document.get(agent, { query: { id: docAId } })

      // Modify the subdocument's JSON
      docA.body.subdocumentPropertyA[0].metadata = {
        source: 'manual_edit',
        timestamp: '2023-01-05',
        tags: ['modified'],
      }

      const result = await document.replace(agent, { instance: docA.body }).unverified()

      if (result.status !== 200) {
        throw new Error(`Update failed: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.equal(200)

      // Verify document B is unaffected
      const docB = await document.get(agent, { query: { id: docBId } })
      expect(docB.body.subdocumentPropertyB[0].metadata.source).to.equal('import')
    })

    it('should allow deleting document A', async function () {
      const result = await document.delete(agent, { query: { id: docAId } }).unverified()

      if (result.status !== 200 && result.status !== 204) {
        throw new Error(`Deletion failed: ${JSON.stringify(result.body)}`)
      }

      expect(result.status).to.be.oneOf([200, 204])

      // Verify document B still exists
      const docB = await document.get(agent, { query: { id: docBId } })
      expect(docB.body.name).to.equal('documentB')
    })
  })
})
