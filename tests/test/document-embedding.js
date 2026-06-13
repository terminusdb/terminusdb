const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('document-embedding', function () {
  describe('error cases (schema without embedding definition)', function () {
    let agent

    const schemaNoEmbedding = {
      '@id': 'Animal',
      '@type': 'Class',
      '@key': { '@type': 'Lexical', '@fields': ['name'] },
      name: 'xsd:string',
    }

    before(async function () {
      agent = new Agent().auth()
      await db.create(agent)
      await document.insert(agent, { schema: schemaNoEmbedding })
      await document.insert(agent, { instance: { '@type': 'Animal', name: 'Plato' } })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('fails for schema graph type with EmbeddingOnlyForInstanceGraphs error', async function () {
      const r = await document
        .get(agent, {
          query: { format: 'embedding', graph_type: 'schema', id: 'Animal/Plato' },
        })
        .unverified()
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:EmbeddingOnlyForInstanceGraphs')
    })

    it('fails when schema has no embedding definitions with NoEmbeddingQueriesDefined error', async function () {
      const r = await document
        .get(agent, { query: { format: 'embedding', id: 'Animal/Plato' } })
        .unverified()
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:NoEmbeddingQueriesDefined')
    })

    it('fails when requesting by type with no embedding definitions with NoEmbeddingQueriesDefined error', async function () {
      const r = await document
        .get(agent, { query: { format: 'embedding', type: 'Animal' } })
        .unverified()
      expect(r.status).to.equal(400)
      expect(r.body['api:error']).to.have.property('@type', 'api:NoEmbeddingQueriesDefined')
    })
  })

  describe('success cases (schema with embedding definition)', function () {
    let agent

    const schemaWithEmbedding = [
      {
        '@type': '@context',
        '@base': 'http://example.com/data/',
        '@schema': 'http://example.com/schema#',
      },
      {
        '@id': 'Animal',
        '@type': 'Class',
        '@key': { '@type': 'Lexical', '@fields': ['name'] },
        name: 'xsd:string',
        '@metadata': {
          embedding: {
            query: 'query ($id: ID) { Animal(id: $id) { name } }',
            template: 'The animal is named {{name}}.',
          },
        },
      },
    ]

    before(async function () {
      agent = new Agent().auth()
      await db.create(agent)
      await document.insert(agent, { schema: schemaWithEmbedding, fullReplace: true })
      await document.insert(agent, { instance: { '@type': 'Animal', name: 'Plato' } })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('returns 200 text/plain with rendered template for a document with embedding definition', async function () {
      const r = await document
        .get(agent, { query: { format: 'embedding', id: 'Animal/Plato' } })
        .unverified()
      expect(r.status).to.equal(200)
      expect(r.type).to.equal('text/plain')
      expect(r.text).to.include('The animal is named Plato.')
    })
  })
})
