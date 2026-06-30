const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('document-hyphenated-prefix', function () {
  this.timeout(10000)

  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  describe('CRUD with a hyphenated namespace prefix', function () {
    const schema = [
      {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        'dfrnt-bom': 'https://example.com/dfrnt-bom/',
      },
      {
        '@id': 'dfrnt-bom:Item',
        '@type': 'Class',
        '@key': {
          '@type': 'Random',
        },
        name: 'xsd:string',
        description: 'xsd:string',
      },
    ]

    before(async function () {
      await db.create(agent, { label: 'Hyphenated prefix document test' })
      await document.insert(agent, { schema, fullReplace: true })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('stores and retrieves a schema document using a hyphenated prefix', async function () {
      const r = await document.get(agent, {
        query: { graph_type: 'schema', id: 'dfrnt-bom:Item' },
      })
      expect(r.status).to.equal(200)
      expect(r.body['@id']).to.equal('dfrnt-bom:Item')
      expect(r.body['@type']).to.equal('Class')
    })

    it('inserts and retrieves an instance document using a hyphenated prefix', async function () {
      const instance = {
        '@type': 'dfrnt-bom:Item',
        name: 'Test item',
        description: 'Original description',
      }
      const result = await document.insert(agent, { instance })
      expect(result.status).to.equal(200)
      expect(result.body.length).to.equal(1)

      const docId = result.body[0]
      const r = await document.get(agent, { query: { id: docId } })
      expect(r.status).to.equal(200)
      expect(r.body['@id']).to.be.a('string')
      expect(r.body['@type']).to.equal('dfrnt-bom:Item')
      expect(r.body.name).to.equal('Test item')
      expect(r.body.description).to.equal('Original description')
    })

    it('updates an instance document using a hyphenated prefix', async function () {
      const instance = {
        '@type': 'dfrnt-bom:Item',
        name: 'Test item',
        description: 'Original description',
      }
      const insertResult = await document.insert(agent, { instance })
      const docId = insertResult.body[0]

      const updated = {
        '@type': 'dfrnt-bom:Item',
        '@id': docId,
        name: 'Test item',
        description: 'Updated description',
      }
      const r = await document.replace(agent, { instance: updated })
      expect(r.status).to.equal(200)

      const retrieved = await document.get(agent, { query: { id: docId } })
      expect(retrieved.status).to.equal(200)
      expect(retrieved.body.description).to.equal('Updated description')
      expect(retrieved.body['@type']).to.equal('dfrnt-bom:Item')
      expect(retrieved.body['@id']).to.be.a('string')
    })
  })

  describe('CRUD with a hyphenated schema namespace', function () {
    const schema = [
      {
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'https://example.com/dfrnt-bom/',
      },
      {
        '@id': 'Item',
        '@type': 'Class',
        '@key': {
          '@type': 'Random',
        },
        name: 'xsd:string',
        description: 'xsd:string',
      },
    ]

    before(async function () {
      await db.create(agent, { label: 'Hyphenated schema namespace document test' })
      await document.insert(agent, { schema, fullReplace: true })
    })

    after(async function () {
      await db.delete(agent)
    })

    it('stores and retrieves a schema document in a hyphenated schema namespace', async function () {
      const r = await document.get(agent, {
        query: { graph_type: 'schema', id: 'Item' },
      })
      expect(r.status).to.equal(200)
      expect(r.body['@id']).to.equal('Item')
      expect(r.body['@type']).to.equal('Class')
    })

    it('inserts and retrieves an instance document in a hyphenated schema namespace', async function () {
      const instance = {
        '@type': 'Item',
        name: 'Test item',
        description: 'Original description',
      }
      const result = await document.insert(agent, { instance })
      expect(result.status).to.equal(200)
      expect(result.body.length).to.equal(1)

      const docId = result.body[0]
      const r = await document.get(agent, { query: { id: docId } })
      expect(r.status).to.equal(200)
      expect(r.body['@id']).to.be.a('string')
      expect(r.body['@type']).to.equal('Item')
      expect(r.body.name).to.equal('Test item')
      expect(r.body.description).to.equal('Original description')
    })
  })
})
