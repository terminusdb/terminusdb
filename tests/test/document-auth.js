const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('document', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('1 database, shared', function () {
    let dbPath
    let docPath

    before(async function () {
      const dbDefaults = endpoint.db(agent.defaults())
      dbPath = dbDefaults.path
      docPath = endpoint.document(dbDefaults).path

      await db.createAfterDel(agent, dbPath)
    })

    after(async function () {
      await db.del(agent, dbPath)
    })

    it('responds with expected @id values (object)', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id },
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: { '@type': id, '@id': `terminusdb:///data/${id}/0` },
        })
        .then(document.verifyInsertSuccess)
    })

    it('responds with expected @id values (array)', async function () {
      const id1 = util.randomString()
      const id2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            { '@type': 'Class', '@id': id1 },
            { '@type': 'Class', '@id': id2 },
          ],
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: [
            { '@type': id1, '@id': `terminusdb:///data/${id1}/1` },
            { '@type': id2, '@id': `terminusdb:///data/${id2}/2` },
          ],
        })
        .then(document.verifyInsertSuccess)
    })

    it('fails on subdocument @key checks (#566)', async function () {
      const schema = { '@type': 'Class', '@subdocument': [] }
      {
        schema['@id'] = util.randomString()
        const r = await document.insert(agent, docPath, { schema: schema })
        document.verifyInsertFailure(r)
        expect(r.body['api:error']['@type']).to.equal('api:SubdocumentKeyMissing')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
      }
      {
        schema['@id'] = util.randomString()
        schema['@key'] = { useless_key: 'useless_value' }
        const r = await document.insert(agent, docPath, { schema: schema })
        document.verifyInsertFailure(r)
        expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyTypeMissing')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
        expect(JSON.parse(r.body['api:error']['api:key'])).to.deep.equal(schema['@key'])
      }
      {
        schema['@id'] = util.randomString()
        schema['@key'] = { '@type': 'Unknown' }
        const r = await document.insert(agent, docPath, { schema: schema })
        document.verifyInsertFailure(r)
        expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyTypeUnknown')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
        expect(r.body['api:error']['api:key_type']).to.equal(schema['@key']['@type'])
      }
    })

    it('fails when @key value is not an object (#587)', async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class', '@key': false }
      const r = await document.insert(agent, docPath, { schema: schema })

      document.verifyInsertFailure(r)
      expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyNotObject')
      expect(r.body['api:error']['api:key_value']).to.equal(false)
      expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
      expect(r.body['api:error']['api:document']['@key']).to.equal(false)
      expect(r.body['api:error']['api:document']['@type']).to.equal('Class')
    })
  })
})
