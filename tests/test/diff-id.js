const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('diff-id', function () {
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

    it('diff two_documents', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id, a: 'xsd:string' },
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: { '@type': id, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)
      const dv1 = r1.header['terminusdb-data-version']
      const [docId] = r1.body
      const r2 = await document
        .replace(agent, docPath, {
          instance: { '@type': id, '@id': docId, a: 'vegan sausage' },
        })
        .then(document.verifyReplaceSuccess)
      const dv2 = r2.header['terminusdb-data-version']

      const { path } = endpoint.versionDiff(agent.defaults())
      const r3 = await agent.post(path).send(
        {
          before_data_version: dv1,
          after_data_version: dv2,
          document_id: docId,
        })
      expect(r3.body).to.deep.equal({
        '@id': r3.body['@id'],
        a: {
          '@after': 'vegan sausage',
          '@before': 'pickles and eggs',
          '@op': 'SwapValue',
        },
      })
      expect(r3.status).to.equal(200)
    })

    it('diff db document against submitted document', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id, a: 'xsd:string' },
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: { '@type': id, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)

      const dv1 = r1.header['terminusdb-data-version']
      const docId = r1.body[0].split('terminusdb:///data/')[1]
      const { path } = endpoint.versionDiff(agent.defaults())

      const r2 = await agent.post(path).send(
        {
          before_data_version: dv1,
          document_id: docId,
          after: { '@type': id, '@id': docId, a: 'vegan sausage' },
        })
      expect(r2.body).to.deep.equal({
        '@id': docId,
        a: {
          '@after': 'vegan sausage',
          '@before': 'pickles and eggs',
          '@op': 'SwapValue',
        },
      })
      expect(r2.status).to.equal(200)
    })

    it('diff db document against submitted document normalizing ids and type', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id, a: 'xsd:string' },
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: { '@type': id, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)

      const dv1 = r1.header['terminusdb-data-version']
      const [docId] = r1.body
      const resultId = docId.split('terminusdb:///data/')[1]
      const { path } = endpoint.versionDiff(agent.defaults())

      const r2 = await agent.post(path).send(
        {
          before_data_version: dv1,
          document_id: docId,
          after: { '@type': id, '@id': docId, a: 'vegan sausage' },
        })
      expect(r2.body).to.deep.equal({
        '@id': resultId,
        a: {
          '@after': 'vegan sausage',
          '@before': 'pickles and eggs',
          '@op': 'SwapValue',
        },
      })
      expect(r2.status).to.equal(200)
    })

    it('diff db document against submitted document normalizing expanded property', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id, a: 'xsd:string' },
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: { '@type': id, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)

      const dv1 = r1.header['terminusdb-data-version']
      const [docId] = r1.body
      const resultId = docId.split('terminusdb:///data/')[1]
      const { path } = endpoint.versionDiff(agent.defaults())

      const r2 = await agent.post(path).send(
        {
          before_data_version: dv1,
          document_id: docId,
          after: {
            '@type': id,
            '@id': docId,
            'terminusdb:///schema#a': 'vegan sausage',
          },
        })
      expect(r2.body).to.deep.equal({
        '@id': resultId,
        a: {
          '@after': 'vegan sausage',
          '@before': 'pickles and eggs',
          '@op': 'SwapValue',
        },
      })
      expect(r2.status).to.equal(200)
    })

    it('diff db document against submitted document with non-existent property', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id, a: 'xsd:string' },
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: { '@type': id, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)

      const dv1 = r1.header['terminusdb-data-version']
      const [docId] = r1.body
      const { path } = endpoint.versionDiff(agent.defaults())

      const r2 = await agent.post(path).send(
        {
          before_data_version: dv1,
          document_id: docId,
          after: {
            '@type': id,
            '@id': docId,
            b: 3,
          },
        })
      expect(r2.body).to.deep.equal(
        {
          '@type': 'api:DiffErrorResponse',
          'api:error': {
            '@type': 'api:UnrecognizedProperty',
            'api:document': {
              '@id': docId,
              '@type': id,
              b: 3,
            },
            'api:property': 'b',
            'api:type': id,
          },
          'api:message': `Submitted document contained unrecognized property b for type "${id}"`,
          'api:status': 'api:failure',
        })
      expect(r2.status).to.equal(400)
    })
  })
})
