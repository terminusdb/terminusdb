const { expect } = require('chai')
const { Agent, branch, db, document, endpoint, util } = require('../lib')

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

    it('diff everything between two commits', async function () {
      const class1 = util.randomString()
      const class2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [{ '@type': 'Class', '@id': class1, a: 'xsd:string', b: class2 },
            {
              '@type': 'Class',
              '@id': class2,
              c: 'xsd:integer',
              '@subdocument': [],
              '@key': { '@type': 'Lexical', '@fields': ['c'] },
            },
          ],
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: {
            '@type': class1,
            a: 'pickles and eggs',
            b: {
              '@type': class2,
              c: 3,
            },
          },
        })
        .then(document.verifyInsertSuccess)
      const dv1 = r1.header['terminusdb-data-version']
      const [docId] = r1.body
      const r2 = await document
        .replace(agent, docPath, {
          instance: {
            '@type': class1,
            a: 'pickles and eggs',
            '@id': docId,
            b: {
              '@type': class2,
              c: 4,
            },
          },
        })
        .then(document.verifyInsertSuccess)
      const dv2 = r2.header['terminusdb-data-version']

      const { path } = endpoint.versionDiff(agent.defaults())

      const r3 = await agent.post(path).send(
        {
          before_data_version: dv1,
          after_data_version: dv2,
        })
      expect(r3.status).to.equal(200)
    })

    it('diff unchanged', async function () {
      const class1 = util.randomString()
      const class2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [{ '@type': 'Class', '@id': class1, a: 'xsd:string' },
            { '@type': 'Class', '@id': class2, b: 'xsd:string' },
          ],
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: { '@type': class1, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)
      const dv1 = r1.header['terminusdb-data-version']
      const r2 = await document
        .insert(agent, docPath, {
          instance: { '@type': class2, b: 'frog legs' },
        })
        .then(document.verifyInsertSuccess)
      const dv2 = r2.header['terminusdb-data-version']
      const [docId2Long] = r2.body
      const docId2 = docId2Long.split('terminusdb:///data/')[1]
      const { path } = endpoint.versionDiff(agent.defaults())

      const r3 = await agent.post(path).send(
        {
          before_data_version: dv1,
          after_data_version: dv2,
        })
      expect(r3.status).to.equal(200)
      expect(r3.body).to.deep.equal([
        {
          '@insert': {
            '@id': docId2,
            '@type': class2,
            b: 'frog legs',
          },
          '@op': 'Insert',
        },
      ])
    })

    it('apply squash commit', async function () {
      const class1 = util.randomString()
      const class2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            { '@type': 'Class', '@id': class1, a: 'xsd:string' },
            { '@type': 'Class', '@id': class2, b: 'xsd:string' },
          ],
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: { '@type': class1, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)

      const r2 = await document
        .insert(agent, docPath, {
          instance: { '@type': class2, b: 'frog legs' },
        })
        .then(document.verifyInsertSuccess)

      const dv2 = r2.header['terminusdb-data-version']
      const [docId2Long] = r2.body
      const docId2 = docId2Long.split('terminusdb:///data/')[1]

      const branchName = util.randomString()
      const newDefaults = endpoint.branchNew(agent.defaults(), branchName)

      await agent.post(newDefaults.path)
        .send({ origin: newDefaults.origin }).then(branch.verifySuccess)
      const newDocPath = endpoint.document(newDefaults).path

      const r3 = await document
        .replace(agent, newDocPath, {
          instance: { '@id': docId2, '@type': class2, b: 'vegan frog legs' },
        })
        .then(document.verifyInsertSuccess)
      const dv3 = r3.header['terminusdb-data-version']

      const app = endpoint.apply(agent.defaults())

      const r4 = await agent.post(app.path)
        .send({
          before_commit: dv2,
          after_commit: dv3,
          commit_info: { author: 'gavin', message: 'something' },
          type: 'squash',
          match_final_state: true,
        })

      expect(r4.status).to.equal(200)

      const r5 = await document
        .get(agent, docPath, { query: { type: class2, as_list: true } })

      expect(r5.body).to.deep.equal([
        {
          '@id': docId2,
          '@type': class2,
          b: 'vegan frog legs',
        }])
    })

    it('apply squash commit to obtain conflict', async function () {
      const class1 = util.randomString()
      const class2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            { '@type': 'Class', '@id': class1, a: 'xsd:string' },
            { '@type': 'Class', '@id': class2, b: 'xsd:string' },
          ],
        })
        .then(document.verifyInsertSuccess)
      const r1 = await document
        .insert(agent, docPath, {
          instance: { '@type': class1, a: 'pickles and eggs' },
        })
        .then(document.verifyInsertSuccess)
      const dv1 = r1.header['terminusdb-data-version']

      const r2 = await document
        .insert(agent, docPath, {
          instance: { '@type': class2, b: 'frog legs' },
        })
        .then(document.verifyInsertSuccess)

      const [docId2Long] = r2.body
      const docId2 = docId2Long.split('terminusdb:///data/')[1]

      const branchName = util.randomString()
      const newDefaults = endpoint.branchNew(agent.defaults(), branchName)

      await agent.post(newDefaults.path)
        .send({ origin: newDefaults.origin }).then(branch.verifySuccess)
      const newDocPath = endpoint.document(newDefaults).path

      const r3 = await document
        .replace(agent, newDocPath, {
          instance: { '@id': docId2, '@type': class2, b: 'vegan frog legs' },
        })
        .then(document.verifyInsertSuccess)
      const dv3 = r3.header['terminusdb-data-version']

      const app = endpoint.apply(agent.defaults())

      const r4 = await agent.post(app.path)
        .send({
          before_commit: dv1,
          after_commit: dv3,
          commit_info: { author: 'gavin', message: 'something' },
          type: 'squash',
          match_final_state: true,
        })

      expect(r4.body['api:witnesses'][0]['@op']).to.equal('InsertConflict')
      expect(r4.body['api:witnesses'][0]['@id_already_exists']).to.equal(docId2Long)
    })
  })
})
