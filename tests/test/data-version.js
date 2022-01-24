const { expect } = require('chai')
const { Agent, db, document, endpoint, util, woql } = require('../lib')

const randomType0 = util.randomString()
const randomType1 = util.randomString()
const randomType2 = util.randomString()

function dataVersionHeader (r) {
  return r.header['terminusdb-data-version']
}

function lastDataVersionHeader (rs) {
  return dataVersionHeader(rs[rs.length - 1])
}

function expectHeaders (label, rs) {
  const h = rs.map(dataVersionHeader)
  const re = new RegExp('^' + label + ':')
  expect(h[0]).to.match(re)
  expect(h[1]).to.match(re)
  expect(h[0]).to.not.equal(h[1])
  expect(h[1]).to.equal(h[2])
  expect(h[1]).to.equal(h[3])
}

describe('data-version', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('db', function () {
    let dbDefaults
    let dbPath

    before(async function () {
      dbDefaults = endpoint.db(agent.defaults())
      dbPath = dbDefaults.path
      await db.createAfterDel(agent, dbPath)
    })

    after(async function () {
      await db.del(agent, dbPath)
    })

    describe('/api/document/..., /api/document/.../_meta', function () {
      let docPath
      let metaPath
      let commitsPath

      before(async function () {
        docPath = endpoint.document(dbDefaults).path
        metaPath = endpoint.documentMeta(dbDefaults).path
        commitsPath = endpoint.documentCommits(dbDefaults).path
      })

      describe('has expected data version for insert', function () {
        const objects = [
          { schema: { '@id': randomType0, '@type': 'Class' } },
          { instance: { '@id': randomType0 + '/0', '@type': randomType0 } },
        ]
        for (const object of objects) {
          it(JSON.stringify(object), async function () {
            // Arrays for collecting results of requests
            const docs = []
            const metas = []
            const commits = []

            // Options
            const getInstances = { query: { as_list: true } }
            const getSchemas = { query: { graph_type: 'schema', as_list: true } }

            docs.push(await document.get(agent, docPath, getInstances).then(document.verifyGetSuccess))
            metas.push(await document.get(agent, metaPath, getInstances).then(document.verifyGetSuccess))
            commits.push(await document.get(agent, commitsPath, getInstances).then(document.verifyGetSuccess))

            docs.push(await document.insert(agent, docPath, object).then(document.verifyInsertSuccess))

            metas.push(await document.get(agent, metaPath, getInstances).then(document.verifyGetSuccess))
            commits.push(await document.get(agent, commitsPath, getInstances).then(document.verifyGetSuccess))

            docs.push(await document.get(agent, docPath, getSchemas).then(document.verifyGetSuccess))
            metas.push(await document.get(agent, metaPath, getInstances).then(document.verifyGetSuccess))
            commits.push(await document.get(agent, commitsPath, getInstances).then(document.verifyGetSuccess))

            const header = lastDataVersionHeader(docs)
            const headerMeta = lastDataVersionHeader(metas)
            const headerCommits = lastDataVersionHeader(commits)

            docs.push(await document.get(agent, docPath, getInstances).set('TerminusDB-Data-Version', header).then(document.verifyGetSuccess))
            metas.push(await document.get(agent, metaPath, getInstances).set('TerminusDB-Data-Version', headerMeta).then(document.verifyGetSuccess))
            commits.push(await document.get(agent, commitsPath, getInstances).set('TerminusDB-Data-Version', headerCommits).then(document.verifyGetSuccess))

            expectHeaders('branch', docs)
            expectHeaders('meta', metas)
            expectHeaders('repo', commits)
          })
        }
      })

      it('fails on bad data version', async function () {
        const r = await document
          .get(agent, docPath)
          .set('TerminusDB-Data-Version', '{}')
          .then(document.verifyGetFailure)
        expect(r.body['api:error']['@type']).to.equal('api:BadDataVersion')
        expect(r.body['api:error']['api:data_version']).to.equal('{}')
      })
    })

    describe('/api/woql/...', function () {
      let woqlPath
      let docPath
      const simpleQuery = {
        query: {
          '@type': 'Equals',
          left: { '@type': 'True' },
          right: { '@type': 'True' },
        },
      }

      before(async function () {
        woqlPath = endpoint.woqlResource(dbDefaults).path
        docPath = endpoint.document(dbDefaults).path
      })

      it('fails on bad data version', async function () {
        const r = await woql
          .post(agent, woqlPath, simpleQuery)
          .set('TerminusDB-Data-Version', 'abc')
          .then(woql.verifyGetFailure)
        expect(r.body['api:error']['@type']).to.equal('api:BadDataVersion')
        expect(r.body['api:error']['api:data_version']).to.equal('abc')
      })

      describe('handles data version headers', function () {
        let h0

        before(async function () {
          await document
            .insert(agent, docPath, { schema: { '@id': randomType2, '@type': 'Class' } })
            .then(document.verifyInsertSuccess)
          const r = await woql
            .post(agent, woqlPath, simpleQuery)
            .then(woql.verifyGetSuccess)
          h0 = dataVersionHeader(r)
          expect(h0).to.match(/^branch:/)
        })

        it('correct', async function () {
          await woql
            .post(agent, woqlPath, simpleQuery)
            .set('TerminusDB-Data-Version', h0)
            .then(woql.verifyGetSuccess)
        })

        it('wrong', async function () {
          const header = h0.substring(0, h0.length - 1)
          const r = await woql
            .post(agent, woqlPath, simpleQuery)
            .set('TerminusDB-Data-Version', header)
            .then(woql.verifyGetFailure)
          expect(r.body['api:error']['@type']).to.equal('api:DataVersionMismatch')
          expect(r.body['api:error']['api:requested_data_version']).to.equal(header)
          expect(r.body['api:error']['api:actual_data_version']).to.equal(h0)
        })

        it('InsertDocument', async function () {
          const insertDocumentQuery = {
            query: {
              '@type': 'InsertDocument',
              identifier: { '@type': 'NodeValue', node: randomType2 + '/0' },
              document: {
                '@type': 'Value',
                dictionary: {
                  '@type': 'DictionaryTemplate',
                  data: [
                    {
                      '@type': 'FieldValuePair',
                      field: '@type',
                      value: { '@type': 'Value', data: randomType2 },
                    },
                    {
                      '@type': 'FieldValuePair',
                      field: '@id',
                      value: { '@type': 'Value', data: randomType2 + '/0' },
                    },
                  ],
                },
              },
            },
            commit_info: { author: 'a', message: 'm' },
          }

          const r1 = await woql
            .post(agent, woqlPath, insertDocumentQuery)
            .set('TerminusDB-Data-Version', h0)
            .then(woql.verifyGetSuccess)
          const h1 = dataVersionHeader(r1)
          const r2 = await woql
            .post(agent, woqlPath, simpleQuery)
            .set('TerminusDB-Data-Version', h1)
          const h2 = dataVersionHeader(r2)

          expect(h1).to.match(/^branch:/)
          expect(h2).to.match(/^branch:/)
          expect(h1).to.not.equal(h0)
          expect(h1).to.equal(h2)
        })
      })
    })
  })

  describe('/api/document/_system', function () {
    let path

    before(async function () {
      path = endpoint.documentSystem().path
    })

    after(async function () {
      const r = await document
        .del(agent, path, { query: { id: randomType1 + '/0' } })
        .then(document.verifyDelSuccess)
      await document
        .del(agent, path, { query: { graph_type: 'schema', id: randomType1 } })
        .set('TerminusDB-Data-Version', dataVersionHeader(r))
        .then(document.verifyDelSuccess)
    })

    describe('has expected data version for insert', function () {
      const objects = [
        { schema: { '@id': randomType1, '@type': 'Class' } },
        { instance: { '@id': randomType1 + '/0', '@type': randomType1 } },
      ]
      for (const object of objects) {
        it(JSON.stringify(object), async function () {
          const rs = []

          const getInstances = { query: { as_list: true } }
          const getSchemas = { query: { graph_type: 'schema', as_list: true } }

          rs.push(await document.get(agent, path, getInstances).then(document.verifyGetSuccess))
          rs.push(await document.insert(agent, path, object).then(document.verifyInsertSuccess))
          rs.push(await document.get(agent, path, getSchemas).then(document.verifyGetSuccess))

          rs.push(await document.get(agent, path, getInstances).set('TerminusDB-Data-Version', lastDataVersionHeader(rs)).then(document.verifyGetSuccess))

          expectHeaders('system', rs)
        })
      }
    })
  })
})
