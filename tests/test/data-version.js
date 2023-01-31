const { expect } = require('chai')
const { Agent, api, db, document, util, woql } = require('../lib')

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
    before(async function () {
      await db.create(agent)
    })

    after(async function () {
      await db.delete(agent)
    })

    describe('/api/document/..., /api/document/.../_meta', function () {
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

            docs.push(await document.get(agent, getInstances))
            metas.push(await document.getFromMeta(agent, getInstances))
            commits.push(await document.getFromCommits(agent, getInstances))

            docs.push(await document.insert(agent, object))

            metas.push(await document.getFromMeta(agent, getInstances))
            commits.push(await document.getFromCommits(agent, getInstances))

            docs.push(await document.get(agent, getSchemas))
            metas.push(await document.getFromMeta(agent, getInstances))
            commits.push(await document.getFromCommits(agent, getInstances))

            const header = lastDataVersionHeader(docs)
            const headerMeta = lastDataVersionHeader(metas)
            const headerCommits = lastDataVersionHeader(commits)

            docs.push(await document.get(agent, getInstances).set('TerminusDB-Data-Version', header))
            metas.push(await document.getFromMeta(agent, getInstances).set('TerminusDB-Data-Version', headerMeta))
            commits.push(await document.getFromCommits(agent, getInstances).set('TerminusDB-Data-Version', headerCommits))

            expectHeaders('branch', docs)
            expectHeaders('meta', metas)
            expectHeaders('repo', commits)
          })
        }
      })

      it('fails on bad data version', async function () {
        const x = '{}'
        await document.get(agent)
          .set('TerminusDB-Data-Version', x)
          .fails(api.error.badDataVersion(x))
      })
    })

    it('/api/document/.../commit/...', async function () {
      const r1 = await document.getFromCommits(agent, { query: { as_list: true } })
      const initialCommit = r1.body.find((i) => i['@type'] === 'InitialCommit')
      if (util.isUndefinedOrNull(initialCommit)) {
        throw new Error(`Missing InitialCommit in response: ${r1.body}`)
      }
      const commitId = initialCommit.identifier
      const header = 'commit:' + commitId
      const r2 = await document
        .getFromCommit(agent, commitId, { query: { as_list: true } })
        .set('TerminusDB-Data-Version', header)
      expect(r2.header['terminusdb-data-version']).to.equal(header)
    })

    describe('/api/woql/...', function () {
      const simpleQuery = {
        '@type': 'Equals',
        left: { '@type': 'True' },
        right: { '@type': 'True' },
      }

      it('fails on bad data version', async function () {
        const x = 'abc'
        await woql
          .post(agent, simpleQuery)
          .set('TerminusDB-Data-Version', x)
          .fails(api.error.badDataVersion(x))
      })

      describe('handles data version headers', function () {
        let h0

        before(async function () {
          await document.insert(agent, { schema: { '@id': randomType2, '@type': 'Class' } })
          const r = await woql.post(agent, simpleQuery)
          h0 = dataVersionHeader(r)
          expect(h0).to.match(/^branch:/)
        })

        it('correct', async function () {
          await woql.post(agent, simpleQuery).set('TerminusDB-Data-Version', h0)
        })

        it('wrong', async function () {
          const header = h0.substring(0, h0.length - 1)
          await woql
            .post(agent, simpleQuery)
            .set('TerminusDB-Data-Version', header)
            .fails(api.error.dataVersionMismatch(header, h0))
        })

        it('InsertDocument', async function () {
          const insertDocumentQuery = {
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
          }

          const r1 = await woql
            .post(agent, insertDocumentQuery)
            .set('TerminusDB-Data-Version', h0)
          const h1 = dataVersionHeader(r1)
          const r2 = await woql
            .post(agent, simpleQuery)
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
})
