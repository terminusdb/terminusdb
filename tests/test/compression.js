const zlib = require('zlib')
const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('compression', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.createAfterDel(agent, endpoint.db(agent.defaults()).path)
  })

  after(async function () {
    await db.del(agent, endpoint.db(agent.defaults()).path)
  })

  const encodings = [
    'gzip',
    'deflate',
  ]

  for (const encoding of encodings) {
    it(encoding, async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class' }
      await document
        .insert(agent, endpoint.document(agent.defaults()).path, { schema })
        .set('Content-Encoding', encoding)
        .serialize((data) => zlib[encoding + 'Sync'](JSON.stringify(data)))
        .then(document.verifyInsertSuccess)
      const r = await document
        .get(agent, endpoint.document(agent.defaults()).path, {
          query: { graph_type: 'schema', id: schema['@id'] },
        })
        .then(document.verifyGetSuccess)
      expect(r.body).to.deep.equal(schema)
    })
  }
})
