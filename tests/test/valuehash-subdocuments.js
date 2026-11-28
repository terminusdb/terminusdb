const { expect } = require('chai')
const { Agent, db, document, util } = require('../lib')

const DATA_VERSION_HEADER = 'terminusdb-data-version'

async function documentHistoryLength (agent, dbName, id) {
  const response = await agent.get(`/api/history/admin/${dbName}?id=${encodeURIComponent(id)}`)
  return response.body.length
}

describe('valuehash-subdocuments', function () {
  let agent
  let dbName

  beforeEach(function () {
    dbName = util.randomString()
    agent = new Agent({ dbName }).auth()
  })

  afterEach(async function () {
    await db.delete(agent)
  })

  async function createSchema (schema) {
    await db.create(agent)
    await document.insert(agent, { schema })
  }

  describe('random-keyed subdocuments in valuehash parents', function () {
    const randomSubSchema = [
      { '@type': 'Class', '@id': 'RSub', '@subdocument': [], '@key': { '@type': 'Random' }, x: 'xsd:string' },
      { '@type': 'Class', '@id': 'VHR', '@key': { '@type': 'ValueHash' }, sub: { '@type': 'Optional', '@class': 'RSub' } },
    ]

    it('treats an identical repeat insert as a no-op for an Optional subdocument', async function () {
      await createSchema(randomSubSchema)
      const instance = { '@type': 'VHR', sub: { '@type': 'RSub', x: 'a' } }

      const firstInsert = await document.insert(agent, { instance })
      const inserted = await document.get(agent, { query: { type: 'VHR', as_list: true } })
      const commitsBefore = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])
      const secondInsert = await document.insert(agent, { instance })
      const commitsAfter = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])

      expect(firstInsert.headers[DATA_VERSION_HEADER]).to.be.a('string')
      expect(secondInsert.headers[DATA_VERSION_HEADER]).to.equal(firstInsert.headers[DATA_VERSION_HEADER])
      expect(commitsAfter).to.equal(commitsBefore)
      const result = await document.get(agent, { query: { type: 'VHR', as_list: true } })
      expect(result.body).to.have.lengthOf(1)
      expect(result.body[0].sub).to.be.an('object')
      expect(result.body[0].sub.x).to.equal('a')
    })

    it('does not grow a Set of random-keyed subdocuments on an identical repeat insert', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'RSub', '@subdocument': [], '@key': { '@type': 'Random' }, x: 'xsd:string' },
        { '@type': 'Class', '@id': 'VHRSet', '@key': { '@type': 'ValueHash' }, sub: { '@type': 'Set', '@class': 'RSub' } },
      ])
      const instance = { '@type': 'VHRSet', sub: [{ '@type': 'RSub', x: 'a' }] }

      const firstInsert = await document.insert(agent, { instance })
      const inserted = await document.get(agent, { query: { type: 'VHRSet', as_list: true } })
      const commitsBefore = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])
      const secondInsert = await document.insert(agent, { instance })
      const commitsAfter = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])

      expect(secondInsert.headers[DATA_VERSION_HEADER]).to.equal(firstInsert.headers[DATA_VERSION_HEADER])
      expect(commitsAfter).to.equal(commitsBefore)
      const result = await document.get(agent, { query: { type: 'VHRSet', as_list: true } })
      expect(result.body).to.have.lengthOf(1)
      expect(result.body[0].sub).to.have.lengthOf(1)
      expect(result.body[0].sub[0].x).to.equal('a')
    })

    it('does not collapse genuinely different subdocument content', async function () {
      await createSchema(randomSubSchema)

      await document.insert(agent, { instance: { '@type': 'VHR', sub: { '@type': 'RSub', x: 'a' } } })
      await document.insert(agent, { instance: { '@type': 'VHR', sub: { '@type': 'RSub', x: 'b' } } })

      const result = await document.get(agent, { query: { type: 'VHR', as_list: true } })
      expect(result.body).to.have.lengthOf(2)
      expect(result.body.map(doc => doc.sub.x).sort()).to.deep.equal(['a', 'b'])
    })

    it('keeps deterministic ValueHash subdocument repeat inserts as no-ops', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'VHSub', '@subdocument': [], '@key': { '@type': 'ValueHash' }, x: 'xsd:string' },
        { '@type': 'Class', '@id': 'VHD', '@key': { '@type': 'ValueHash' }, sub: { '@type': 'Optional', '@class': 'VHSub' } },
      ])
      const instance = { '@type': 'VHD', sub: { '@type': 'VHSub', x: 'a' } }

      const firstInsert = await document.insert(agent, { instance })
      const inserted = await document.get(agent, { query: { type: 'VHD', as_list: true } })
      const commitsBefore = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])
      const secondInsert = await document.insert(agent, { instance })
      const commitsAfter = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])

      expect(secondInsert.headers[DATA_VERSION_HEADER]).to.equal(firstInsert.headers[DATA_VERSION_HEADER])
      expect(commitsAfter).to.equal(commitsBefore)
      const result = await document.get(agent, { query: { type: 'VHD', as_list: true } })
      expect(result.body).to.have.lengthOf(1)
      expect(result.body[0].sub.x).to.equal('a')
    })
  })

})
