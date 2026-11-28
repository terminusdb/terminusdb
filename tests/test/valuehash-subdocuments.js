const { expect } = require('chai')
const { Agent, db, document, util, woql } = require('../lib')

const DATA_VERSION_HEADER = 'terminusdb-data-version'

async function documentHistoryLength (agent, dbName, id) {
  const response = await agent.get(`/api/history/admin/${dbName}?id=${encodeURIComponent(id)}`)
  return response.body.length
}

async function triplesContaining (agent, fragment) {
  const query = {
    '@type': 'Triple',
    subject: { '@type': 'NodeValue', variable: 'Subject' },
    predicate: { '@type': 'NodeValue', variable: 'Predicate' },
    object: { '@type': 'Value', variable: 'Object' },
  }
  const result = await woql.post(agent, query)
  return result.body.bindings.filter(binding => JSON.stringify(binding).includes(fragment))
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

    it('treats an unsorted set inside a random subdocument as identical on repeat insert', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'RSetSub', '@subdocument': [], '@key': { '@type': 'Random' }, tags: { '@type': 'Set', '@class': 'xsd:string' } },
        { '@type': 'Class', '@id': 'VHSetParent', '@key': { '@type': 'ValueHash' }, sub: { '@type': 'Optional', '@class': 'RSetSub' } },
      ])
      const instance = { '@type': 'VHSetParent', sub: { '@type': 'RSetSub', tags: ['b', 'a'] } }

      await document.insert(agent, { instance })
      const inserted = await document.get(agent, { query: { type: 'VHSetParent', as_list: true } })
      const commitsBefore = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])
      await document.insert(agent, { instance })
      const commitsAfter = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])

      const result = await document.get(agent, { query: { type: 'VHSetParent', as_list: true } })
      expect(commitsAfter).to.equal(commitsBefore)
      expect(result.body).to.have.lengthOf(1)
      expect(result.body[0].sub).to.be.an('object')
      expect(result.body[0].sub.tags).to.deep.equal(['a', 'b'])
    })

    it('does not reuse a random subdocument when array shape changes', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'RArraySub', '@subdocument': [], '@key': { '@type': 'Random' }, arr: { '@type': 'Array', '@dimensions': 2, '@class': 'xsd:string' } },
        { '@type': 'Class', '@id': 'VHArrayParent', '@key': { '@type': 'ValueHash' }, sub: { '@type': 'Optional', '@class': 'RArraySub' } },
      ])
      await document.insert(agent, { instance: { '@type': 'VHArrayParent', sub: { '@type': 'RArraySub', arr: [['a', 'b']] } } })
      const inserted = await document.get(agent, { query: { type: 'VHArrayParent', as_list: true } })
      const id = inserted.body[0]['@id']

      await document.replace(agent, { instance: { '@id': id, '@type': 'VHArrayParent', sub: { '@type': 'RArraySub', arr: [['a'], ['b']] } } })

      const result = await document.get(agent, { query: { type: 'VHArrayParent', as_list: true } })
      expect(result.body[0].sub.arr).to.deep.equal([['a'], ['b']])
    })

    it('does not rewrite a captured random subdocument id during replacement', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'RCaptureSub', '@subdocument': [], '@key': { '@type': 'Random' }, x: 'xsd:string' },
        { '@type': 'Class', '@id': 'LCaptureParent', '@key': { '@type': 'Lexical', '@fields': ['k'] }, k: 'xsd:string', sub: { '@type': 'Optional', '@class': 'RCaptureSub' }, alias: { '@type': 'Optional', '@class': 'RCaptureSub' } },
      ])
      const instance = { '@type': 'LCaptureParent', k: 'one', sub: { '@type': 'RCaptureSub', '@capture': 'child', x: 'a' }, alias: { '@ref': 'child' } }
      await document.insert(agent, { instance })
      const inserted = await document.get(agent, { query: { type: 'LCaptureParent', as_list: true } })
      const id = inserted.body[0]['@id']

      await document.replace(agent, { instance: { '@id': id, ...instance } })

      const result = await document.get(agent, { query: { type: 'LCaptureParent', as_list: true } })
      expect(result.body).to.have.lengthOf(1)
      expect(result.body[0].sub.x).to.equal('a')
      expect(result.body[0].alias.x).to.equal('a')
    })

    it('treats an identical repeat insert with a list as a no-op', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'RListSub', '@subdocument': [], '@key': { '@type': 'Random' }, items: { '@type': 'List', '@class': 'xsd:string' } },
        { '@type': 'Class', '@id': 'VHListParent', '@key': { '@type': 'ValueHash' }, sub: { '@type': 'Optional', '@class': 'RListSub' } },
      ])
      const instance = { '@type': 'VHListParent', sub: { '@type': 'RListSub', items: ['a', 'b'] } }

      await document.insert(agent, { instance })
      const inserted = await document.get(agent, { query: { type: 'VHListParent', as_list: true } })
      const commitsBefore = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])
      await document.insert(agent, { instance })
      const commitsAfter = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])

      expect(commitsAfter).to.equal(commitsBefore)
      const result = await document.get(agent, { query: { type: 'VHListParent', as_list: true } })
      expect(result.body).to.have.lengthOf(1)
      expect(result.body[0].sub.items).to.deep.equal(['a', 'b'])
    })

    it('treats an identical repeat insert with a typed literal as a no-op', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'RTypedSub', '@subdocument': [], '@key': { '@type': 'Random' }, n: 'xsd:integer' },
        { '@type': 'Class', '@id': 'VHTypedParent', '@key': { '@type': 'ValueHash' }, sub: { '@type': 'Optional', '@class': 'RTypedSub' } },
      ])
      const instance = { '@type': 'VHTypedParent', sub: { '@type': 'RTypedSub', n: '1' } }

      await document.insert(agent, { instance })
      const inserted = await document.get(agent, { query: { type: 'VHTypedParent', as_list: true } })
      const commitsBefore = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])
      await document.insert(agent, { instance })
      const commitsAfter = await documentHistoryLength(agent, dbName, inserted.body[0]['@id'])

      expect(commitsAfter).to.equal(commitsBefore)
    })

    it('does not duplicate random children in a shared subdocument during replacement', async function () {
      await createSchema([
        { '@type': 'Class', '@id': 'RSharedChild', '@subdocument': [], '@key': { '@type': 'Random' }, x: 'xsd:string' },
        { '@type': 'Class', '@id': 'VHGroup', '@subdocument': [], '@key': { '@type': 'ValueHash' }, children: { '@type': 'Set', '@class': 'RSharedChild' } },
        { '@type': 'Class', '@id': 'VPGroup', '@key': { '@type': 'Lexical', '@fields': ['k'] }, k: 'xsd:string', sub: { '@type': 'Optional', '@class': 'VHGroup' } },
      ])
      const group = { '@type': 'VHGroup', children: [{ '@type': 'RSharedChild', x: 'a' }] }
      await document.insert(agent, { instance: { '@type': 'VPGroup', k: 'one', sub: group } })
      const inserted = await document.get(agent, { query: { id: 'VPGroup/one' } })
      const groupId = inserted.body.sub['@id']
      expect(inserted.body.sub.children).to.have.lengthOf(1)
      await document.insert(agent, { instance: { '@type': 'VPGroup', k: 'two', sub: { '@type': '@id', '@id': groupId } } })

      await document.replace(agent, { instance: { '@id': 'VPGroup/one', '@type': 'VPGroup', k: 'one', sub: group } })

      const result = await document.get(agent, { query: { type: 'VPGroup', as_list: true } })
      expect(result.body).to.have.lengthOf(2)
      expect(result.body[0].sub.children).to.have.lengthOf(1)
      expect(result.body[1].sub.children).to.have.lengthOf(1)
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

  describe('shared valuehash subdocument removal', function () {
    const sharedSchema = [
      { '@type': 'Class', '@id': 'VHSub', '@subdocument': [], '@key': { '@type': 'ValueHash' }, name: 'xsd:string' },
      { '@type': 'Class', '@id': 'VP', '@key': { '@type': 'Lexical', '@fields': ['k'] }, k: 'xsd:string', sub: { '@type': 'Optional', '@class': 'VHSub' } },
    ]
    const sharedInstance = [
      { '@type': 'VP', k: 'one', sub: { '@type': 'VHSub', name: 'shared' } },
      { '@type': 'VP', k: 'two', sub: { '@type': 'VHSub', name: 'shared' } },
    ]

    async function insertSharedParents () {
      await createSchema(sharedSchema)
      await document.insert(agent, { instance: sharedInstance, merge_repeats: true })
      const result = await document.get(agent, { query: { type: 'VP', as_list: true } })
      expect(result.body).to.have.lengthOf(2)
      expect(result.body[0].sub['@id']).to.equal(result.body[1].sub['@id'])
      return result.body[0].sub['@id']
    }

    it('deletes one sharing parent and keeps the subdocument for the survivor', async function () {
      await insertSharedParents()

      await document.delete(agent, { query: { id: 'VP/one' } })

      const survivor = await document.get(agent, { query: { id: 'VP/two' } })
      expect(survivor.body.sub.name).to.equal('shared')
    })

    it('updates one sharing parent to drop the reference and keeps the subdocument for the survivor', async function () {
      await insertSharedParents()

      await document.replace(agent, { instance: { '@id': 'VP/one', '@type': 'VP', k: 'one' } })

      const survivor = await document.get(agent, { query: { id: 'VP/two' } })
      expect(survivor.body.sub.name).to.equal('shared')
    })

    it('removes the shared subdocument when its last reference disappears', async function () {
      const childId = await insertSharedParents()
      const childFragment = childId.split('/').pop()

      await document.delete(agent, { query: { id: 'VP/one' } })
      await document.delete(agent, { query: { id: 'VP/two' } })

      expect(await triplesContaining(agent, childFragment)).to.have.lengthOf(0)
    })

    it('removes the shared subdocument when all sharers are deleted in one transaction', async function () {
      const childId = await insertSharedParents()
      const childFragment = childId.split('/').pop()

      await document.delete(agent, { body: ['VP/one', 'VP/two'] })

      expect(await triplesContaining(agent, childFragment)).to.have.lengthOf(0)
    })

    it('removes the shared subdocument when all sharers are deleted by type', async function () {
      const childId = await insertSharedParents()
      const childFragment = childId.split('/').pop()

      await document.delete(agent, { queryString: 'type=VP&author=default_author&message=default_message' })

      expect(await triplesContaining(agent, childFragment)).to.have.lengthOf(0)
    })

    it('still cascades into an exclusively owned valuehash subdocument', async function () {
      await createSchema(sharedSchema)
      await document.insert(agent, { instance: { '@type': 'VP', k: 'one', sub: { '@type': 'VHSub', name: 'exclusive' } } })
      const inserted = await document.get(agent, { query: { id: 'VP/one' } })
      const childFragment = inserted.body.sub['@id'].split('/').pop()

      await document.delete(agent, { query: { id: 'VP/one' } })

      expect(await triplesContaining(agent, childFragment)).to.have.lengthOf(0)
    })
  })
})
