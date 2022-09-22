const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('backlinks', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  beforeEach(async function () {
    await db.create(agent)
    const schema = [
      { '@type': 'Class', '@id': 'CycleDoc', other: { '@type': 'Set', '@class': 'CycleDoc' } },
      { '@type': 'Class', '@id': 'Doc', subdoc: { '@type': 'Set', '@class': 'Subdoc' } },
      {
        '@type': 'Class',
        '@id': 'Subdoc',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        subdoc: { '@type': 'Set', '@class': 'Subdoc' },
      },
    ]
    await document.insert(agent, { schema })
  })

  afterEach(async function () {
    await db.delete(agent)
  })

  it('fails with a subdocument directly inserted with no @linked-by', async function () {
    const instance = { '@type': 'Subdoc' }
    const r = await document.insert(agent, { instance }).unverified()
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:InsertedSubdocumentAsDocument')
  })

  it('fails with a subdocument inserted as part of another document but also having @linked-by', async function () {
    const instance = [{ '@type': 'Doc', '@capture': 'doc1' },
      { '@type': 'Doc', subdoc: { '@linked-by': { '@ref': 'doc1', '@property': 'subdoc' } } }]
    const r = await document.insert(agent, { instance }).unverified()
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:EmbeddedSubdocumentHasLinkedBy')
  })

  it('generates the correct id for a backlinked subdocument', async function () {
    const instance = [{ '@type': 'Doc', '@capture': 'doc' },
      { '@type': 'Subdoc', '@linked-by': { '@ref': 'doc', '@property': 'subdoc' } }]

    const r = await document.insert(agent, { instance })
    const docId = r.body[0]
    const subdocId = r.body[1]

    expect(subdocId).to.match(new RegExp(`^${docId}/subdoc/Subdoc/.*`))
  })

  it('generates the correct id for a deeply nested backlinked subdocument', async function () {
    const instance = [{ '@type': 'Doc', '@capture': 'doc1' },
      { '@type': 'Subdoc', '@capture': 'doc2', '@linked-by': { '@ref': 'doc1', '@property': 'subdoc' } },
      { '@type': 'Subdoc', '@linked-by': { '@ref': 'doc2', '@property': 'subdoc' } },
    ]

    const r = await document.insert(agent, { instance })
    const docId = r.body[0]
    const subdocId1 = r.body[1]
    const subdocId2 = r.body[2]

    expect(subdocId1).to.match(new RegExp(`^${docId}/subdoc/Subdoc/.*`))
    expect(subdocId2).to.match(new RegExp(`^${subdocId1}/subdoc/Subdoc/.*`))
  })

  it('succeed on a backlinked subdocument with a future ref', async function () {
    const instance = [{ '@type': 'Subdoc', '@linked-by': { '@ref': 'doc', '@property': 'subdoc' } },
      { '@type': 'Doc', '@capture': 'doc' },
    ]

    const r = await document.insert(agent, { instance })
    const subdocId = r.body[0]
    const docId = r.body[1]

    expect(subdocId).to.match(new RegExp(`^${docId}/subdoc/Subdoc/.*`))
  })

  it('fails on a backlinked subdocument cycle', async function () {
    const instance = [{ '@type': 'Subdoc', '@capture': 'doc1', '@linked-by': { '@ref': 'doc2', '@property': 'subdoc' } },
      { '@type': 'Subdoc', '@capture': 'doc2', '@linked-by': { '@ref': 'doc1', '@property': 'subdoc' } },
    ]

    const r = await document.insert(agent, { instance }).unverified()
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:NotAllCapturesFound')
  })

  it('succeeds on a backlinked document cycle', async function () {
    const instance = [{ '@type': 'CycleDoc', '@capture': 'doc1', '@linked-by': { '@ref': 'doc2', '@property': 'other' } },
      { '@type': 'CycleDoc', '@capture': 'doc2', '@linked-by': { '@ref': 'doc1', '@property': 'other' } },
    ]

    await document.insert(agent, { instance })

    const r = await document.get(agent, { query: { as_list: true } })
    const doc1 = r.body[0]
    const doc2 = r.body[1]

    expect(doc1.other[0]).to.equal(doc2['@id'])
    expect(doc2.other[0]).to.equal(doc1['@id'])
  })

  it('fails to replace a document with backlinks', async function (){
    const instance = [{ '@type': 'CycleDoc' }]

    await document.insert(agent, { instance })
    const r = await document.insert(agent, { instance })
    const [id] = r.body
    const instance2 = [{ '@type': 'CycleDoc', '@id' : id, '@linked-by': { '@id': id, '@property': 'other' } }]
    const r2 = await document.replace(agent, { instance : instance2 }).unverified()
    expect(r2.status).to.equal(400)
    expect(r2.body['api:error']['@type']).to.equal('api:LinksInReplaceError')
  })

})
