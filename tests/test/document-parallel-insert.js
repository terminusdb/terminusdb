const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')

/*
 * Pins the semantics of concurrent, unpinned document inserts on a single
 * branch. Motivated by a benchmark that reported "Schema check failure"
 * under parallel load: the failures were correct referential-integrity
 * rejections caused by batches that referenced documents belonging to a
 * different (still in-flight) commit. Each commit is validated against
 * committed state plus its own additions only.
 */
describe('document-parallel-insert', function () {
  let agent

  const batch = (tag) => [
    { '@type': 'Parent', label: `parent-${tag}` },
    { '@type': 'Child', label: `child-${tag}-a`, parent: `Parent/parent-${tag}` },
    { '@type': 'Child', label: `child-${tag}-b`, parent: `Parent/parent-${tag}` },
  ]

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    const schema = [
      util.defaultContext,
      {
        '@type': 'Class',
        '@id': 'Parent',
        '@key': { '@type': 'Lexical', '@fields': ['label'] },
        label: 'xsd:string',
      },
      {
        '@type': 'Class',
        '@id': 'Child',
        '@key': { '@type': 'Lexical', '@fields': ['label'] },
        label: 'xsd:string',
        parent: 'Parent',
      },
    ]
    await document.insert(agent, { schema, fullReplace: true })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('commits concurrent unpinned inserts of self-contained batches', async function () {
    const workers = 5
    const responses = await Promise.all(
      Array.from({ length: workers }, (_, w) =>
        document.insert(agent, { instance: batch(`conc-${w}`) }).unverified()),
    )
    for (const response of responses) {
      expect(response.status).to.equal(200)
    }
    const result = await document.get(agent, { query: { as_list: true } })
    expect(result.body.length).to.equal(3 * workers)
  })

  it('resolves references to documents within the same commit', async function () {
    // A batch is self-contained: links may target documents that arrive in
    // the same commit, regardless of ordering inside the batch.
    const instance = [
      { '@type': 'Child', label: 'child-first', parent: 'Parent/parent-last' },
      { '@type': 'Parent', label: 'parent-last' },
    ]
    await document.insert(agent, { instance })
  })

  it('rejects a batch that references an uncommitted document', async function () {
    const instance = [
      { '@type': 'Child', label: 'orphan', parent: 'Parent/never-committed' },
    ]
    const witness = {
      '@type': 'references_untyped_object',
      subject: 'terminusdb:///data/Child/orphan',
      predicate: 'terminusdb:///schema#parent',
      object: 'terminusdb:///data/Parent/never-committed',
    }
    await document.insert(agent, { instance }).fails(api.error.schemaCheckFailure([witness]))
  })

  it('commits atomically: a failing batch leaves no documents behind', async function () {
    const instance = [
      { '@type': 'Parent', label: 'good-in-bad-batch' },
      { '@type': 'Child', label: 'bad-in-bad-batch', parent: 'Parent/still-missing' },
    ]
    const witness = {
      '@type': 'references_untyped_object',
      subject: 'terminusdb:///data/Child/bad-in-bad-batch',
      predicate: 'terminusdb:///schema#parent',
      object: 'terminusdb:///data/Parent/still-missing',
    }
    await document
      .insert(agent, { instance })
      .fails(api.error.schemaCheckFailure([witness]))
    const r = await document
      .get(agent, { query: { id: 'Parent/good-in-bad-batch' } })
      .unverified()
    expect(r.status).to.equal(404)
    expect(r.body['api:error']['@type']).to.equal('api:DocumentNotFound')
  })

  it('succeeds on retry once the referenced document has committed', async function () {
    // The failure above is ordering, not corruption: committing the missing
    // Parent first makes the previously rejected batch valid.
    await document.insert(agent, { instance: { '@type': 'Parent', label: 'never-committed' } })
    const instance = [
      { '@type': 'Child', label: 'orphan', parent: 'Parent/never-committed' },
    ]
    await document.insert(agent, { instance })
  })
})
