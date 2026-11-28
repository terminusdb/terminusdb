const { expect } = require('chai')
const { Agent } = require('../lib')

describe('rebase-conflict-resolution', function () {
  let agent
  let dbName

  before(async function () {
    agent = new Agent().auth()
    dbName = agent.dbName
  })

  const schema = [
    {
      '@type': 'Class',
      '@id': 'Thing',
      '@key': { '@type': 'Lexical', '@fields': ['k'] },
      '@base': 'Thing/',
      k: 'xsd:string',
      label: 'xsd:string',
      tags: { '@type': 'Set', '@class': 'xsd:string' },
    },
  ]

  const base = { '@type': 'Thing', k: 't', label: 'base', tags: ['x'] }

  const request = promise => promise.ok(res => true)

  const setup = async (name, otherDoc, mainDoc, initial = base) => {
    const suffix = `${name}-${Math.random().toString(36).slice(2, 8)}`
    const local = `${dbName}-${suffix}`
    await request(agent.post('/api/db/admin/' + local).send({ label: suffix }))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=schema&graph_type=schema`).send(schema))
    await request(agent.post(`/api/document/admin/${local}/local/branch/main?author=a&message=base`).send(initial))
    await request(agent.post(`/api/branch/admin/${local}/local/branch/other`).send({
      origin: `admin/${local}/local/branch/main`,
    }))
    await request(agent.put(`/api/document/admin/${local}/local/branch/other?author=a&message=other`).send(otherDoc))
    await request(agent.put(`/api/document/admin/${local}/local/branch/main?author=a&message=main`).send(mainDoc))
    return local
  }

  const rebase = (local, resolution) => {
    const body = {
      rebase_from: `admin/${local}/local/branch/main`,
      author: 'a',
    }
    if (resolution) body.conflict_resolution = resolution
    return request(agent.post(`/api/rebase/admin/${local}/local/branch/other`).send(body))
  }

  it('rejects divergent Set writes by default', async function () {
    this.timeout(60000)
    const local = await setup(
      'strict',
      { ...base, tags: ['x', 'y'] },
      { ...base, tags: ['x', 'z'] },
    )
    const result = await rebase(local)
    expect(result.status).to.equal(409)
    expect(result.body['api:error']['@type']).to.equal('api:RebaseMergeConflict')
    expect(result.body['api:error']['api:path']).to.deep.equal(['tags'])
    expect(result.body['api:message']).to.include('conflict_resolution')
    expect(result.body['api:error']).to.not.have.property('api:base')
    expect(result.body['api:error']).to.not.have.property('api:ours')
    expect(result.body['api:error']).to.not.have.property('api:theirs')
  })

  it('unions divergent Set writes only when merge is selected', async function () {
    this.timeout(60000)
    const local = await setup(
      'merge',
      { ...base, tags: ['x', 'y'] },
      { ...base, label: 'main label', tags: ['x', 'z'] },
    )
    const result = await rebase(local, 'merge')
    expect(result.status, `rebase status: ${JSON.stringify(result.body)}`).to.equal(200)
    const report = result.body['api:rebase_report'][0]['api:resolution_report']
    expect(report).to.deep.include({
      '@type': 'api:RebaseResolutionReport',
      'api:document': 'Thing/t',
      'api:path': ['tags'],
      'api:resolution': 'merge',
    })
    const doc = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Thing/t`))
    expect(doc.status).to.equal(200)
    expect(doc.body.tags.slice().sort()).to.deep.equal(['x', 'y', 'z'])
    expect(doc.body.label).to.equal('main label')
  })

  it('unions additions to an initially empty Set', async function () {
    this.timeout(60000)
    const emptyBase = { ...base, tags: [] }
    const local = await setup(
      'empty-set',
      { ...base, tags: ['y'] },
      { ...base, tags: ['z'] },
      emptyBase,
    )
    const result = await rebase(local, 'merge')
    expect(result.status, `rebase status: ${JSON.stringify(result.body)}`).to.equal(200)
    const doc = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Thing/t`))
    expect(doc.body.tags.slice().sort()).to.deep.equal(['y', 'z'])
  })

  it('keeps cardinality-one fields non-unionable under merge', async function () {
    this.timeout(60000)
    const local = await setup(
      'scalar',
      { ...base, label: 'other label' },
      { ...base, label: 'main label' },
    )
    const result = await rebase(local, 'merge')
    expect(result.status).to.equal(409)
    expect(result.body['api:error']['@type']).to.equal('api:RebaseMergeConflict')
    expect(result.body['api:error']['api:path']).to.deep.equal(['label'])
  })

  it('accepts per-document merge selection', async function () {
    this.timeout(60000)
    const local = await setup(
      'per-document',
      { ...base, tags: ['x', 'y'] },
      { ...base, tags: ['x', 'z'] },
    )
    const result = await rebase(local, {
      default: 'error',
      documents: { merge: ['Thing/t'] },
    })
    expect(result.status, `rebase status: ${JSON.stringify(result.body)}`).to.equal(200)
    const doc = await request(agent.get(`/api/document/admin/${local}/local/branch/other?id=Thing/t`))
    expect(doc.body.tags.slice().sort()).to.deep.equal(['x', 'y', 'z'])
  })
})
