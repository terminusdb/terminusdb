const { expect } = require('chai')
const { Agent, api } = require('../lib')

describe('diff', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('gets a value diff', async function () {
    const path = api.path.diff(agent)
    const r = await agent.post(path).send({ before: 'asdf', after: 'fdsa' })
    expect(r.status).to.equal(200)
    expect(r.body).to.deep.equal({ '@after': 'fdsa', '@before': 'asdf', '@op': 'SwapValue' })
  })

  it('patch a diff', async function () {
    const diffPath = api.path.diff(agent)
    const patchPath = api.path.patch(agent)
    const before = { asdf: 'fdsa' }
    const after = { asdf: 'blarf' }
    const r = await agent.post(diffPath).send({ before, after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent.post(patchPath).send({ before, patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })

  it('patch a deep diff', async function () {
    const diffPath = api.path.diff(agent)
    const patchPath = api.path.patch(agent)
    const before = { asdf: { bar: 'fdsa' } }
    const after = { asdf: { bar: 'blarf' } }
    const r = await agent.post(diffPath).send({ before, after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent.post(patchPath).send({ before, patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })

  it('patch a deep list diff', async function () {
    const diffPath = api.path.diff(agent)
    const patchPath = api.path.patch(agent)
    const before = { asdf: { bar: [0, 1, 2] } }
    const after = { asdf: { bar: [0, 1] } }
    const r = await agent.post(diffPath).send({ before, after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent.post(patchPath).send({ before, patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })

  it('patch a deep list deep diff', async function () {
    const diffPath = api.path.diff(agent)
    const patchPath = api.path.patch(agent)
    const before = { asdf: { bar: [{ baz: 'quux' }] } }
    const after = { asdf: { bar: [{ baz: 'quuz' }] } }
    const r = await agent.post(diffPath).send({ before, after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent.post(patchPath).send({ before, patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })
})
