const { expect } = require('chai')
const { Agent, endpoint } = require('../lib')

describe('diff', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('gets a value diff', async function () {
    const { path } = endpoint.diff(agent.defaults())
    const r = await agent
      .post(path)
      .send({ before: 'asdf', after: 'fdsa' })
    expect(r.status).to.equal(200)
    expect(r.body).to.deep.equal({ '@after': 'fdsa', '@before': 'asdf', '@op': 'SwapValue' })
  })

  it('patch a diff', async function () {
    const { path: diffPath } = endpoint.diff(agent.defaults())
    const { path: patchPath } = endpoint.patch(agent.defaults())
    const before = { asdf: 'fdsa' }
    const after = { asdf: 'blarf' }
    const r = await agent
      .post(diffPath)
      .send({ before: before, after: after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent
      .post(patchPath)
      .send({ before: before, patch: patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })

  it('patch a deep diff', async function () {
    const { path: diffPath } = endpoint.diff(agent.defaults())
    const { path: patchPath } = endpoint.patch(agent.defaults())
    const before = { asdf: { bar: 'fdsa' } }
    const after = { asdf: { bar: 'blarf' } }
    const r = await agent
      .post(diffPath)
      .send({ before: before, after: after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent
      .post(patchPath)
      .send({ before: before, patch: patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })

  it('patch a deep list diff', async function () {
    const { path: diffPath } = endpoint.diff(agent.defaults())
    const { path: patchPath } = endpoint.patch(agent.defaults())
    const before = { asdf: { bar: [0, 1, 2] } }
    const after = { asdf: { bar: [0, 1] } }
    const r = await agent
      .post(diffPath)
      .send({ before: before, after: after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent
      .post(patchPath)
      .send({ before: before, patch: patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })

  it('patch a deep list deep diff', async function () {
    const { path: diffPath } = endpoint.diff(agent.defaults())
    const { path: patchPath } = endpoint.patch(agent.defaults())
    const before = { asdf: { bar: [{ baz: 'quux' }] } }
    const after = { asdf: { bar: [{ baz: 'quuz' }] } }
    const r = await agent
      .post(diffPath)
      .send({ before: before, after: after })
    expect(r.status).to.equal(200)
    const patch = r.body
    const r2 = await agent
      .post(patchPath)
      .send({ before: before, patch: patch })
    expect(r2.status).to.equal(200)
    expect(r2.body).to.deep.equal(after)
  })

  it('gets a table diff', async function () {
    const { path } = endpoint.diff(agent.defaults())
    const r = await agent
      .post(path)
      .send({
        before: [['asdf', 'quux'], ['fdsa', 'baaz']],
        after: [['asdf', 'quux'], ['fdsa', 'bar']],
      })
    expect(r.status).to.equal(200)

    const tableDiff = {
      '@op': 'ModifyTable',
      copies: [
        {
          '@at': { '@height': 1, '@width': 2, '@x': 0, '@y': 0 },
          '@value': [['asdf', "quux"]],
        },
      ],
      deletes: [
        {
          '@at': { '@height': 1, '@width': 1, '@x': 1, '@y': 1 },
          '@value': [['baaz']],
        },
        {
          '@at': { '@height': 1, '@width': 1, '@x': 0, '@y': 1 },
          '@value': [['fdsa']],
        },
      ],
      dimensions: {
        '@after': [2, 2],
        '@before': [2, 2],
      },
      inserts: [{
        '@at': { '@height': 1, '@width': 1, '@x': 0, '@y': 1 },
        '@value': [['fdsa']],
      },
      {
        '@at': { '@height': 1, '@width': 1, '@x': 1, '@y': 1 },
        '@value': [['bar']],
      }],
      moves: [],
    }
    expect(r.body).to.deep.equal(tableDiff)
  })

  it('gets a table diff with move', async function () {
    const { path } = endpoint.diff(agent.defaults())
    const r = await agent
      .post(path)
      .send({
        before: [['asdf', 'quux'],
          ['fdsa', 'baaz']],
        after: [['fdsa', 'baaz'],
          ['asdf', 'quux']],
      })
    expect(r.status).to.equal(200)

    const tableDiff = {
      '@op': 'ModifyTable',
      copies: [],
      deletes: [],
      dimensions: { '@after': [2, 2], '@before': [2, 2] },
      inserts: [],
      moves: [{
        '@from': { '@height': 1, '@width': 2, '@x': 0, '@y': 0 },
        '@to': { '@height': 1, '@width': 2, '@x': 0, '@y': 1 },
        '@value': [['asdf', 'quux']],
      },
      {
        '@from': { '@height': 1, '@width': 2, '@x': 0, '@y': 1 },
        '@to': { '@height': 1, '@width': 2, '@x': 0, '@y': 0 },
        '@value': [['fdsa', 'baaz']],
      },
      ],
    }
    expect(r.body).to.deep.equal(tableDiff)
  })
})
