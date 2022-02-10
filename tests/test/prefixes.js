const { expect } = require('chai')
const { Agent, db, endpoint } = require('../lib')

describe('prefixes', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    const defaults = agent.defaults()
    const { path } = endpoint.db(defaults)
    await db.create(agent, path, {
      prefixes: {
        '@base': 'http://somewhere.for.now/document/',
        '@schema': 'http://somewhere.for.now/schema#',
      },
    }).then(db.verifyCreateSuccess)
  })

  after(async function () {
    const defaults = agent.defaults()
    const { path } = endpoint.db(defaults)
    await db.del(agent, path)
  })

  it('fetches prefixes succesfully', async function () {
    const defaults = agent.defaults()
    const r = await agent.get(`/api/prefixes/admin/${defaults.dbName}`)
    expect(r.status).to.equal(200)
    expect(r.body).to.include(
      {
        '@base': 'http://somewhere.for.now/document/',
        '@schema': 'http://somewhere.for.now/schema#',
        '@type': 'Context',
      },
    )
  })
})
