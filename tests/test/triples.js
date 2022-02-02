const { expect } = require('chai')
const { Agent, db, endpoint } = require('../lib')

describe('triples', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('responds with triples on system schema', async function () {
    const { path } = endpoint.triples({ dbName: '_system', graph: 'schema' })
    const r = await agent.get(path)
    expect(r.status).to.equal(200)
  })

  it('fails with an error when submitting an invalid descriptor', async function () {
    const { path } = endpoint.triples({ dbName: 'nonsense', graph: '' })
    const r = await agent.get(path)
    expect(r.status).to.equal(400)
    expect(r.body['api:status']).to.equal('api:failure')
  })

  it('fails with an error when submitting a bad descriptor', async function () {
    const { path } = endpoint.triples({ dbName: 'admin/fdsa', graph: '' })
    const r = await agent.get(path)
    expect(r.status).to.equal(400)
    expect(r.body['api:status']).to.equal('api:failure')
  })

  it('handles two triple requests on new db', async function () {
    const { orgName, dbName } = agent.defaults()
    const { path } = endpoint.triples({
      dbName: 'admin/' + dbName,
      graph: 'local/branch/main/instance',
    })
    await db.create(agent, endpoint.db({
      orgName,
      dbName,
    }).path)
    const turtle = `
    @prefix layer: <http://terminusdb.com/schema/layer#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    layer:LayerIdRestriction a owl:Restriction.`
    const r = await agent.put(path).send({
      commit_info: { author: 'TestSuite', message: 'Testing Turtle triples' },
      turtle: turtle,
    })
    expect(r.status).to.equal(200)
    const secondTurtle = `
    @prefix layer: <http://terminusdb.com/schema/layer#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    layer:LayerIdRestriction a owl:Restriction.`
    const secondReq = await agent.put(path).send({
      commit_info: { author: 'TestSuite', message: 'Testing Turtle triples 2' },
      turtle: secondTurtle,
    })
    expect(secondReq.status).to.equal(200)
  })

  // TODO: Create test for this, it responds with an application/json type now... which it isn't.
  it('responds with a turtle mimetype')

  it('responds with proper status code on anonymous request')
})
