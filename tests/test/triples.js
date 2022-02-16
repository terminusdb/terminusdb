const { expect } = require('chai')
const { Agent, db, endpoint, triples, util } = require('../lib')

describe('triples', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('passes _system schema', async function () {
    const { path } = endpoint.triplesSystem()
    const r = await agent.get(path)
    expect(r.status).to.equal(200)
  })

  it('fails with bad descriptor', async function () {
    const descriptor = util.randomString()
    const { path } = endpoint.triples(descriptor)
    const r = await agent.get(path).then(triples.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BadAbsoluteGraphDescriptor')
    expect(r.body['api:error']['api:absolute_graph_descriptor']).to.equal(descriptor)
  })

  it('passes put twice', async function () {
    const { path: dbPath } = endpoint.db(agent.defaults())
    const { path: triplesPath } = endpoint.triplesBranch(agent.defaults())
    // Create a database
    await db.create(agent, dbPath).then(db.verifyCreateSuccess)
    const body = { commit_info: { author: 'a', message: 'm' } }
    body.turtle = `
      @prefix layer: <http://terminusdb.com/schema/layer#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      layer:LayerIdRestriction a owl:Restriction.`
    // Put the first triple
    await agent.put(triplesPath).send(body).then(triples.verifyInsertSuccess)
    body.turtle = `
      @prefix layer: <http://terminusdb.com/schema/layer#> .
      @prefix owl: <http://www.w3.org/2002/07/owl#> .
      layer:LayerIdRestriction2 a owl:Restriction.`
    // Put the second triple
    await agent.put(triplesPath).send(body).then(triples.verifyInsertSuccess)
    // Delete the database
    await db.del(agent, dbPath).then(db.verifyDeleteSuccess)
  })

  // TODO: Create test for this, it responds with an application/json type now... which it isn't.
  // See issue: https://github.com/terminusdb/terminusdb/issues/981
  it('responds with a turtle mimetype', async function () {
    this.skip()
  })

  it('responds with proper status code on anonymous request', async function () {
    this.skip()
  })
})
