const { Agent, api, db, triples, util } = require('../lib')

describe('triples', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('passes get with _system schema', async function () {
    await triples.getFromSystem(agent)
  })

  it('fails get with bad graph descriptor', async function () {
    const descriptor = util.randomString()
    await triples
      .get(agent, { descriptor })
      .fails(api.error.badGraphDescriptor(descriptor))
  })

  it('passes insert twice', async function () {
    // Create a database
    await db.create(agent)
    // Put the first triple
    {
      const turtle = `
        @prefix layer: <http://terminusdb.com/schema/layer#> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        layer:LayerIdRestriction a owl:Restriction.`
      await triples.insertIntoBranch(agent, turtle)
    }
    // Put the second triple
    {
      const turtle = `
        @prefix layer: <http://terminusdb.com/schema/layer#> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        layer:LayerIdRestriction2 a owl:Restriction.`
      await triples.insertIntoBranch(agent, turtle)
    }
    // Delete the database
    await db.delete(agent)
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
