const { expect } = require('chai')
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

  it('passes replace schema with string literals', async function () {
    // Create a database
    await db.create(agent)
    // Put the first triple
    const turtle = `

    @base <terminusdb:///schema#> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    @prefix woql: <http://terminusdb.com/schema/woql#> .
    @prefix json: <http://terminusdb.com/schema/json#> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
    @prefix xdd: <http://terminusdb.com/schema/xdd#> .
    @prefix vio: <http://terminusdb.com/schema/vio#> .
    @prefix sys: <http://terminusdb.com/schema/sys#> .
    @prefix api: <http://terminusdb.com/schema/api#> .
    @prefix owl: <http://www.w3.org/2002/07/owl#> .
    @prefix doc: <data/> .

    <schema#Philosopher>
      a sys:Class ;
      <schema#name> xsd:string .
    <terminusdb://context>
      a sys:Context ;
      sys:base "terminusdb:///data/" ;
      sys:schema "terminusdb:///schema#" .
    `
    await triples.replaceIntoBranch(agent, turtle, { graph: 'schema' })
    // Delete the database
    await db.delete(agent)
  })

  it('passes insert with trig', async function () {
    // Create a database
    await db.create(agent, { schema: false })
    const trig = `
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
    @prefix swp: <http://www.w3.org/2004/03/trix/swp-1/> .
    @prefix dc: <http://purl.org/dc/elements/1.1/> .
    @prefix ex: <http://www.example.org/vocabulary#> .
    @prefix : <http://www.example.org/exampleDocument#> .

    :G1 { :Monica ex:name "Monica Murphy" .
          :Monica ex:homepage <http://www.monicamurphy.org> .
          :Monica ex:email <mailto:monica@monicamurphy.org> .
          :Monica ex:hasSkill ex:Management }

    :G2 { :Monica rdf:type ex:Person .
          :Monica ex:hasSkill ex:Programming }

    :G3 { :G1 swp:assertedBy _:w1 .
          _:w1 swp:authority :Chris .
          _:w1 dc:date "2003-10-02"^^xsd:date .
          :G2 swp:quotedBy _:w2 .
          :G3 swp:assertedBy _:w2 .
          _:w2 dc:date "2003-09-03"^^xsd:date .
          _:w2 swp:authority :Chris .
          :Chris rdf:type ex:Person .
          :Chris ex:email <mailto:chris@bizer.de> }
  `
    const r = await triples.insertIntoBranch(agent, trig)
    expect(r.body).to.deep.equal({ '@type': 'api:TriplesInsertResponse', 'api:status': 'api:success' })
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
