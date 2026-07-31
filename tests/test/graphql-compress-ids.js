const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

function createGraphQLClient(agent, compressIds) {
  const path = api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
  const base = agent.baseUrl
  const uri = compressIds === null
    ? `${base}${path}`
    : `${base}${path}?compress_ids=${compressIds}`

  const httpLink = new HttpLink({ uri, fetch })
  const authMiddleware = new ApolloLink((operation, forward) => {
    operation.setContext(({ headers = {} }) => ({
      headers: {
        ...headers,
        authorization: util.authorizationHeader(agent),
      },
    }))
    return forward(operation)
  })

  const ComposedLink = concat(authMiddleware, httpLink)
  return new ApolloClient({
    cache: new InMemoryCache({ addTypename: false }),
    link: ComposedLink,
  })
}

async function rawGraphQL(agent, compressIds, query) {
  const path = api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
  const base = agent.baseUrl
  const uri = compressIds === null
    ? `${base}${path}`
    : `${base}${path}?compress_ids=${compressIds}`
  const response = await fetch(uri, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': util.authorizationHeader(agent),
    },
    body: JSON.stringify({ query }),
  })
  return { status: response.status, body: await response.json() }
}

const schema = [{
  '@type': '@context',
  '@base': 'terminusdb:///data/',
  '@schema': 'terminusdb:///schema#',
  prefix: 'http://prefix.com/',
}, {
  '@type': 'Class',
  '@id': 'Person',
  '@key': { '@type': 'Lexical', '@fields': ['name'] },
  name: 'xsd:string',
  age: 'xsd:integer',
}]

describe('GraphQL compress_ids', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    await document.insert(agent, { schema, fullReplace: true })
    await document.insert(agent, { instance: { '@type': 'Person', name: 'Alice', age: 30 } })
  })

  after(async function () {
    await db.delete(agent)
  })

  //
  // Tests 1-4: _id field resolution
  //

  it('_id returns compressed ID when compress_ids=true', async function () {
    const client = createGraphQLClient(agent, true)
    const QUERY = gql`
      query {
        Person {
          _id
          name
        }
      }`
    const result = await client.query({ query: QUERY })
    expect(result.data.Person).to.have.lengthOf(1)
    expect(result.data.Person[0]._id).to.equal('Person/Alice')
  })

  it('_id returns full IRI when compress_ids=false', async function () {
    const client = createGraphQLClient(agent, false)
    const QUERY = gql`
      query {
        Person {
          _id
          name
        }
      }`
    const result = await client.query({ query: QUERY })
    expect(result.data.Person).to.have.lengthOf(1)
    expect(result.data.Person[0]._id).to.equal('terminusdb:///data/Person/Alice')
  })

  it('_id returns compressed ID when compress_ids defaults (no param)', async function () {
    const client = createGraphQLClient(agent, null)
    const QUERY = gql`
      query {
        Person {
          _id
          name
        }
      }`
    const result = await client.query({ query: QUERY })
    expect(result.data.Person).to.have.lengthOf(1)
    expect(result.data.Person[0]._id).to.equal('Person/Alice')
  })

  it('rejects invalid compress_ids value with 400', async function () {
    const { status, body } = await rawGraphQL(agent, 'notabool', '{ Person { _id name } }')
    expect(status).to.equal(400)
    expect(body.errors).to.exist
    expect(body.errors[0].message).to.match(/compress_ids/)
  })

  //
  // Tests 5-8: _insertDocuments mutation
  //

  it('_insertDocuments returns compressed IDs when compress_ids=true', async function () {
    const { body } = await rawGraphQL(agent, true,
      `mutation { _insertDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"Bob\\",\\"age\\":25}") }`)
    expect(body.data._insertDocuments).to.include('Person/Bob')
  })

  it('_insertDocuments returns full IRIs when compress_ids=false', async function () {
    const { body } = await rawGraphQL(agent, false,
      `mutation { _insertDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"Charlie\\",\\"age\\":40}") }`)
    expect(body.data._insertDocuments).to.include('terminusdb:///data/Person/Charlie')
  })

  it('_insertDocuments returns compressed IDs when compress_ids defaults', async function () {
    const { body } = await rawGraphQL(agent, null,
      `mutation { _insertDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"Dave\\",\\"age\\":50}") }`)
    expect(body.data._insertDocuments).to.include('Person/Dave')
  })

  it('_insertDocuments returns compressed IDs with default param after explicit false query', async function () {
    await rawGraphQL(agent, false,
      `mutation { _insertDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"Eve\\",\\"age\\":35}") }`)

    const client = createGraphQLClient(agent, null)
    const QUERY = gql`
      query {
        Person(orderBy: {name: ASC}) {
          _id
          name
        }
      }`
    const result = await client.query({ query: QUERY })
    const eve = result.data.Person.find(p => p.name === 'Eve')
    expect(eve._id).to.equal('Person/Eve')
  })

  //
  // Tests 9-10: _replaceDocuments mutation
  //

  it('_replaceDocuments returns compressed IDs when compress_ids=true', async function () {
    const { body } = await rawGraphQL(agent, true,
      `mutation { _replaceDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"Alice\\",\\"age\\":31}") }`)
    expect(body.data._replaceDocuments).to.include('Person/Alice')
  })

  it('_replaceDocuments returns full IRIs when compress_ids=false', async function () {
    const { body } = await rawGraphQL(agent, false,
      `mutation { _replaceDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"Bob\\",\\"age\\":26}") }`)
    expect(body.data._replaceDocuments).to.include('terminusdb:///data/Person/Bob')
  })

  //
  // Tests 11-12: _deleteDocuments mutation
  //

  it('_deleteDocuments returns compressed IDs when compress_ids=true', async function () {
    const { body: insertBody } = await rawGraphQL(agent, false,
      `mutation { _insertDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"ToDelete\\",\\"age\\":99}") }`)
    const fullId = insertBody.data._insertDocuments[0]

    const { body } = await rawGraphQL(agent, true,
      `mutation { _deleteDocuments(ids: ["${fullId}"]) }`)
    expect(body.data._deleteDocuments).to.include('Person/ToDelete')
  })

  it('_deleteDocuments returns full IRIs when compress_ids=false', async function () {
    const { body: insertBody } = await rawGraphQL(agent, false,
      `mutation { _insertDocuments(json: "{\\"@type\\":\\"Person\\",\\"name\\":\\"ToDelete2\\",\\"age\\":98}") }`)
    const fullId = insertBody.data._insertDocuments[0]

    const { body } = await rawGraphQL(agent, false,
      `mutation { _deleteDocuments(ids: ["${fullId}"]) }`)
    expect(body.data._deleteDocuments).to.include('terminusdb:///data/Person/ToDelete2')
  })
})
