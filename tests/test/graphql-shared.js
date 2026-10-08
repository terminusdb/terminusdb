const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

describe('GraphQL @shared documents', function () {
  let agent
  let client

  const schema = [
    {
      '@id': 'City',
      '@type': 'Class',
      name: 'xsd:string',
      routes: { '@type': 'Set', '@class': 'Route' },
    },
    {
      '@id': 'Route',
      '@type': 'Class',
      '@shared': [],
      '@key': { '@type': 'Random' },
      name: 'xsd:string',
      destination: 'City',
    },
  ]

  const cities = [
    {
      '@type': 'City',
      '@id': 'City/Tokyo',
      name: 'Tokyo',
      routes: { '@type': 'Route', name: 'tomei-expwy-west', destination: 'City/Nagoya' },
    },
    {
      '@type': 'City',
      '@id': 'City/Nagoya',
      name: 'Nagoya',
      routes: { '@type': 'Route', name: 'tomei-expwy-east', destination: 'City/Tokyo' },
    },
  ]

  before(async function () {
    agent = new Agent().auth()
    const path = api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
    const base = agent.baseUrl
    const uri = `${base}${path}`

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
    const cache = new InMemoryCache({ addTypename: false })
    client = new ApolloClient({ cache, link: ComposedLink })

    await db.create(agent)
    await document.insert(agent, { schema })
    await document.insert(agent, { instance: cities })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('queries through a set of @shared documents', async function () {
    const Q = gql`query { City(orderBy: { name: ASC }) { name routes { name destination { name } } } }`
    const result = await client.query({ query: Q })
    expect(result.data.City).to.deep.equal([
      { name: 'Nagoya', routes: [{ name: 'tomei-expwy-east', destination: { name: 'Tokyo' } }] },
      { name: 'Tokyo', routes: [{ name: 'tomei-expwy-west', destination: { name: 'Nagoya' } }] },
    ])
  })

  it('queries @shared documents directly', async function () {
    const Q = gql`query { Route(orderBy: { name: ASC }) { name destination { name } } }`
    const result = await client.query({ query: Q })
    expect(result.data.Route).to.deep.equal([
      { name: 'tomei-expwy-east', destination: { name: 'Tokyo' } },
      { name: 'tomei-expwy-west', destination: { name: 'Nagoya' } },
    ])
  })
})
