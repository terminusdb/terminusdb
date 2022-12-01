const { expect } = require('chai')
const { Agent, api, db, document } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')

describe('GraphQL', function () {
  let agent
  let client

  const schema = [{
    '@id': 'Person',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    age: 'xsd:decimal',
    order: 'xsd:integer',
    friend: { '@type': 'Set', '@class': 'Person' },
    cat: { '@type': 'Set', '@class': 'Cat' },
  }, {
    '@id': 'Cat',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
  }]

  const aristotle = { '@type': 'Person', name: 'Aristotle', age: 61, order: 3, friend: ['Person/Plato'] }
  const plato = { '@type': 'Person', name: 'Plato', age: 80, order: 2, friend: ['Person/Aristotle'] }
  const socrates = { '@type': 'Person', name: 'Socrates', age: 71, order: 1, friend: ['Person/Plato'] }
  const kant = { '@type': 'Person', name: 'Immanuel Kant', age: 79, order: 3, friend: ['Person/Immanuel%20Kant'], cat: ['Cat/Toots'] }
  const popper = { '@type': 'Person', name: 'Karl Popper', age: 92, order: 5, cat: ['Cat/Pickles', 'Cat/Toots'] }
  const gödel = { '@type': 'Person', name: 'Kurt Gödel', age: 71, order: 5, friend: ['Person/Immanuel%20Kant'], cat: ['Cat/Pickles'] }

  const pickles = { '@type': 'Cat', name: 'Pickles' }
  const toots = { '@type': 'Cat', name: 'Toots' }

  const instances = [aristotle, plato, socrates, kant, popper, gödel, pickles, toots]

  before(async function () {
    /* GraphQL Boilerplate */
  /* Termius Boilerplate */
    agent = new Agent().auth()
    const path = api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName })
    const base = agent.baseUrl
    const uri = `${base}${path}`

    const httpLink = new HttpLink({ uri, fetch })
    const authMiddleware = new ApolloLink((operation, forward) => {
    // add the authorization to the headers
      operation.setContext(({ headers = {} }) => ({
        headers: {
          ...headers,
          authorization: 'Basic YWRtaW46cm9vdA==',
        },
      }))
      return forward(operation)
    })

    const ComposedLink = concat(authMiddleware, httpLink)

    const cache = new InMemoryCache({
      addTypename: false,
    })

    client = new ApolloClient({
      cache,
      link: ComposedLink,
    })

    await db.create(agent)

    await document.insert(agent, { schema })

    await document.insert(agent, { instance: instances })
  })

  after(async function () {
    // await db.delete(agent)
  })

  describe('queries', function () {
    it('basic data query', async function () {
      const PERSON_QUERY = gql`
 query PersonQuery {
    Person{
        name
        age
        order
    }
}`
      const result = await client.query({ query: PERSON_QUERY })

      expect(result.data.Person).to.deep.equal([
        { name: 'Aristotle', age: 61, order: '3' },
        { name: 'Immanuel Kant', age: 79, order: '3' },
        { name: 'Karl Popper', age: 92, order: '5' },
        { name: 'Kurt Gödel', age: 71, order: '5' },
        { name: 'Plato', age: 80, order: '2' },
        { name: 'Socrates', age: 71, order: '1' },
      ])
    })

    it('filter query', async function () {
      const FILTER_QUERY = gql`
 query PersonQuery {
    Person(filter: {name: {ge : "K"}, age: {ge : 30}}, orderBy : {order : ASC}){
        name
        age
        order
    }
}`
      const result = await client.query({ query: FILTER_QUERY })
      expect(result.data.Person).to.deep.equal([
        { name: 'Socrates', age: 71, order: '1' },
        { name: 'Plato', age: 80, order: '2' },
        { name: 'Karl Popper', age: 92, order: '5' },
        { name: 'Kurt Gödel', age: 71, order: '5' },
      ])
    })

    it('back-link query', async function () {
      const BACKLINK_QUERY = gql`
 query PersonQuery {
    Person(orderBy : {order : ASC}){
        name
        age
        order
        _friend_of_Person{
           name
        }
    }
}`
      const result = await client.query({ query: BACKLINK_QUERY })
      expect(result.data.Person).to.deep.equal([
        { name: 'Socrates', age: 71, order: '1', _friend_of_Person: [] },
        {
          name: 'Plato',
          age: 80,
          order: '2',
          _friend_of_Person: [
            {
              name: 'Aristotle',
            },
            {
              name: 'Socrates',
            },
          ],
        },
        {
          name: 'Aristotle',
          age: 61,
          order: '3',
          _friend_of_Person: [
            {
              name: 'Plato',
            },
          ],
        },
        {
          name: 'Immanuel Kant',
          age: 79,
          order: '3',
          _friend_of_Person: [{
            name: 'Immanuel Kant',
          },
          {
            name: 'Kurt Gödel',
          },
          ],
        },
        { name: 'Karl Popper', age: 92, order: '5', _friend_of_Person: [] },
        { name: 'Kurt Gödel', age: 71, order: '5', _friend_of_Person: [] },
      ])
    })

    it('path query', async function () {
      const PATH_QUERY = gql`
 query PersonQuery {
    Person(id: "terminusdb:///data/Person/Socrates", orderBy : {order : ASC}){
        _id
        name
        age
        order
        _path_to_Person(path: "friend+"){
           name
        }
    }
}`
      const result = await client.query({ query: PATH_QUERY })

      expect(result.data.Person[0]._path_to_Person).to.deep.equal(
        [
          {
            name: 'Plato',
          },
          {
            name: 'Aristotle',
          },
        ],
      )
    })

    it('path query backward and forward', async function () {
      const PATH_QUERY = gql`
 query PersonQuery {
    Person(id: "terminusdb:///data/Person/Immanuel%20Kant", orderBy : {order : ASC}){
        _id
        name
        age
        order
        _path_to_Cat(path: "(<friend)*,cat"){
           name
        }
    }
}`
      const result = await client.query({ query: PATH_QUERY })

      expect(result.data.Person[0]._path_to_Cat).to.deep.equal(
        [
          {
            name: 'Toots',
          },
          {
            name: 'Pickles',
          },
        ],
      )
    })

    it('graphql ids query', async function () {
      const PERSON_QUERY = gql`
 query PersonQuery {
    Person(ids : ["terminusdb:///data/Person/Immanuel%20Kant",
                  "terminusdb:///data/Person/Socrates"
                 ]){
        name
    }
}`
      const result = await client.query({ query: PERSON_QUERY })

      expect(result.data.Person).to.deep.equal([
        { name: 'Immanuel Kant' },
        { name: 'Socrates' },
      ])
    })
  })
})
