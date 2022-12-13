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
  }, {
    '@id': 'Rocks',
    '@type': 'Enum',
    '@value': ['Big', 'Medium', 'Small'],
  }, {
    '@id': 'Everything',
    '@type': 'Class',
    anySimpleType: 'xsd:anySimpleType',
    string: 'xsd:string',
    boolean: 'xsd:boolean',
    decimal: 'xsd:decimal',
    float: 'xsd:float',
    time: 'xsd:time',
    date: 'xsd:date',
    dateTime: 'xsd:dateTime',
    dateTimeStamp: 'xsd:dateTimeStamp',
    gYear: 'xsd:gYear',
    gMonth: 'xsd:gMonth',
    gDay: 'xsd:gDay',
    gYearMonth: 'xsd:gYearMonth',
    duration: 'xsd:duration',
    yearMonthDuration: 'xsd:yearMonthDuration',
    dayTimeDuration: 'xsd:dayTimeDuration',
    byte: 'xsd:byte',
    short: 'xsd:short',
    int: 'xsd:int',
    long: 'xsd:long',
    unsignedByte: 'xsd:unsignedByte',
    unsignedShort: 'xsd:unsignedShort',
    unsignedInt: 'xsd:unsignedInt',
    unsignedLong: 'xsd:unsignedLong',
    integer: 'xsd:integer',
    positiveInteger: 'xsd:positiveInteger',
    negativeInteger: 'xsd:negativeInteger',
    nonPositiveInteger: 'xsd:nonPositiveInteger',
    nonNegativeInteger: 'xsd:nonNegativeInteger',
    base64nary: 'xsd:base64Binary',
    hexBinary: 'xsd:hexBinary',
    anyURI: 'xsd:anyURI',
    language: 'xsd:language',
    normalizedString: 'xsd:normalizedString',
    token: 'xsd:token',
    NMTOKEN: 'xsd:NMTOKEN',
    Name: 'xsd:Name',
    NCName: 'xsd:NCName',
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

    it('insert and retrieve everything', async function () {
      const everything = {
        '@type': 'Everything',
        anySimpleType: 3,
        string: 'string',
        boolean: true,
        decimal: 3.2,
        float: 3.2,
        time: '23:34:43.0003Z',
        date: '2021-03-05',
        dateTime: '2021-03-05T23:34:43.0003Z',
        dateTimeStamp: '2021-03-05T23:34:43.0003Z',
        gYear: '-32',
        gMonth: '--11',
        gDay: '---29',
        gYearMonth: '1922-03',
        duration: 'P3Y2DT7M',
        yearMonthDuration: 'P3Y7M',
        dayTimeDuration: 'P1DT10H7M12S',
        byte: -8,
        short: -10,
        int: -32,
        long: -532,
        unsignedByte: 3,
        unsignedShort: 5,
        unsignedInt: 8,
        unsignedLong: 10,
        integer: 20,
        positiveInteger: '2342423',
        negativeInteger: '-2348982734',
        nonPositiveInteger: '-334',
        nonNegativeInteger: '3243323',
        base64nary: 'VGhpcyBpcyBhIHRlc3Q=',
        hexBinary: '5468697320697320612074657374',
        anyURI: 'http://this.com',
        language: 'en',
        normalizedString: 'norm',
        token: 'token',
        NMTOKEN: 'NMTOKEN',
        Name: 'Name',
        NCName: 'NCName',
      }
      const res = await document.insert(agent, { instance: everything }).unverified()
      const id = res.body[0]
      const QUERY_EVERYTHING = gql`
query EverythingQuery {
   Everything {
        anySimpleType
        string
        boolean
        decimal
        float
        time
        date
        dateTime
        dateTimeStamp
        gYear
        gMonth
        gDay
        gYearMonth
        duration
        yearMonthDuration
        dayTimeDuration
        byte
        short
        int
        long
        unsignedByte
        unsignedShort
        unsignedInt
        unsignedLong
        integer
        positiveInteger
        negativeInteger
        nonPositiveInteger
        nonNegativeInteger
        base64nary
        hexBinary
        anyURI
        language
        normalizedString
        token
        NMTOKEN
        Name
        NCName
   }
}
`
      const r = await client.query({ query: QUERY_EVERYTHING })
      expect(r.data.Everything).to.deep.equal([
        {
          anySimpleType: '"3"',
          string: 'string',
          boolean: true,
          decimal: '3.2',
          float: 3.200000047683716,
          time: '23:34:43Z',
          date: '2021-03-05',
          dateTime: '2021-03-05T23:34:43.000030Z',
          dateTimeStamp: '2021-03-05T23:34:43.000030Z',
          gYear: '-032',
          gMonth: '--11',
          gDay: '---29',
          gYearMonth: '1922-03',
          duration: 'P3Y2DT7M',
          yearMonthDuration: 'P3Y7M',
          dayTimeDuration: 'P1DT10H7M12S',
          byte: -8,
          short: -10,
          int: -32,
          long: '-532',
          unsignedByte: 3,
          unsignedShort: 5,
          unsignedInt: '8',
          unsignedLong: '10',
          integer: '20',
          positiveInteger: '2342423',
          negativeInteger: '-2348982734',
          nonPositiveInteger: '-334',
          nonNegativeInteger: '3243323',
          base64nary: 'VGhpcyBpcyBhIHRlc3Q=',
          hexBinary: '5468697320697320612074657374',
          anyURI: 'http://this.com',
          language: 'en',
          normalizedString: 'norm',
          token: 'token',
          NMTOKEN: 'NMTOKEN',
          Name: 'Name',
          NCName: 'NCName',
        },
      ])
    })
  })
})
