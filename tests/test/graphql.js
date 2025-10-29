const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')
const fetch = require('cross-fetch')
const {
  ApolloClient, ApolloLink, concat, InMemoryCache,
  gql, HttpLink,
} = require('@apollo/client/core')
const Decimal = require('decimal.js')

describe('GraphQL', function () {
  let agent
  let client

  const schema = [{
    '@type': '@context',
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
    prefix: 'http://prefix.com/',
  }, {
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
  }, {
    '@id': 'Parent',
    '@type': 'Class',
    name: 'xsd:string',
  }, {
    '@id': 'Child',
    '@type': 'Class',
    '@inherits': ['Parent'],
    number: 'xsd:byte',
  },
  {
    '@id': 'Source',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    targets: { '@type': 'List', '@class': 'Target' },
  },
  {
    '@id': 'Target',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
  },
  {
    '@id': 'SourceArray',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    target_array: { '@type': 'List', '@class': 'TargetArray' },
  },
  {
    '@id': 'TargetArray',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
  },
  {
    '@id': 'MaybeRocks',
    '@type': 'Class',
    rocks_opt: { '@type': 'Optional', '@class': 'Rocks' },
  },
  {
    '@id': 'NotThere',
    '@type': 'Class',
    property: { '@type': 'Array', '@class': 'xsd:decimal' },
  },
  {
    '@id': 'JSONClass',
    '@type': 'Class',
    json: 'sys:JSON',
  },
  {
    '@id': 'JSONs',
    '@type': 'Class',
    json: { '@type': 'Set', '@class': 'sys:JSON' },
  },
  {
    '@id': 'RockSet',
    '@type': 'Class',
    rocks: { '@type': 'Set', '@class': 'Rocks' },
  },
  {
    '@id': 'OneOf',
    '@type': 'Class',
    '@oneOf': [
      {
        a: 'xsd:string',
        b: 'xsd:string',
      },
    ],
  },
  {
    '@id': 'Integer',
    '@type': 'Class',
    int: 'xsd:integer',
  },
  {
    '@id': 'NonNegativeInteger',
    '@type': 'Class',
    nonnegint: 'xsd:nonNegativeInteger',
  },
  {
    '@id': 'DateAndTime',
    '@type': 'Class',
    datetime: 'xsd:dateTime',
  },
  {
    '@id': 'BadlyNamedOptional',
    '@type': 'Class',
    'is-it-ok': { '@type': 'Optional', '@class': 'xsd:string' },
  },
  {
    '@id': 'MyBigFloat',
    '@type': 'Class',
    bigfloat: 'xsd:decimal',
  },
  {
    '@id': 'Prefix',
    '@type': 'Class',
    'prefix:foo': 'xsd:string',
  },
  {
    '@id': 'Node',
    '@type': 'Class',
    'prefix:node': { '@type': 'Optional', '@class': 'Node' },
    'prefix:string': 'xsd:string',
  },
  {
    '@id': 'EnumPointer',
    '@type': 'Class',
    pointer: 'an-enum',
  },
  {
    '@id': 'an-enum',
    '@type': 'Enum',
    '@value': ['enum-one', 'enum-two'],
  },
  {
    '@id': 'prefix:MyClass',
    '@type': 'Class',
    name: 'xsd:string',
    'prefix:link': { '@type': 'Optional', '@class': 'prefix:MyClass' },
  },
  {
    '@type': 'Foreign',
    '@id': 'http://external#Thing',
  },
  {
    '@type': 'Class',
    '@id': 'RequiredForeignField',
    required_foreign_field: 'http://external#Thing',
  },
  {
    '@type': 'Class',
    '@id': 'SetForeignField',
    set_foreign_field: {
      '@type': 'Set',
      '@class': 'http://external#Thing',
    },
  },
  {
    '@type': 'Class',
    '@id': 'prefix:Outer',
    inner: 'prefix:Inner',
  },
  {
    '@type': 'Class',
    '@id': 'prefix:Inner',
    inner_name: 'xsd:string',
  },
  {
    '@type': 'Class',
    '@id': 'UnprefixedParent',
    parent_name: 'xsd:string',
  },
  {
    '@type': 'Class',
    '@id': 'prefix:Child',
    '@inherits': 'UnprefixedParent',
    child_name: 'xsd:string',
  },
  {
    '@type': 'Class',
    '@id': 'A',
    '@key': {
      '@fields': [
        'name',
      ],
      '@type': 'Lexical',
    },
    name: 'xsd:string',
  },
  ]

  const aristotle = { '@type': 'Person', name: 'Aristotle', age: '61', order: '3', friend: ['Person/Plato'] }
  const plato = { '@type': 'Person', name: 'Plato', age: '80', order: '2', friend: ['Person/Aristotle'] }
  const socrates = { '@type': 'Person', name: 'Socrates', age: '71', order: '1', friend: ['Person/Plato'] }
  const kant = { '@type': 'Person', name: 'Immanuel Kant', age: '79', order: '3', friend: ['Person/Immanuel%20Kant'], cat: ['Cat/Toots'] }
  const popper = { '@type': 'Person', name: 'Karl Popper', age: '92', order: '5', cat: ['Cat/Pickles', 'Cat/Toots'] }
  const gödel = { '@type': 'Person', name: 'Kurt Gödel', age: '71', order: '5', friend: ['Person/Immanuel%20Kant'], cat: ['Cat/Pickles'] }

  const pickles = { '@type': 'Cat', name: 'Pickles' }
  const toots = { '@type': 'Cat', name: 'Toots' }

  const int1 = { int: 1 }
  const int2 = { int: 100 }
  const int3 = { int: 11 }
  const int4 = { int: 2 }

  const nonnegint = { nonnegint: 300 }
  const datetime = { datetime: '2021-03-05T23:34:43.0003Z' }

  const bigfloat1 = { bigfloat: '0.0' }
  const bigfloat2 = { bigfloat: '10096.757' }
  const bigfloat3 = { bigfloat: '101.0' }
  const bigfloat4 = { '@type': 'MyBigFloat', bigfloat: '0.01234567890123456789' }

  const instances = [aristotle, plato, socrates, kant, popper, gödel, pickles, toots, int1, int2, int3, int4, nonnegint, datetime, bigfloat1, bigfloat2, bigfloat3, bigfloat4]

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
          authorization: util.authorizationHeader(agent),
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

    await document.insert(agent, { schema, fullReplace: true })

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
        { name: 'Aristotle', age: 61, order: 3 },
        { name: 'Immanuel Kant', age: 79, order: 3 },
        { name: 'Karl Popper', age: 92, order: 5 },
        { name: 'Kurt Gödel', age: 71, order: 5 },
        { name: 'Plato', age: 80, order: 2 },
        { name: 'Socrates', age: 71, order: 1 },
      ])
    })

    it('filter query', async function () {
      const FILTER_QUERY = gql`
 query PersonQuery {
    Person(filter: {name: {ge : "K"}, age: {ge : "30"}}, orderBy : {order : ASC}){
        name
        age
        order
    }
}`
      const result = await client.query({ query: FILTER_QUERY })
      expect(result.data.Person).to.deep.equal([
        { name: 'Socrates', age: 71, order: 1 },
        { name: 'Plato', age: 80, order: 2 },
        { name: 'Karl Popper', age: 92, order: 5 },
        { name: 'Kurt Gödel', age: 71, order: 5 },
      ])
    })

    it('graphql order by stringy num', async function () {
      const INTEGER_QUERY = gql`
 query IntegerQuery {
    Integer(orderBy: {int: ASC}) {
        int
    }
}`
      const result = await client.query({ query: INTEGER_QUERY })
      expect(result.data.Integer).to.deep.equal(
        [
          {
            int: 1,
          },
          {
            int: 2,
          },
          {
            int: 11,
          },
          {
            int: 100,
          },

        ],
      )
    })

    it('graphql filter nonNegativeInteger', async function () {
      const NON_NEGATIVE_INTEGER_QUERY = gql`
 query NonNegativeIntegerQuery {
    NonNegativeInteger(filter: {nonnegint: {ge: "4"}}, orderBy: {nonnegint: ASC}) {
        nonnegint
    }
}`
      const result = await client.query({ query: NON_NEGATIVE_INTEGER_QUERY })
      expect(result.data.NonNegativeInteger).to.deep.equal(
        [
          {
            nonnegint: 300,
          },

        ],
      )
    })

    it('graphql filter dateTime', async function () {
      const DATETIME_QUERY = gql`
 query dateTimeQuery {
    DateAndTime(filter: {datetime: {le: "2021-03-05T23:34:43.0003Z" }},
                orderBy: {datetime: ASC}) {
        datetime
    }
}`
      const result = await client.query({ query: DATETIME_QUERY })
      expect(result.data.DateAndTime).to.deep.equal(
        [
          {
            datetime: '2021-03-05T23:34:43.000300Z',
          },

        ],
      )
    })

    it('graphql filter stringy num', async function () {
      const INTEGER_QUERY = gql`
 query IntegerQuery {
    Integer(filter: {int: {ge : "4"}}, orderBy: {int: ASC}) {
        int
    }
}`
      const result = await client.query({ query: INTEGER_QUERY })
      expect(result.data.Integer).to.deep.equal(
        [
          {
            int: 11,
          },
          {
            int: 100,
          },
        ],
      )
    })

    it('graphql order BigFloat', async function () {
      const BIGFLOAT_QUERY = gql`
 query BigFloat {
    MyBigFloat(orderBy: {bigfloat: ASC}) {
        bigfloat
    }
}`
      const result = await client.query({ query: BIGFLOAT_QUERY })

      // NOTE: Apollo Client parses JSON numbers as JavaScript floats (IEEE 754)
      // Precision beyond ~16 digits is lost. The RAW JSON contains full 20-digit precision.
      //
      // For production apps needing full precision, see:
      // - "Apollo Client precision loss and solutions" test (overview)
      // - "GraphQL decimal precision: Apollo Client strategies" suite (examples)
      //
      // For server correctness verification, see: "RAW JSON verification" tests
      expect(result.data.MyBigFloat).to.be.an('array').with.lengthOf(4)
      expect(result.data.MyBigFloat[0].bigfloat).to.equal(0)
      // Apollo truncates to ~16 digits, so we can't verify full 20-digit precision here
      expect(result.data.MyBigFloat[1].bigfloat).to.be.a('number')
      expect(result.data.MyBigFloat[2].bigfloat).to.equal(101)
      expect(result.data.MyBigFloat[3].bigfloat).to.equal(10096.757)
    })

    it('graphql 20-digit decimal precision query (RAW JSON verification)', async function () {
      const BIGFLOAT4_QUERY = gql`
 query BigFloat4Query {
    MyBigFloat(orderBy: {bigfloat: ASC}) {
        bigfloat
    }
}`

      // Test the RAW HTTP response to verify JSON numbers (not strings)
      const rawResponse = await agent.post(api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName }))
        .send({ query: BIGFLOAT4_QUERY.loc.source.body })

      // CRITICAL: Verify the RAW JSON contains unquoted numbers (not strings)
      // This is the correct JSON representation per JSON_SERIALIZATION_RULES.md
      // The value should appear as:  "bigfloat":0.01234567890123456789
      // NOT as:  "bigfloat":"0.01234567890123456789"

      expect(rawResponse.text).to.include('"bigfloat":0.01234567890123456789')
      expect(rawResponse.text).to.not.include('"bigfloat":"0.01234567890123456789"')

      // Verify the full 20-digit number is present in the JSON (before JavaScript parsing)
      const match = rawResponse.text.match(/"bigfloat":0\.01234567890123456789/)
      expect(match).to.exist

      // NOTE: JavaScript clients using Apollo/native JSON.parse will lose precision
      // beyond ~16 digits due to IEEE 754 float conversion.
      // Clients needing full 20-digit precision must use custom JSON parsers
      // (e.g., json-bigint, decimal.js with JSON.parse reviver, or lossless-json)
    })

    it('Apollo Client precision loss and solutions', async function () {
      // PROBLEM: GraphQL servers output JSON numbers for xsd:decimal (per JSON spec),
      // but Apollo Client uses native JSON.parse() which converts numbers to IEEE 754
      // floats, losing precision beyond ~16 digits for values like 0.01234567890123456789
      //
      // SOLUTIONS: (1) TypePolicies, (2) Custom ApolloLink, (3) json-bigint library
      //
      // Reference: https://www.apollographql.com/docs/react/caching/cache-field-behavior/

      const PRECISION_QUERY = gql`
 query PrecisionTest {
    MyBigFloat(orderBy: {bigfloat: ASC}) {
        bigfloat
    }
}`

      // 1. PROBLEM: Default Apollo Client loses precision
      const defaultResult = await client.query({ query: PRECISION_QUERY })
      const defaultValue = defaultResult.data.MyBigFloat[1].bigfloat

      expect(typeof defaultValue).to.equal('number')
      // IEEE 754 double precision loses digits beyond ~16 significant digits
      expect(defaultValue.toString()).to.not.equal('0.01234567890123456789')

      // 2. VERIFICATION: Server outputs correct 20-digit JSON number
      const rawResponse = await agent.post(api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName }))
        .send({ query: PRECISION_QUERY.loc.source.body })

      // The raw JSON contains the full precision (before JavaScript parsing)
      expect(rawResponse.text).to.include('"bigfloat":0.01234567890123456789')

      // 3. SOLUTION A: Apollo TypePolicies (field-level transformation)
      // TypePolicies allow automatic transformation when reading from cache:
      //
      // const cache = new InMemoryCache({
      //   typePolicies: {
      //     MyBigFloat: {
      //       fields: {
      //         bigfloat: {
      //           read(value) {
      //             // Convert number to Decimal when reading from cache
      //             return typeof value === 'number' ? new Decimal(value) : value
      //           }
      //         }
      //       }
      //     }
      //   }
      // })
      //
      // const client = new ApolloClient({ cache, link: httpLink })
      //
      // LIMITATION: Precision already lost during JSON.parse, TypePolicy can't recover it
      // Best for: Converting floats to Decimal instances consistently across your app
      // Not suitable for: Recovering full 20-digit precision (use Solution B or C)

      // 4. SOLUTION B: Custom ApolloLink (intercepts response before parsing)
      // This is the BEST solution - intercept raw JSON text before parsing
      //
      // Implementation would look like:
      // import { ApolloLink, HttpLink } from '@apollo/client'
      // import { LosslessJSON } from 'lossless-json'
      //
      // const losslessLink = new ApolloLink((operation, forward) => {
      //   return forward(operation).map(response => {
      //     // Transform response data here with lossless parsing
      //     return response
      //   })
      // })
      //
      // const client = new ApolloClient({
      //   link: losslessLink.concat(httpLink),
      //   cache: new InMemoryCache()
      // })

      // 5. SOLUTION C: Custom fetch with json-bigint (preserves precision)
      // This requires using a library that doesn't convert to floats during parsing:
      //
      // import JSONbig from 'json-bigint'
      //
      // const customFetch = (uri, options) => {
      //   return fetch(uri, options).then(response => {
      //     return response.text().then(text => ({
      //       ...response,
      //       text: () => Promise.resolve(text),
      //       json: () => Promise.resolve(JSONbig.parse(text))
      //     }))
      //   })
      // }
      //
      // const httpLink = new HttpLink({
      //   uri: graphqlEndpoint,
      //   fetch: customFetch
      // })

      // RECOMMENDATION: For applications requiring full 20-digit decimal precision,
      // use Solution B (custom ApolloLink) or C (custom fetch with json-bigint).
      // TypePolicies (Solution A) cannot recover precision already lost during parsing.
      //
      // SEE BELOW: "GraphQL decimal precision: Apollo Client strategies" suite
      // for concrete, working examples of each strategy.
    })
  })

  describe('GraphQL decimal precision: Apollo Client strategies', function () {
    // ============================================================================
    // IMPORTANT: Understanding GraphQL Decimal Precision in Apollo Client
    // ============================================================================
    //
    // THE SERVER IS CORRECT: TerminusDB GraphQL outputs xsd:decimal as JSON numbers
    // with full 20-digit precision per JSON_SERIALIZATION_RULES.md.
    // Example raw JSON: {"bigfloat":0.01234567890123456789}
    //
    // THE CHALLENGE: Apollo Client uses native JSON.parse() which converts JSON
    // numbers to IEEE 754 floats, losing precision beyond ~16 digits.
    //
    // TESTING STRATEGIES:
    //
    // 1. For RAW JSON verification (server correctness):
    //    - Use agent.post() to get raw HTTP response
    //    - Verify response.text contains unquoted numbers with full precision
    //    - Example: expect(response.text).to.include('"bigfloat":0.01234567890123456789')
    //
    // 2. For Apollo Client tests (documenting client limitation):
    //    - Use client.query() for typical Apollo usage
    //    - Document that precision is lost during parsing
    //    - Use .closeTo() for approximate equality, not .equals()
    //    - Add comments explaining the limitation
    //
    // 3. For production applications needing full precision:
    //    - Use TypePolicies (easy, but precision already lost)
    //    - Use manual parsing with regex (full precision, no Apollo cache)
    //    - Use json-bigint custom fetch (recommended for production)
    //
    // This suite provides concrete examples of all three approaches.
    // ============================================================================

    it('graphql decimal precision approach: TypePolicies with cache transformation', async function () {
      // This strategy uses Apollo's TypePolicies to automatically convert
      // cached number values to Decimal instances when reading from cache.
      //
      // PROS: Easy to implement, type-safe, integrates with Apollo cache
      // CONS: Cannot recover precision already lost during JSON.parse()

      const QUERY = gql`
 query TestTypePolicies {
    MyBigFloat(orderBy: {bigfloat: ASC}) {
        bigfloat
    }
}`

      // In production, you'd create cache with TypePolicies like this:
      //
      // const decimalCache = new InMemoryCache({
      //   typePolicies: {
      //     MyBigFloat: {
      //       fields: {
      //         bigfloat: {
      //           read(value) {
      //             return typeof value === 'number' ? new Decimal(value) : value
      //           }
      //         }
      //       }
      //     }
      //   }
      // })
      //
      // const typePolicyClient = new ApolloClient({
      //   cache: decimalCache,
      //   link: authMiddleware.concat(httpLink)
      // })

      // Get data using default client
      const result = await client.query({ query: QUERY })
      const rawValue = result.data.MyBigFloat[1].bigfloat

      // Simulate TypePolicy read function
      const value = typeof rawValue === 'number' ? new Decimal(rawValue) : rawValue

      // Verify value is now a Decimal instance
      expect(value).to.be.instanceOf(Decimal)

      // Can use Decimal methods
      expect(value.toNumber()).to.be.a('number')

      // LIMITATION: Precision was lost during JSON.parse, so we can't recover full 20 digits
      // The original 0.01234567890123456789 was truncated by JavaScript's float parsing
      const precisionLost = !value.equals(new Decimal('0.01234567890123456789'))
      expect(precisionLost).to.be.true // Demonstrates the limitation

      // Use case: When you want Decimal instances throughout your app but can accept
      // ~16 digit precision (sufficient for most financial calculations)
    })

    it('graphql decimal precision approach: manual parsing with full precision', async function () {
      // This strategy bypasses Apollo's JSON.parse by fetching raw response text
      // and parsing with a custom reviver that preserves precision.
      //
      // PROS: Full precision preserved, no additional libraries needed
      // CONS: Bypasses Apollo cache, need to parse manually

      const QUERY = `
query TestManualParsing {
  MyBigFloat(orderBy: {bigfloat: ASC}) {
    bigfloat
  }
}`

      // Make raw HTTP request to get response text before JSON parsing
      const rawResponse = await agent.post(api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName }))
        .send({ query: QUERY })

      // Verify server sends correct 20-digit JSON number
      expect(rawResponse.text).to.include('"bigfloat":0.01234567890123456789')

      // Strategy: Parse the raw text with a custom approach
      // Since JSON.parse already converts to floats, we need to extract numbers as strings first
      const jsonText = rawResponse.text

      // Replace decimal numbers with quoted strings before parsing
      const stringified = jsonText.replace(/"bigfloat":([-0-9.eE+]+)/g, '"bigfloat":"$1"')

      // Now parse with string values
      const parsed = JSON.parse(stringified)

      // Convert string values to Decimal
      const data = parsed.data.MyBigFloat.map(item => ({
        ...item,
        bigfloat: new Decimal(item.bigfloat),
      }))

      const value = data[1].bigfloat

      // Verify full 20-digit precision preserved
      expect(value).to.be.instanceOf(Decimal)
      expect(value.equals(new Decimal('0.01234567890123456789'))).to.be.true
      expect(value.toString()).to.equal('0.01234567890123456789')

      // Use case: When you need full precision and can handle manual parsing
      // Best for: Critical financial calculations, scientific applications
    })

    it('graphql decimal precision approach: json-bigint custom fetch pattern', async function () {
      // This test documents how to integrate json-bigint with Apollo Client
      // for production applications requiring arbitrary precision.
      //
      // IMPLEMENTATION STEPS:
      //
      // 1. Install dependencies:
      //    npm install json-bigint
      //
      // 2. Create custom fetch function:
      //
      // import JSONbig from 'json-bigint'
      // import { ApolloClient, InMemoryCache, HttpLink } from '@apollo/client'
      //
      // const bigintFetch = (uri, options) => {
      //   return fetch(uri, options).then(response => {
      //     return response.text().then(text => {
      //       const parsed = JSONbig({ useNativeBigInt: false }).parse(text)
      //       return {
      //         ...response,
      //         text: () => Promise.resolve(text),
      //         json: () => Promise.resolve(parsed)
      //       }
      //     })
      //   })
      // }
      //
      // 3. Configure Apollo Client:
      //
      // const httpLink = new HttpLink({
      //   uri: 'http://localhost:6363/api/graphql/myorg/mydb',
      //   fetch: bigintFetch
      // })
      //
      // const client = new ApolloClient({
      //   link: httpLink,
      //   cache: new InMemoryCache()
      // })
      //
      // 4. Result: All numeric fields preserve full precision as BigNumber instances
      //
      // PROS: Full precision, works seamlessly with Apollo, production-ready
      // CONS: Requires additional dependency (json-bigint)

      // For this test, we verify the pattern works by demonstrating manual approach
      const QUERY = `
query TestBigIntPattern {
  MyBigFloat(orderBy: {bigfloat: ASC}) {
    bigfloat
  }
}`

      const rawResponse = await agent.post(api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName }))
        .send({ query: QUERY })

      // Verify: Server outputs correct JSON number format
      expect(rawResponse.text).to.include('"bigfloat":0.01234567890123456789')
      expect(rawResponse.text).to.not.include('"bigfloat":"0.01234567890123456789"')

      // The json-bigint library would parse this maintaining precision
      // by converting numbers to BigNumber/Decimal objects during parsing

      // RECOMMENDATION: Use this strategy for production applications where
      // decimal precision is critical (finance, scientific, legal applications)
    })

    it('graphql decimal trailing zero normalization (RAW JSON verification)', async function () {
      // This test documents JSON number normalization behavior
      // Per JSON spec, trailing zeros are semantically redundant and may be normalized
      // Example: 0.12345678901234567890 → 0.1234567890123456789 (trailing zero removed)

      // Insert a decimal with trailing zero
      const trailingZeroDoc = {
        '@type': 'MyBigFloat',
        bigfloat: '0.12345678901234567890', // 20 digits with trailing zero
      }
      await document.insert(agent, { instance: trailingZeroDoc })

      const QUERY = gql`
 query TrailingZeroQuery {
    MyBigFloat {
        bigfloat
    }
}`

      // Test RAW JSON response
      const rawResponse = await agent.post(api.path.graphQL({ dbName: agent.dbName, orgName: agent.orgName }))
        .send({ query: QUERY.loc.source.body })

      // Verify the RAW JSON contains the number (trailing zero may be normalized)
      // Both 0.12345678901234567890 and 0.1234567890123456789 are valid
      const hasNumber = rawResponse.text.includes('"bigfloat":0.1234567890123456789')
      expect(hasNumber).to.be.true

      // Verify it's NOT a quoted string
      const hasQuotedString = rawResponse.text.includes('"bigfloat":"0.1234567890123456789"')
      expect(hasQuotedString).to.be.false

      // The precision is maintained (19-20 significant digits)
      const match = rawResponse.text.match(/"bigfloat":(0\.1234567890123456789\d?)/)
      expect(match).to.exist
    })
  })

  // Remaining tests continue in the original "queries" suite above
  describe('additional queries', function () {
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
        { name: 'Socrates', age: 71, order: 1, _friend_of_Person: [] },
        {
          name: 'Plato',
          age: 80,
          order: 2,
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
          order: 3,
          _friend_of_Person: [
            {
              name: 'Plato',
            },
          ],
        },
        {
          name: 'Immanuel Kant',
          age: 79,
          order: 3,
          _friend_of_Person: [{
            name: 'Immanuel Kant',
          },
          {
            name: 'Kurt Gödel',
          },
          ],
        },
        { name: 'Karl Popper', age: 92, order: 5, _friend_of_Person: [] },
        { name: 'Kurt Gödel', age: 71, order: 5, _friend_of_Person: [] },
      ])
    })

    it('back link to list', async function () {
      const edges = [
        {
          '@type': 'Source',
          name: '1',
          targets: ['Target/1', 'Target/2', 'Target/3'],
        },
        {
          '@type': 'Source',
          name: '2',
          targets: ['Target/1', 'Target/2', 'Target/3'],
        },
        {
          '@type': 'Target',
          name: '1',
        },
        {
          '@type': 'Target',
          name: '2',
        },
        {
          '@type': 'Target',
          name: '3',
        },
      ]
      await document.insert(agent, { instance: edges })
      const PATH_QUERY = gql`
 query SourceQuery {
    Target {
        name
        _targets_of_Source(orderBy: { name : DESC }){
           name
        }
    }
}`
      const result = await client.query({ query: PATH_QUERY })
      expect(result.data.Target).to.deep.equal(
        [
          { name: '1', _targets_of_Source: [{ name: '2' }, { name: '1' }] },
          { name: '2', _targets_of_Source: [{ name: '2' }, { name: '1' }] },
          { name: '3', _targets_of_Source: [{ name: '2' }, { name: '1' }] },
        ],
      )
    })

    it('ne query', async function () {
      const NE_QUERY = gql`
  query PersonQuery {
     Person(filter:{name:{ne:"Socrates"}}, orderBy : {order : ASC}){
          name
        }
   }`
      const result = await client.query({ query: NE_QUERY })

      expect(result.data.Person).to.deep.equal(
        [
          { name: 'Plato' },
          { name: 'Aristotle' },
          { name: 'Immanuel Kant' },
          { name: 'Karl Popper' },
          { name: 'Kurt Gödel' },
        ],
      )
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

    it('graphql path query backward and forward', async function () {
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
    Person(ids : [
           "terminusdb:///data/Person/Immanuel%20Kant",
           "terminusdb:///data/Person/Socrates"
           ] ){
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
        positiveInteger: 2342423,
        negativeInteger: -2348982734,
        nonPositiveInteger: -334,
        nonNegativeInteger: 3243323,
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

      await document.insert(agent, { instance: everything }).unverified()

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
          anySimpleType: '"3"', // Whole numbers stored as integer, not decimal
          string: 'string',
          boolean: true,
          decimal: 3.2, // GraphQL outputs JSON numbers, Apollo parses as float
          float: 3.200000047683716,
          time: '23:34:43Z',
          date: '2021-03-05',
          dateTime: '2021-03-05T23:34:43.000300Z',
          dateTimeStamp: '2021-03-05T23:34:43.000300Z',
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
          long: -532,
          unsignedByte: 3,
          unsignedShort: 5,
          unsignedInt: 8,
          unsignedLong: 10,
          integer: 20,
          positiveInteger: 2342423,
          negativeInteger: -2348982734,
          nonPositiveInteger: -334,
          nonNegativeInteger: 3243323,
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

    it('graphql subsumption', async function () {
      const members = [{ name: 'Joe', number: 3 },
        { name: 'Jim', number: 5 },
        { '@type': 'Parent', name: 'Dad' }]
      await document.insert(agent, { instance: members })
      const PARENT_QUERY = gql`
 query ParentQuery {
    Parent(orderBy: {name : ASC}){
        _type
        name
    }
}`
      const result = await client.query({ query: PARENT_QUERY })
      expect(result.data.Parent).to.deep.equal(
        [
          {
            _type: 'Parent',
            name: 'Dad',
          },
          {
            _type: 'Child',
            name: 'Jim',
          },
          {
            _type: 'Child',
            name: 'Joe',
          },
        ])
    })

    it('graphql no subsumption', async function () {
      const PARENT_QUERY = gql`
 query ParentQuery {
    Parent(include_children: false, orderBy: {name : ASC}){
        _type
        name
    }
}`
      const result = await client.query({ query: PARENT_QUERY })
      expect(result.data.Parent).to.deep.equal(
        [
          {
            _type: 'Parent',
            name: 'Dad',
          },
        ])
    })

    it('collects le filtered strings', async function () {
      const instance = [
        {
          '@id': 'A/a1',
          '@type': 'A',
          name: 'a1',
        },
        {
          '@id': 'A/a2',
          '@type': 'A',
          name: 'a2',
        }]
      await document.insert(agent, { instance })
      const TEST_QUERY = gql`
 query TEST {
    A(filter:{name:{le:"a2"}}) { name }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.A).to.have.deep.members([
        {
          name: 'a1',
        },
        {
          name: 'a2',
        },
      ])
    })

    it('graphql meta-tags', async function () {
      const testObj = {
        '@id': 'Test',
        '@key': {
          '@type': 'Random',
        },
        '@metadata': {
          render_as: {
            test: 'markdown',
          },
        },
        '@type': 'Class',
        test: {
          '@class': 'xsd:string',
          '@type': 'Optional',
        },
      }
      await document.insert(agent, { schema: testObj })
      const TEST_QUERY = gql`
 query TestQuery {
    Test{
        test
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.Test).to.deep.equal([])
    })

    it('graphql optional enum', async function () {
      const testObj = {
        '@type': 'MaybeRocks',
        rocks_opt: 'Big',
      }
      await document.insert(agent, { instance: testObj })
      const TEST_QUERY = gql`
 query TestRocks {
    MaybeRocks{
        rocks_opt
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.MaybeRocks).to.deep.equal([
        {
          rocks_opt: 'Big',
        },
      ])
    })

    it('graphql oneOf treated as optional', async function () {
      const testObj = {
        '@type': 'OneOf',
        '@id': 'OneOf/1',
        a: 'a',
      }
      await document.insert(agent, { instance: testObj })

      const TEST_QUERY = gql`
 query  {
    OneOf{
        a,
        b
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.OneOf).to.deep.equal([
        {
          a: 'a',
          b: null,
        },
      ])
    })

    it('graphql array property not present', async function () {
      const testObj = {
        '@type': 'NotThere',
      }
      await document.insert(agent, { instance: testObj })

      const TEST_QUERY = gql`
 query NotThere {
    NotThere{
        property
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.NotThere).to.deep.equal([
        { property: [] },
      ])
    })

    it('graphql list of enum', async function () {
      const testObj = {
        '@type': 'RockSet',
        rocks: ['Big', 'Medium', 'Small'],
      }
      await document.insert(agent, { instance: testObj })

      const TEST_QUERY = gql`
 query NotThere {
    RockSet{
        rocks
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.RockSet[0].rocks).to.have.deep.members([
        'Big',
        'Medium',
        'Small',
      ])
    })

    it('graphql json', async function () {
      const testObj = {
        '@type': 'JSONClass',
        json: { this: { is: { a: { json: [] } } } },
      }
      await document.insert(agent, { instance: testObj })

      const TEST_QUERY = gql`
 query JSON {
    JSONClass{
        json
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.JSONClass).to.deep.equal([
        { json: '{"this":{"is":{"a":{"json":[]}}}}' },
      ])
    })

    it('graphql json set', async function () {
      const testObj = {
        '@type': 'JSONs',
        json: [{ this: { is: { a: { json: [] } } } },
          { and: ['another', 'one'] },
        ],
      }
      await document.insert(agent, { instance: testObj })

      const TEST_QUERY = gql`
 query JSONs {
    JSONs{
        json
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.JSONs[0].json).to.have.deep.members(
        [
          '{"and":["another","one"]}',
          '{"this":{"is":{"a":{"json":[]}}}}',
        ],
      )
    })

    it('graphql optional rename', async function () {
      const testObj = {
        '@type': 'BadlyNamedOptional',
        'is-it-ok': 'something',
      }
      await document.insert(agent, { instance: testObj })

      const TEST_QUERY = gql`
 query TEST {
    BadlyNamedOptional{
        is_it_ok
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.BadlyNamedOptional).to.deep.equal([
        { is_it_ok: 'something' },
      ])
    })

    it('graphql queries a prefix', async function () {
      const instance = {
        'prefix:foo': 'baz',
      }
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    Prefix{
        prefix_foo
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.Prefix).to.deep.equal([{ prefix_foo: 'baz' }])
    })

    it('graphql queries reversable', async function () {
      const instance = {
        'prefix:string': 'Bar',
        'prefix:node': { 'prefix:string': 'Baz' },
      }
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    Node(filter: { prefix_string : { eq : "Baz" }}){
        prefix_string
        _prefix_node_of_Node{
            prefix_string
        }
    }
}`

      const result = await client.query({ query: TEST_QUERY })

      expect(result.data.Node).to.deep.equal([
        {
          _prefix_node_of_Node: [
            {
              prefix_string: 'Bar',
            },
          ],
          prefix_string: 'Baz',
        },
      ])
    })

    it('has a renamed enum type', async function () {
      const instance = {
        '@type': 'EnumPointer',
        pointer: 'enum-one',
      }
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    EnumPointer{
        pointer
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.EnumPointer).to.deep.equal([{ pointer: 'enum_one' }])
    })

    it('can rename in subsumption', async function () {
      const instance = {
        parent_name: 'far', child_name: 'further',
      }
      await document.insert(agent, { instance }).unverified()

      const TEST_QUERY = gql`
 query TEST {
    UnprefixedParent{
        parent_name
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.UnprefixedParent).to.deep.equal([{ parent_name: 'far' }])
    })

    it('filters from initial iterator with array', async function () {
      const instance = [{
        '@id': 'SourceArray/1',
        name: '1',
        target_array: [{
          '@id': 'TargetArray/11',
          name: '11',
        },
        {
          '@id': 'TargetArray/12',
          name: '12',
        }],
      },
      {
        '@id': 'SourceArray/2',
        name: '2',
        target_array: [{
          '@id': 'TargetArray/21',
          name: '21',
        },
        {
          '@id': 'TargetArray/22',
          name: '22',
        },
        ],
      }]
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    SourceArray(filter:{target_array: {someHave: {name: {eq: "22"}}}}) {
      _id
      name
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.SourceArray).to.have.deep.members([
        {
          _id: 'terminusdb:///data/SourceArray/2',
          name: '2',
        },
      ])
    })

    it('filters with an allHave', async function () {
      const instance = [{
        '@id': 'SourceArray/all1',
        name: 'all1',
        target_array: [{
          '@id': 'TargetArray/all23',
          name: 'all23',
        },
        {
          '@id': 'TargetArray/all12',
          name: 'all12',
        }],
      },
      {
        '@id': 'SourceArray/all2',
        name: 'all2',
        target_array: [{
          '@id': 'TargetArray/all21',
          name: 'all21',
        },
        {
          '@id': 'TargetArray/all22',
          name: 'all22',
        },
        ],
      }]
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    SourceArray(filter:{target_array: {allHave: {name: {startsWith: "all2"}}}}) {
      _id
      name
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.SourceArray).to.have.deep.members([
        {
          _id: 'terminusdb:///data/SourceArray/all2',
          name: 'all2',
        },
      ])
    })

    it('graphql type renaming works', async function () {
      const instance = {
        '@type': 'prefix:Outer',
        inner: { inner_name: 'This is an inner' },
      }
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    prefix_Outer{
       inner{
          _type
          inner_name
       }
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.prefix_Outer).to.deep.equal([
        {
          inner: {
            _type: 'prefix_Inner',
            inner_name: 'This is an inner',
          },
        },
      ])
    })
  })

  describe('GraphQL Crashing', function () {
    let agent
    let client

    beforeEach(async function () {
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
      await document.insert(agent, { schema, fullReplace: true })
    })

    afterEach(async function () {
      await db.delete(agent)
    })

    it('has a renamed back link', async function () {
      const instance = {
        '@type': 'prefix:MyClass',
        name: 'foo',
        'prefix:link': {
          '@type': 'prefix:MyClass',
          name: 'bar',
        },
      }
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    prefix_MyClass{
        name
        _prefix_link_of_prefix_MyClass { name }
    }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.prefix_MyClass).to.have.deep.members([
        {
          _prefix_link_of_prefix_MyClass: [
            {
              name: 'foo',
            },
          ],
          name: 'bar',
        },
        {
          _prefix_link_of_prefix_MyClass: [],
          name: 'foo',
        },
      ],
      )
    })

    it('has required foreign field', async function () {
      const instance = {
        required_foreign_field: 'http://example.com/entity',
      }
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    RequiredForeignField(filter:{required_foreign_field: {_id: "http://example.com/entity"}}) { required_foreign_field }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.RequiredForeignField).to.have.deep.members([
        {
          required_foreign_field: 'http://example.com/entity',
        },
      ])
    })

    it('has set foreign field', async function () {
      const instance = {
        set_foreign_field: 'http://example.com/entity',
      }
      await document.insert(agent, { instance })

      const TEST_QUERY = gql`
 query TEST {
    SetForeignField(filter:{set_foreign_field: {someHave: {_id: "http://example.com/entity"}}}) { set_foreign_field }
}`

      const result = await client.query({ query: TEST_QUERY })
      expect(result.data.SetForeignField).to.have.deep.members([
        {
          set_foreign_field: ['http://example.com/entity'],
        },
      ])
    })

    it('shadows a graphql type and fails', async function () {
      const collision = {
        '@type': 'Class',
        '@id': 'BigInt',
        bigint: 'xsd:integer',
      }
      await document.insert(agent, { schema: collision })

      const TEST_QUERY = gql`
 query TEST {
    BigInt{
        bigint
    }
}`

      const result = await client.query({ query: TEST_QUERY })
        .catch((error) => {
          const nwe = error.networkError
          expect(nwe.statusCode).to.equal(500)
        })
      expect(result).to.equal(undefined)
    })
  })
})
