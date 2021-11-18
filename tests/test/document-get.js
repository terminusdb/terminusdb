const { parser: jsonl } = require('stream-json/jsonl/Parser')
const { expect } = require('chai')
const { Agent, db, document, endpoint, Params } = require('../lib')

describe('document-get', function () {
  let agent
  let dbPath
  let docPath

  const prefixes = {
    '@base': 'terminusdb:///data/',
    '@schema': 'terminusdb:///schema#',
    '@type': '@context',
  }

  const schema = {
    '@id': 'Person',
    '@type': 'Class',
    '@key': {
      '@type': 'Lexical',
      '@fields': ['name'],
    },
    name: 'xsd:string',
    age: 'xsd:decimal',
  }

  const instances = [
    { '@type': 'Person', name: 'Aristotle', age: 61 },
    { '@type': 'Person', name: 'Plato', age: 80 },
    { '@type': 'Person', name: 'Socrates', age: 71 },
  ]

  before(async function () {
    agent = new Agent().auth()

    const dbDefaults = endpoint.db(agent.defaults())

    dbPath = dbDefaults.path
    await db.createAfterDel(agent, dbPath)

    docPath = endpoint.document(dbDefaults).path
    await document.insert(agent, docPath, { schema: schema })
    await document.insert(agent, docPath, { instance: instances })
  })

  after(async function () {
    await db.del(agent, dbPath)
  })

  function expectSchema (objects) {
    expect(objects.length).to.equal(2)
    expect(objects[0]).to.deep.equal(prefixes)
    expect(objects[1]).to.deep.equal(schema)
  }

  describe('returns expected schema stream', function () {
    const options = [
      { query: { graph_type: 'schema' } },
      { query: { graph_type: 'schema', as_list: false } },
      { body: { graph_type: 'schema' } },
      { body: { graph_type: 'schema', as_list: false } },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        const r = await document
          .get(agent, docPath, option)
          .pipe(jsonl())
        const objects = []
        r.on('data', (data) => {
          objects.push(data.value)
        })
        r.on('end', () => {
          expectSchema(objects)
        })
      })
    }
  })

  describe('returns expected schema list', function () {
    const options = [
      { query: { graph_type: 'schema', as_list: true } },
      { body: { graph_type: 'schema', as_list: true } },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        const r = await document
          .get(agent, docPath, option)
          .then(document.verifyGetSuccess)
        expectSchema(r.body)
      })
    }
  })

  function expectInstances (objects, params) {
    params = new Params(params)
    const skip = params.integer('skip', 0)
    let count = params.integer('count', instances.length)
    params.assertEmpty()

    if (count > instances.length - skip) {
      count = instances.length - skip
    }
    if (count < 0) {
      count = 0
    }

    expect(objects).to.be.an('array').that.has.lengthOf(count)
    for (let i = 0; i < count; i++) {
      const object = objects[i]
      const expected = instances[i + skip]
      expect(object['@type']).to.equal(expected['@type'])
      delete object['@type']
      expect(object.name).to.equal(expected.name)
      delete object.name
      expect(object.age).to.equal(expected.age)
      delete object.age
      expect(object['@id']).to.equal('Person/' + expected.name)
      delete object['@id']
      expect(Object.keys(object).length).to.equal(0)
    }
  }

  describe('returns expected instance stream', function () {
    const options = [
      null,
      { query: { as_list: false } },
      { body: { as_list: false } },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        const r = await document
          .get(agent, docPath, option)
          .pipe(jsonl())
        const objects = []
        r.on('data', (data) => {
          objects.push(data.value)
        })
        r.on('end', () => {
          expectInstances(objects)
        })
      })
    }
  })

  describe('returns expected instance list', function () {
    const options = [
      { query: { as_list: true } },
      { body: { as_list: true } },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        const r = await document
          .get(agent, docPath, option)
          .then(document.verifyGetSuccess)
        expectInstances(r.body)
      })
    }
  })

  describe('returns expected count and skip', function () {
    const options = [
      { query: { count: 0 } },
      { body: { count: 1 } },
      { query: { count: 2 } },
      { body: { count: 3 } },
      { query: { count: 4 } },
      { query: { skip: 0 } },
      { body: { skip: 1 } },
      { query: { skip: 2 } },
      { body: { skip: 3 } },
      { query: { skip: 4 } },
      { query: { count: 0, skip: 3 } },
      { query: { count: 2, skip: 2 } },
      { query: { count: 1, skip: 1 } },
      { query: { count: 4, skip: 0 } },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        const params = Object.assign({}, option.query, option.body)
        if (option.query) {
          option.query.as_list = true
        }
        if (option.body) {
          option.body.as_list = true
        }
        const r = await document
          .get(agent, docPath, option)
          .then(document.verifyGetSuccess)
        expectInstances(r.body, params)
      })
    }
  })

  describe('fails for bad parameter type', function () {
    const options = [
      [{ queryString: 'as_list=7' }, 'as_list', 'boolean', '7'],
      [{ bodyString: '{"as_list":"wrong"}' }, 'as_list', 'boolean', 'wrong'],
      [{ queryString: 'prefixed=null' }, 'prefixed', 'boolean', null],
      [{ queryString: 'prefixed=abc' }, 'prefixed', 'boolean', 'abc'],
      [{ bodyString: '{"prefixed":1}' }, 'prefixed', 'boolean', 1],
      [{ queryString: 'unfold=' }, 'unfold', 'boolean', ''],
      [{ bodyString: '{"unfold":"false"}' }, 'unfold', 'boolean', 'false'],
      [{ queryString: 'minimized="3"' }, 'minimized', 'boolean', '"3"'],
      [{ bodyString: '{"minimized":0}' }, 'minimized', 'boolean', 0],
      [{ queryString: 'skip=true' }, 'skip', 'nonnegative_integer', true],
      [{ bodyString: '{"skip":[]}' }, 'skip', 'nonnegative_integer', []],
      [{ bodyString: '{"skip":-1000}' }, 'skip', 'nonnegative_integer', -1000],
      [{ queryString: 'count=%20' }, 'count', 'nonnegative_integer', ' '],
      [{ queryString: 'count=-1' }, 'count', 'nonnegative_integer', '-1'],
      [{ bodyString: '{"count":{}}' }, 'count', 'nonnegative_integer', {}],
      [{ queryString: 'id=' }, 'id', 'string', ''],
      [{ bodyString: '{"id":""}' }, 'id', 'string', ''],
      [{ bodyString: '{"id":null}' }, 'id', 'string', null],
      [{ queryString: 'type=' }, 'type', 'string', ''],
      [{ bodyString: '{"type":19}' }, 'type', 'string', 19],
      [{ bodyString: '{"type":false}' }, 'type', 'string', false],
      [{ bodyString: '{"query":null}' }, 'query', 'object', null],
      [{ bodyString: '{"query":82}' }, 'query', 'object', 82],
      [{ queryString: 'graph_type=' }, 'graph_type', 'graph', ''],
      [{ queryString: 'graph_type=bla' }, 'graph_type', 'graph', 'bla'],
      [{ bodyString: '{"graph_type":123}' }, 'graph_type', 'graph', 123],
      [{ bodyString: '{"graph_type":null}' }, 'graph_type', 'graph', null],
      [{ bodyString: '{"graph_type":"schemas"}' }, 'graph_type', 'graph', 'schemas'],
    ]
    for (const [option, paramName, paramType, value] of options) {
      it(JSON.stringify(option), async function () {
        const r = await document
          .get(agent, docPath, option)
          .then(document.verifyGetFailure)
        expect(r.body['api:error']['@type']).to.equal('api:BadParameterType')
        expect(r.body['api:error']['api:parameter']).to.equal(paramName)
        expect(r.body['api:error']['api:expected_type']).to.equal(paramType)
        expect(r.body['api:error']['api:value']).to.deep.equal(value)
      })
    }
  })
})
