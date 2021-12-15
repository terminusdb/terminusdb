const { parser: jsonl } = require('stream-json/jsonl/Parser')
const { expect } = require('chai')
const { Agent, db, document, endpoint, Params, util } = require('../lib')

describe('document-get', function () {
  let agent
  let dbPath
  let docPath

  const context = {
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
    order: 'xsd:integer',
  }

  const aristotle = { '@type': 'Person', name: 'Aristotle', age: 61, order: 3 }
  const plato = { '@type': 'Person', name: 'Plato', age: 80, order: 2 }
  const socrates = { '@type': 'Person', name: 'Socrates', age: 71, order: 1 }
  const kant = { '@type': 'Person', name: 'Immanuel Kant', age: 79, order: 3 }
  const popper = { '@type': 'Person', name: 'Karl Popper', age: 92, order: 5 }
  const gödel = { '@type': 'Person', name: 'Kurt Gödel', age: 71, order: 5 }

  const instances = [aristotle, plato, socrates]

  before(async function () {
    agent = new Agent().auth()

    const dbDefaults = endpoint.db(agent.defaults())

    dbPath = dbDefaults.path
    await db.createAfterDel(agent, dbPath)

    docPath = endpoint.document(dbDefaults).path
    await document
      .insert(agent, docPath, { schema: schema })
      .then(document.verifyInsertSuccess)
    await document
      .insert(agent, docPath, { instance: instances })
      .then(document.verifyInsertSuccess)
  })

  after(async function () {
    await db.del(agent, dbPath)
  })

  function expectSchema (objects, params) {
    params = new Params(params)
    const schemaPrefix =
      params.boolean('prefixed', true)
        ? ''
        : context['@schema']
    params.assertEmpty()

    expect(objects.length).to.equal(2)
    expect(objects[0]).to.deep.equal(context)

    let schema2
    if (schemaPrefix) {
      schema2 = util.deepClone(schema)
      schema2['@id'] = schemaPrefix + schema2['@id']
    } else {
      schema2 = schema
    }
    expect(objects[1]).to.deep.equal(schema2)
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

  function expectInstances (objects, instances, params) {
    params = new Params(params)
    const skip = params.integer('skip', 0)
    let count = params.integer('count', instances.length)
    const [schemaPrefix, basePrefix] =
      params.boolean('prefixed', true)
        ? ['', '']
        : [context['@schema'], context['@base']]
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
      expect(object['@type']).to.equal(schemaPrefix + expected['@type'])
      delete object['@type']
      const name = schemaPrefix + 'name'
      expect(object[name]).to.equal(expected.name)
      delete object[name]
      const age = schemaPrefix + 'age'
      expect(object[age]).to.equal(expected.age)
      delete object[age]
      const order = schemaPrefix + 'order'
      expect(object[order]).to.equal(expected.order)
      delete object[order]
      expect(object['@id']).to.equal(encodeURI(basePrefix + 'Person/' + expected.name))
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
          expectInstances(objects, instances)
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
        expectInstances(r.body, instances)
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
        expectInstances(r.body, instances, params)
      })
    }
  })

  describe('returns expected for prefixed=false', function () {
    const options = [
      { query: { graph_type: 'schema', as_list: true, prefixed: false } },
      { body: { graph_type: 'schema', as_list: true, prefixed: false } },
      { query: { graph_type: 'instance', as_list: true, prefixed: false } },
      { body: { graph_type: 'instance', as_list: true, prefixed: false } },
      { query: { graph_type: 'schema', as_list: true, compress_ids: false } },
      { body: { graph_type: 'schema', as_list: true, compress_ids: false, prefixed: false } },
      { query: { graph_type: 'instance', as_list: true, compress_ids: false, prefixed: true } },
      { body: { graph_type: 'instance', as_list: true, compress_ids: false }, query: { prefixed: true } },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        let graphType
        if (option.query) {
          graphType = option.query.graph_type
        }
        if (option.body) {
          graphType = option.body.graph_type || graphType
        }
        if (graphType === 'schema') {
          // TODO: Implement `prefixed` for schemas:
          // https://github.com/terminusdb/terminusdb/issues/801
          this.skip()
        }
        const r = await document
          .get(agent, docPath, option)
          .then(document.verifyGetSuccess)
        switch (graphType) {
          case 'schema':
            expectSchema(r.body, { prefixed: false })
            break
          case 'instance':
            expectInstances(r.body, instances, { prefixed: false })
            break
          default:
            throw new Error(`Unexpected 'graphType': ${graphType}`)
        }
      })
    }
  })

  describe('fails for bad parameter type', function () {
    const options = [
      [{ queryString: 'as_list=7' }, 'as_list', 'boolean', '7'],
      [{ bodyString: '{"as_list":"wrong"}' }, 'as_list', 'boolean', 'wrong'],
      [{ queryString: 'compress_ids=null' }, 'compress_ids', 'boolean', null],
      [{ queryString: 'compress_ids=abc' }, 'compress_ids', 'boolean', 'abc'],
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

  describe('gives same query results for type and @type', function () {
    const options = [
      { query: { type: 'Person', as_list: true } },
      { body: { query: { '@type': 'Person' }, as_list: true } },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        const r = await document
          .get(agent, docPath, option)
          .then(document.verifyGetSuccess)
        expectInstances(r.body, instances)
      })
    }
  })

  describe('succeeds query for @type and field', function () {
    const queries = [
      [{ name: 'Plato' }, 1],
      [{ age: 71 }, 2],
      [{ order: 3 }, 0],
    ]
    for (const [query, index] of queries) {
      it(JSON.stringify(query), async function () {
        Object.assign(query, { '@type': 'Person' })
        const r = await document
          .get(agent, docPath, { body: { query: query } })
          .then(document.verifyGetSuccess)
        expectInstances([r.body], instances.slice(index, index + 1))
      })
    }
  })

  it('fails query on field without type or @type', async function () {
    const r = await document
      .get(agent, docPath, { body: { query: { name: 'Plato' } } })
      .then(document.verifyGetFailure)
    expect(r.body['api:error']['@type']).to.equal('api:QueryMissingType')
  })

  describe('queries field of type Set', function () {
    const localSchema = {
      '@id': 'Group',
      '@type': 'Class',
      people: { '@type': 'Set', '@class': 'Person' },
    }

    const localInstances = [
      { '@id': 'Group/0', '@type': 'Group', people: [kant, popper, gödel] },
      { '@id': 'Group/1', '@type': 'Group', people: [] },
      { '@id': 'Group/2', '@type': 'Group' },
    ]

    before(async function () {
      await document
        .insert(agent, docPath, { schema: localSchema })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, { instance: localInstances })
        .then(document.verifyInsertSuccess)
    })

    after(async function () {
      await document
        .del(agent, docPath, { body: localInstances.map((i) => i['@id']) })
        .then(document.verifyDelSuccess)
      await document
        .del(agent, docPath, { query: { graph_type: 'schema', id: 'Group' } })
        .then(document.verifyDelSuccess)
    })

    const q1 = { '@type': 'Group' }

    it(JSON.stringify(q1), async function () {
      const r = await document
        .get(agent, docPath, { body: { query: q1, as_list: true } })
        .then(document.verifyGetSuccess)
      const myInstances = []
      for (const instance of localInstances) {
        const copy = Object.assign({}, instance)
        if (instance.people) {
          if (instance.people.length) {
            copy.people = instance.people.map((p) => encodeURI(p['@type'] + '/' + p.name))
          } else {
            delete copy.people
          }
        }
        myInstances.push(copy)
      }
      expect(r.body).to.have.deep.members(myInstances)
    })

    const q2 = { '@type': 'Person', age: 71 }

    it(JSON.stringify(q2), async function () {
      const r = await document
        .get(agent, docPath, { body: { query: q2, as_list: true } })
        .then(document.verifyGetSuccess)
      expectInstances(r.body, [socrates, gödel])
    })
  })

  describe('queries field of type Optional', function () {
    const localSchema = {
      '@id': 'Friendship',
      '@type': 'Class',
      friend: { '@type': 'Optional', '@class': 'Person' },
    }

    const localInstances = [
      { '@id': 'Friendship/0', '@type': 'Friendship', friend: kant },
      { '@id': 'Friendship/1', '@type': 'Friendship', friend: gödel },
      { '@id': 'Friendship/2', '@type': 'Friendship' },
    ]

    before(async function () {
      await document
        .insert(agent, docPath, { schema: localSchema })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, { instance: localInstances })
        .then(document.verifyInsertSuccess)
    })

    after(async function () {
      await document
        .del(agent, docPath, { body: localInstances.map((i) => i['@id']) })
        .then(document.verifyDelSuccess)
      await document
        .del(agent, docPath, { query: { graph_type: 'schema', id: 'Friendship' } })
        .then(document.verifyDelSuccess)
    })

    const q1 = { '@type': 'Friendship' }

    it(JSON.stringify(q1), async function () {
      const r = await document
        .get(agent, docPath, { body: { query: q1, as_list: true } })
        .then(document.verifyGetSuccess)
      const expectedInstances = []
      for (const instance of localInstances) {
        const copy = Object.assign({}, instance)
        if (instance.friend) {
          copy.friend = encodeURI(instance.friend['@type'] + '/' + instance.friend.name)
        }
        expectedInstances.push(copy)
      }
      expect(r.body).to.have.deep.members(expectedInstances)
    })

    const q2 = { '@type': 'Person', age: 71 }

    it(JSON.stringify(q2), async function () {
      const r = await document
        .get(agent, docPath, {
          body: { query: q2, as_list: true },
        })
        .then(document.verifyGetSuccess)
      expectInstances(r.body, [socrates, gödel])
    })
  })
})
