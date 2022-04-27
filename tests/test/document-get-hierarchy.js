const { expect } = require('chai')
const { Agent, db, document, endpoint } = require('../lib')

describe('document-get-hierarchy', function () {
  let agent
  let dbPath
  let docPath

  const shape = {
    '@id': 'Shape',
    '@type': 'Class',
    x: 'xsd:double',
    y: 'xsd:double',
  }

  const circle = {
    '@id': 'Circle',
    '@type': 'Class',
    '@inherits': 'Shape',
    radius: 'xsd:double',
  }

  const square = {
    '@id': 'Square',
    '@type': 'Class',
    '@inherits': 'Shape',
    side: 'xsd:double',
  }

  let schema

  const shapes = [
    { '@type': 'Shape', x: 1, y: 2 },
    { '@type': 'Shape', x: -2, y: -1 },
  ]

  const squares = [
    { '@type': 'Square', x: 34, y: -30, side: 4 },
    { '@type': 'Square', x: -0.3, y: 1348400, side: 937 },
  ]

  const circles = [
    { '@type': 'Circle', x: 0, y: 0, radius: 9218042857 },
    { '@type': 'Circle', x: -9298, y: -0.10318410, radius: 1 },
  ]

  let instances

  before(async function () {
    agent = new Agent().auth()

    schema = [shape, circle, square]
    instances = shapes.concat(squares, circles)

    const dbDefaults = endpoint.db(agent.defaults())

    dbPath = dbDefaults.path
    await db.createAfterDel(agent, dbPath)

    docPath = endpoint.document(dbDefaults).path
    await document
      .insert(agent, docPath, { schema })
      .then(document.verifyInsertSuccess)
    await document
      .insert(agent, docPath, { instance: instances })
      .then(document.verifyInsertSuccess)
  })

  after(async function () {
    await db.del(agent, dbPath)
  })

  it('succeeds query for superclass', async function () {
    const r = await document
      .get(agent, docPath, { body: { query: { '@type': 'Shape' }, as_list: true } })
      .then(document.verifyGetSuccess)
    for (const object of r.body) {
      delete object['@id']
    }
    expect(r.body).to.have.deep.members(instances)
  })

  describe('succeeds query for subclass', function () {
    const queries = [
      ['Circle', circles],
      ['Square', squares],
    ]
    for (const [type, instances] of queries) {
      it(type, async function () {
        const r = await document
          .get(agent, docPath, { body: { query: { '@type': type }, as_list: true } })
          .then(document.verifyGetSuccess)
        for (const object of r.body) {
          delete object['@id']
        }
        expect(r.body).to.have.deep.members(instances)
      })
    }
  })
})
