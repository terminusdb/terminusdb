const { expect } = require('chai')
const { Agent, db, document } = require('../lib')

describe('document-get-hierarchy', function () {
  let agent
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

  let instance

  before(async function () {
    agent = new Agent().auth()

    schema = [shape, circle, square]
    instance = shapes.concat(squares, circles)

    await db.create(agent)

    await document.insert(agent, { schema })
    await document.insert(agent, { instance })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('succeeds query for superclass', async function () {
    const r = await document.get(agent, { body: { query: { '@type': 'Shape' }, as_list: true } })
    for (const object of r.body) {
      delete object['@id']
    }
    expect(r.body).to.have.deep.members(instance)
  })

  describe('succeeds query for subclass', function () {
    const queries = [
      ['Circle', circles],
      ['Square', squares],
    ]
    for (const [type, instances] of queries) {
      it(type, async function () {
        const r = await document.get(agent, { body: { query: { '@type': type }, as_list: true } })
        for (const object of r.body) {
          delete object['@id']
        }
        expect(r.body).to.have.deep.members(instances)
      })
    }
  })
})
