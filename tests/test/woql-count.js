const { expect } = require('chai')
const { Agent, db, woql } = require('../lib')

const node = (n) => ({ '@type': 'NodeValue', node: n })
const val = (v, t) => ({ '@type': 'Value', data: { '@type': t, '@value': v } })
const v = (name) => ({ '@type': 'Value', variable: name })

describe('woql-count', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent, { schema: false })

    // a1..a3 rdf:type T1, a4 rdf:type T2, plus integer-valued 'val' triples
    const setup = {
      '@type': 'And',
      and: [
        { '@type': 'AddTriple', subject: node('a1'), predicate: node('rdf:type'), object: node('T1') },
        { '@type': 'AddTriple', subject: node('a2'), predicate: node('rdf:type'), object: node('T1') },
        { '@type': 'AddTriple', subject: node('a3'), predicate: node('rdf:type'), object: node('T1') },
        { '@type': 'AddTriple', subject: node('a4'), predicate: node('rdf:type'), object: node('T2') },
        { '@type': 'AddTriple', subject: node('a1'), predicate: node('val'), object: val(5, 'xsd:integer') },
        { '@type': 'AddTriple', subject: node('a2'), predicate: node('val'), object: val(10, 'xsd:integer') },
        { '@type': 'AddTriple', subject: node('a3'), predicate: node('val'), object: val(20, 'xsd:integer') },
      ],
    }
    await woql.post(agent, setup)
  })

  after(async function () {
    await db.delete(agent)
  })

  describe('triple_count pattern', function () {
    it('counts triples matching a bound predicate and object', async function () {
      const query = {
        '@type': 'TripleCount',
        subject: { '@type': 'NodeValue', variable: 'S' },
        predicate: node('rdf:type'),
        object: node('T1'),
        count: v('N'),
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(Number(r.body.bindings[0].N['@value'])).to.equal(3)
    })

    it('counts triples matching a bound subject only', async function () {
      const query = {
        '@type': 'TripleCount',
        subject: node('a1'),
        predicate: { '@type': 'NodeValue', variable: 'P' },
        object: { '@type': 'Value', variable: 'O' },
        count: v('N'),
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(Number(r.body.bindings[0].N['@value'])).to.equal(2)
    })

    it('binds zero when nothing matches', async function () {
      const query = {
        '@type': 'TripleCount',
        subject: { '@type': 'NodeValue', variable: 'S' },
        predicate: node('rdf:type'),
        object: node('Nonexistent'),
        count: v('N'),
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(Number(r.body.bindings[0].N['@value'])).to.equal(0)
    })

    it('restricts the count to a named graph', async function () {
      const query = {
        '@type': 'TripleCount',
        subject: { '@type': 'NodeValue', variable: 'S' },
        predicate: node('rdf:type'),
        object: node('T1'),
        count: v('N'),
        graph: 'instance',
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(Number(r.body.bindings[0].N['@value'])).to.equal(3)
    })

    it('succeeds when the count is bound to the matching value', async function () {
      const query = {
        '@type': 'TripleCount',
        subject: { '@type': 'NodeValue', variable: 'S' },
        predicate: node('rdf:type'),
        object: node('T1'),
        count: val(3, 'xsd:integer'),
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
    })
  })

  describe('triple_range_count', function () {
    it('counts objects in a half-open value range', async function () {
      const query = {
        '@type': 'TripleRangeCount',
        subject: { '@type': 'NodeValue', variable: 'S' },
        predicate: node('val'),
        low: val(5, 'xsd:integer'),
        high: val(15, 'xsd:integer'),
        count: v('N'),
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(Number(r.body.bindings[0].N['@value'])).to.equal(2)
    })

    it('excludes the upper bound', async function () {
      const query = {
        '@type': 'TripleRangeCount',
        subject: { '@type': 'NodeValue', variable: 'S' },
        predicate: node('val'),
        low: val(4, 'xsd:integer'),
        high: val(20, 'xsd:integer'),
        count: v('N'),
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(Number(r.body.bindings[0].N['@value'])).to.equal(2)
    })

    it('binds zero for a datatype with no indexed values', async function () {
      const query = {
        '@type': 'TripleRangeCount',
        subject: { '@type': 'NodeValue', variable: 'S' },
        predicate: node('val'),
        low: val('a', 'xsd:string'),
        high: val('z', 'xsd:string'),
        count: v('N'),
      }
      const r = await woql.post(agent, query)
      expect(r.body.bindings).to.have.lengthOf(1)
      expect(Number(r.body.bindings[0].N['@value'])).to.equal(0)
    })
  })
})
