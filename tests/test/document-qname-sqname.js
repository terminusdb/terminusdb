const { expect } = require('chai')
const { Agent, db, document, woql } = require('../lib')

describe('QName and SQName expanded-name types', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
    const schema = {
      '@id': 'Names',
      '@type': 'Class',
      qname: 'xsd:QName',
      sqname: 'xbrl:SQName',
    }
    await document.insert(agent, { schema })
  })

  after(async function () {
    await db.delete(agent)
  })

  it('inserts and retrieves xsd:QName and xbrl:SQName fields as lexical values', async function () {
    const doc = {
      '@type': 'Names',
      qname: 'xbrl:canonicalValues',
      sqname: 'xbrl:a:b:c',
    }
    const inserted = await document.insert(agent, { instance: doc })
    const id = inserted.body[0]
    const got = await document.get(agent, { query: { id } })
    expect(got.body.qname).to.equal('xbrl:canonicalValues')
    expect(got.body.sqname).to.equal('xbrl:a:b:c')
  })

  it('round-trips via WOQL triple and renders lexically via the query context', async function () {
    const query = {
      '@type': 'Triple',
      subject: { '@type': 'NodeValue', variable: 'doc' },
      predicate: { '@type': 'NodeValue', node: 'qname' },
      object: { '@type': 'Value', variable: 'v' },
    }
    const r = await woql.post(agent, query)
    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].v).to.deep.equal({
      '@type': 'xsd:QName',
      '@value': 'xbrl:canonicalValues',
    })
  })

  it('typecasts string to xbrl:SQName with an explicit prefixes map', async function () {
    const query = {
      '@type': 'Typecast',
      value: { '@type': 'Value', data: { '@type': 'xsd:string', '@value': 'eg:foo:bar' } },
      type: { '@type': 'NodeValue', node: 'xbrl:SQName' },
      result: { '@type': 'Value', variable: 's' },
      prefixes: { '@type': 'Value', data: { eg: 'http://example.org/ns#' } },
    }
    const r = await woql.post(agent, query)
    expect(r.body.bindings).to.have.lengthOf(1)
    // The cast expands via the prefixes map; the binding renders via the
    // query context, where `eg` is unbound, so Clark form comes back.
    expect(r.body.bindings[0].s).to.deep.equal({
      '@type': 'xbrl:SQName',
      '@value': '{http://example.org/ns#}foo:bar',
    })
  })

  it('typecasts xsd:QName and keeps Clark form for unbound namespaces', async function () {
    const query = {
      '@type': 'Typecast',
      value: {
        '@type': 'Value',
        data: { '@type': 'xsd:string', '@value': '{http://unbound.example/}local' },
      },
      type: { '@type': 'NodeValue', node: 'xsd:QName' },
      result: { '@type': 'Value', variable: 'q' },
    }
    const r = await woql.post(agent, query)
    expect(r.body.bindings).to.have.lengthOf(1)
    expect(r.body.bindings[0].q).to.deep.equal({
      '@type': 'xsd:QName',
      '@value': '{http://unbound.example/}local',
    })
  })

  it('fails loudly on an unbound prefix', async function () {
    const query = {
      '@type': 'Typecast',
      value: { '@type': 'Value', data: { '@type': 'xsd:string', '@value': 'nope:foo' } },
      type: { '@type': 'NodeValue', node: 'xsd:QName' },
      result: { '@type': 'Value', variable: 'q' },
    }
    const r = await woql.post(agent, query).unverified()
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:BadCast')
  })

  it('rejects an SQName with whitespace in the localname', async function () {
    const query = {
      '@type': 'Typecast',
      value: { '@type': 'Value', data: { '@type': 'xsd:string', '@value': 'xbrl:a b' } },
      type: { '@type': 'NodeValue', node: 'xbrl:SQName' },
      result: { '@type': 'Value', variable: 's' },
    }
    const r = await woql.post(agent, query).unverified()
    expect(r.status).to.equal(400)
    expect(r.body['api:error']['@type']).to.equal('api:BadCast')
  })

  it('retags xbrl:SQName to xsd:QName only when the localname is an NCName', async function () {
    const ok = {
      '@type': 'Typecast',
      value: {
        '@type': 'Value',
        data: { '@type': 'xbrl:SQName', '@value': 'xbrl:foo' },
      },
      type: { '@type': 'NodeValue', node: 'xsd:QName' },
      result: { '@type': 'Value', variable: 'q' },
    }
    const okR = await woql.post(agent, ok)
    expect(okR.body.bindings).to.have.lengthOf(1)
    expect(okR.body.bindings[0].q).to.deep.equal({
      '@type': 'xsd:QName',
      '@value': 'xbrl:foo',
    })

    const bad = {
      '@type': 'Typecast',
      value: {
        '@type': 'Value',
        data: { '@type': 'xbrl:SQName', '@value': 'xbrl:a:b' },
      },
      type: { '@type': 'NodeValue', node: 'xsd:QName' },
      result: { '@type': 'Value', variable: 'q' },
    }
    const badR = await woql.post(agent, bad).unverified()
    expect(badR.status).to.equal(400)
    expect(badR.body['api:error']['@type']).to.equal('api:BadCast')
  })
})
