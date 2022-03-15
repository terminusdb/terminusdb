const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('schema-check-failures', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.createAfterDel(agent, endpoint.db(agent.defaults()).path)
  })

  after(async function () {
    await db.del(agent, endpoint.db(agent.defaults()).path)
  })

  it('fails replace schema with new non-optional field (#780)', async function () {
    // Insert an initial schema.
    const schema = { '@id': util.randomString(), '@type': 'Class' }
    const path = endpoint.document(agent.defaults()).path
    await document.insert(agent, path, { schema }).then(document.verifyInsertSuccess)
    // Insert an initial instance.
    const instance = { '@type': schema['@id'] }
    await document.insert(agent, path, { instance }).then(document.verifyInsertSuccess)
    // Update the schema with a new field that is not Optional.
    schema.name = 'xsd:string'
    const r = await document.replace(agent, path, { schema }).then(document.verifyReplaceFailure)
    expect(r.body['api:error']['@type']).to.equal('api:SchemaCheckFailure')
    expect(r.body['api:error']['api:witnesses']).to.be.an('array').that.has.lengthOf(1)
    expect(r.body['api:error']['api:witnesses'][0]['@type']).to.equal('instance_not_cardinality_one')
    expect(r.body['api:error']['api:witnesses'][0].class).to.equal('http://www.w3.org/2001/XMLSchema#string')
    expect(r.body['api:error']['api:witnesses'][0].predicate).to.equal('terminusdb:///schema#name')
  })

  it('fails insert schema with unknown class in field', async function () {
    const schema = {
      '@id': util.randomString(),
      '@type': 'Class',
      field: util.randomString(),
    }
    const path = endpoint.document(agent.defaults()).path
    const r = await document.insert(agent, path, { schema }).then(document.verifyInsertFailure)
    expect(r.body['api:error']['@type']).to.equal('api:SchemaCheckFailure')
    expect(r.body['api:error']['api:witnesses']).to.be.an('array').that.has.lengthOf(1)
    expect(r.body['api:error']['api:witnesses'][0]['@type']).to.equal('not_a_class_or_base_type')
    expect(r.body['api:error']['api:witnesses'][0].class).to.equal('terminusdb:///schema#' + schema.field)
  })

  describe('fails insert schema for type family with unknown @class (#1019)', function () {
    let schema
    const types = [
      'Array',
      'Cardinality',
      'List',
      'Optional',
      'Set',
      'Table',
    ]

    before(function () {
      schema = {
        '@id': util.randomString(),
        '@type': 'Class',
        field: {
          '@class': util.randomString(),
          // The '@cardinality' field is here to avoid triggering a different
          // error when the '@type' is 'Cardinality'.
          '@cardinality': 2,
        },
      }
    })

    for (const type of types) {
      it(type, async function () {
        schema.field['@type'] = type
        const path = endpoint.document(agent.defaults()).path
        const r = await document.insert(agent, path, { schema }).then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:SchemaCheckFailure')
        expect(r.body['api:error']['api:witnesses']).to.be.an('array').that.has.lengthOf(1)
        expect(r.body['api:error']['api:witnesses'][0]['@type']).to.equal('not_a_class_or_base_type')
        expect(r.body['api:error']['api:witnesses'][0].class).to.equal('terminusdb:///schema#' + schema.field['@class'])
      })
    }
  })
})
