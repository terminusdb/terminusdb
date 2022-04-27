const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('document-full-replace', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.createAfterDel(agent, endpoint.db(agent.defaults()).path)
  })

  after(async function () {
    await db.del(agent, endpoint.db(agent.defaults()).path)
  })

  it('passes insert schema twice', async function () {
    const option = {
      schema: [
        util.defaultContext,
        { '@type': 'Class', '@id': util.randomString() },
        { '@type': 'Class', '@id': util.randomString() },
      ],
      fullReplace: true,
    }
    const path = endpoint.document(agent.defaults()).path
    await document.insert(agent, path, option).then(document.verifyInsertSuccess)
    await document.insert(agent, path, option).then(document.verifyInsertSuccess)
  })

  it('passes insert instance twice', async function () {
    const schema = { '@type': 'Class', '@id': util.randomString() }
    const path = endpoint.document(agent.defaults()).path
    await document.insert(agent, path, { schema }).then(document.verifyInsertSuccess)
    const option = {
      instance: [
        { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}` },
        { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}` },
      ],
      fullReplace: true,
    }
    await document.insert(agent, path, option).then(document.verifyInsertSuccess)
    await document.insert(agent, path, option).then(document.verifyInsertSuccess)
  })

  describe('fails insert schema with duplicate @id (#1063)', function () {
    for (const fullReplace of [false, true]) {
      it(`full_replace=${fullReplace}`, async function () {
        const option = { fullReplace }
        const id = util.randomString()
        const schema = [
          { '@type': 'Class', '@id': id },
          { '@type': 'Enum', '@id': id, '@value': ['hello'] },
        ]
        if (fullReplace) {
          option.schema = [util.defaultContext].concat(schema)
        } else {
          option.schema = schema
        }
        const r = await document.insert(agent, endpoint.document(agent.defaults()).path, option).then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:SameDocumentIdsMutatedInOneTransaction')
        expect(r.body['api:error']['api:duplicate_ids']).to.be.an('array').that.has.lengthOf(1)
        expect(r.body['api:error']['api:duplicate_ids'][0]).to.equal(id)
      })
    }
  })

  describe('handles insert with empty request body', function () {
    const options = [
      { schema: {}, fullReplace: false },
      { schema: {}, fullReplace: true },
      { instance: {}, fullReplace: false },
      { instance: {}, fullReplace: true },
    ]
    for (const option of options) {
      it(JSON.stringify(option), async function () {
        option.bodyString = ''
        const r = await document.insert(agent, endpoint.document(agent.defaults()).path, option)
        if (util.isDefined(option.schema) && option.fullReplace) {
          document.verifyInsertFailure(r)
          expect(r.body['api:error']['@type']).to.equal('api:NoContextFoundInSchema')
        } else {
          document.verifyInsertSuccess(r)
        }
      })
    }
  })

  it('passes insert schema with full_replace', async function () {
    const id = util.randomString()
    const schema = [
      util.defaultContext,
      { '@type': 'Class', '@id': id, x: 'xsd:integer' },
    ]
    await document
      .insert(agent, endpoint.document(agent.defaults()).path, { schema, fullReplace: true })
      .then(document.verifyInsertSuccess)
    const r = await document
      .get(agent, endpoint.document(agent.defaults()).path, { query: { graph_type: 'schema', id } })
      .then(document.verifyGetSuccess)
    expect(r.body['@id']).to.equal(id)
  })

  it('fails insert schema with full_replace and no @context', async function () {
    const r = await document
      .insert(agent, endpoint.document(agent.defaults()).path, {
        schema: { '@type': 'Class', '@id': util.randomString() },
        fullReplace: true,
      })
      .then(document.verifyInsertFailure)
    expect(r.body['api:error']['@type']).to.equal('api:NoContextFoundInSchema')
  })
})
