const { expect } = require('chai')
const { Agent, api, db, document, util } = require('../lib')

describe('document-full-replace', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
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
    await document.insert(agent, option)
    await document.insert(agent, option)
  })

  it('passes insert instance twice', async function () {
    const schema = { '@type': 'Class', '@id': util.randomString() }
    await document.insert(agent, { schema })
    const option = {
      instance: [
        { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}` },
        { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}` },
      ],
      fullReplace: true,
    }
    await document.insert(agent, option)
    await document.insert(agent, option)
  })

    it('handles captures correctly in a full instance replace', async function() {
        let classId = util.randomString()
        const schema = { '@type': 'Class', '@id': classId, 'other': classId }
        await document.insert(agent, { schema })
        const option = {
            instance: [
                { '@type': schema['@id'], '@capture': 'Ref_A', 'other': {'@ref': 'Ref_B'} },
                { '@type': schema['@id'], '@capture': 'Ref_B', 'other': {'@ref': 'Ref_A'} },
            ],
            fullReplace: true,
        }
        await document.insert(agent, option)

        let result = await document.get(agent, {query: {as_list: true}})
        let documents = result.body
        let id1 = documents[0]['@id']
        let id2 = documents[1]['@id']
        let other1 = documents[0]['other']
        let other2 = documents[1]['other']
        expect(id1).to.equal(other2)
        expect(id2).to.equal(other1)
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
        await document.insert(agent, option).fails(api.error.sameDocumentIdsMutatedInOneTransaction([id]))
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
        if (util.isDefined(option.schema) && option.fullReplace) {
          await document.insert(agent, option).fails(api.error.noContextFoundInSchema)
        } else {
          await document.insert(agent, option)
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
    await document.insert(agent, { schema, fullReplace: true })
    const r = await document.get(agent, { query: { graph_type: 'schema', id } })
    expect(r.body['@id']).to.equal(id)
  })

  it('fails insert schema with full_replace and no @context', async function () {
    await document
      .insert(agent, {
        schema: { '@type': 'Class', '@id': util.randomString() },
        fullReplace: true,
      })
      .fails(api.error.noContextFoundInSchema)
  })
})
