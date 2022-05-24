const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('document-delete', function () {
  let agent
  let dbPath
  let docPath

  before(async function () {
    agent = new Agent().auth()

    const dbDefaults = endpoint.db(agent.defaults())

    dbPath = dbDefaults.path
    await db.createAfterDel(agent, dbPath)

    docPath = endpoint.document(dbDefaults).path
  })

  after(async function () {
    await db.del(agent, dbPath)
  })

  describe('fails for bad parameter type', function () {
    const options = [
      [{ queryString: 'author=' }, 'author', 'non-empty string', ''],
      [{ queryString: 'author=a&message=' }, 'message', 'non-empty string', ''],
      [{ queryString: 'author=a&message=m&graph_type=' }, 'graph_type', 'graph (schema, instance)', ''],
      [{ queryString: 'author=a&message=m&graph_type=0' }, 'graph_type', 'graph (schema, instance)', '0'],
      [{ queryString: 'author=a&message=m&graph_type=null' }, 'graph_type', 'graph (schema, instance)', null],
      [{ queryString: 'author=a&message=m&nuke=' }, 'nuke', 'boolean (true, false)', ''],
      [{ queryString: 'author=a&message=m&nuke=null' }, 'nuke', 'boolean (true, false)', null],
      [{ queryString: 'author=a&message=m&nuke=abc' }, 'nuke', 'boolean (true, false)', 'abc'],
      [{ queryString: 'author=a&message=m&id=' }, 'id', 'non-empty string', ''],
      [{ bodyString: '0' }, 'id', 'non-empty string', 0],
      [{ bodyString: 'null' }, 'id', 'non-empty string', null],
      [{ bodyString: '[false]' }, 'id', 'non-empty string', false],
      [{ bodyString: '[true]' }, 'id', 'non-empty string', true],
      [{ bodyString: '["", "s"]' }, 'id', 'non-empty string', ''],
      [{ bodyString: '"" "s"' }, 'id', 'non-empty string', ''],
    ]
    for (const [option, paramName, paramType, value] of options) {
      it(JSON.stringify(option), async function () {
        const r = await document
          .del(agent, docPath, option)
          .then(document.verifyDelFailure)
        expect(r.body['api:error']['@type']).to.equal('api:BadParameterType')
        expect(r.body['api:error']['api:parameter']).to.equal(paramName)
        expect(r.body['api:error']['api:expected_type']).to.equal(paramType)
        expect(r.body['api:error']['api:value']).to.deep.equal(value)
      })
    }
  })

  it('fails for missing targets', async function () {
    const r = await document
      .del(agent, docPath)
      .then(document.verifyDelFailure)
    expect(r.body['api:error']['@type']).to.equal('api:MissingTargets')
  })

  it('fails for document not found', async function () {
    const id = util.randomString()
    const r = await document
      .del(agent, docPath, { query: { id } })
      .then(document.verifyDelNotFound)
    expect(r.body['api:error']['@type']).to.equal('api:DocumentNotFound')
    expect(r.body['api:error']).to.have.property('api:document_id').that.equals(id)
  })
})
