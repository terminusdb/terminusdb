const { expect } = require('chai')
const { Agent, db, document, endpoint } = require('../lib')

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
      [{ queryString: 'author=' }, 'author', 'string', ''],
      [{ queryString: 'author=a&message=' }, 'message', 'string', ''],
      [{ queryString: 'author=a&message=m&graph_type=' }, 'graph_type', 'graph', ''],
      [{ queryString: 'author=a&message=m&graph_type=0' }, 'graph_type', 'graph', '0'],
      [{ queryString: 'author=a&message=m&graph_type=null' }, 'graph_type', 'graph', null],
      [{ queryString: 'author=a&message=m&nuke=' }, 'nuke', 'boolean', ''],
      [{ queryString: 'author=a&message=m&nuke=null' }, 'nuke', 'boolean', null],
      [{ queryString: 'author=a&message=m&nuke=abc' }, 'nuke', 'boolean', 'abc'],
      [{ queryString: 'author=a&message=m&id=' }, 'id', 'string', ''],
      [{ bodyString: '0' }, 'id', 'string', 0],
      [{ bodyString: 'null' }, 'id', 'string', null],
      [{ bodyString: '[false]' }, 'id', 'string', false],
      [{ bodyString: '[true]' }, 'id', 'string', true],
      [{ bodyString: '["", "s"]' }, 'id', 'string', ''],
      [{ bodyString: '"" "s"' }, 'id', 'string', ''],
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
})
