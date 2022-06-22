const { Agent, api, db, document, util } = require('../lib')

describe('document-delete', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
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
        await document
          .delete(agent, option)
          .fails(api.error.badParameterType(paramName, paramType, value))
      })
    }
  })

  it('fails for missing targets', async function () {
    await document.delete(agent).fails(api.error.missingTargets)
  })

  it('fails for document not found', async function () {
    const id = util.randomString()
    await document.delete(agent, { query: { id } }).notFound(id)
  })
})
