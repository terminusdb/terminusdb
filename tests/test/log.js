const { expect } = require('chai')
const { Agent, util, document, db } = require('../lib')

describe('log', function () {
  let agent

  before(function () {
    agent = new Agent({ dbName: 'hello' }).auth()
  })

  it('gets a log from changes', async function () {
    await agent.post('/api/db/admin/hello').send({ label: 'Hello' })

    const id = util.randomString()
    const schema = { '@type': 'Class', '@id': id }
    await document.insert(agent, { schema })
    const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
    const result1 = await document.insert(agent, { instance: instance1 })

    const version1 = result1.headers['terminusdb-data-version'].split('branch:')[1]
    const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }

    const result2 = await document.insert(agent, { instance: instance2 })
    const version2 = result2.headers['terminusdb-data-version'].split('branch:')[1]

    const logRequest = await agent.get('/api/log/admin/hello')
    const log = logRequest.body

    expect(log).to.have.lengthOf(3)
    expect(log[0].author).to.equal('default_author')
    expect(log[0].identifier).to.equal(version2)
    expect(log[1].identifier).to.equal(version1)

    // cleanup
    await agent.delete('/api/db/admin/hello')
  })
})
