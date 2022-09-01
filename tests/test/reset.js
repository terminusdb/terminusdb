const { expect } = require('chai')
const { Agent, util, document } = require('../lib')

describe('reset', function () {
  let agent
  let dbName

  before(function () {
    dbName = util.randomString()
    agent = new Agent({ dbName }).auth()
  })

  it('resets to previous commit on API reset call', async function () {
    await agent.post(`/api/db/admin/${dbName}`).send({ label: 'Hello' })

    await document.insert(agent, { bodyString: '{ "text": "test1" }', raw_json: true })
    await document.insert(agent, { bodyString: '{ "text": "test2" }', raw_json: true })
    await document.insert(agent, { bodyString: '{ "text": "test3" }', raw_json: true })
    const logRequest = await agent.get(`/api/log/admin/${dbName}`)
    const resetCommit = logRequest.body[1].identifier
    const resetRequest = await agent.post(`/api/reset/admin/${dbName}`).send({ commit_descriptor: resetCommit })
    const resetResponse = resetRequest.body
    expect(resetResponse).to.deep.equal({ '@type': 'api:ResetResponse', 'api:status': 'api:success' })
    const docRequest = await agent.get(`/api/document/admin/${dbName}`).query('as_list=true')
    const docs = docRequest.body
    expect(docs[0].text).to.equal('test1')
    expect(docs[1].text).to.equal('test2')
    expect(docs).to.have.lengthOf(2)
    // cleanup
    await agent.delete(`/api/db/admin/${dbName}`)
  })
})
