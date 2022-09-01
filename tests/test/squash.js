const { expect } = require('chai')
const { Agent, util, document } = require('../lib')

describe('squash', function () {
  let agent
  let dbName

  before(function () {
    dbName = util.randomString()
    agent = new Agent({ dbName }).auth()
  })

  it('squash two commits in one', async function () {
    await agent.post(`/api/db/admin/${dbName}`).send({ label: 'Hello' })

    const id = util.randomString()
    const schema = { '@type': 'Class', '@id': id }
    await document.insert(agent, { schema })
    const instance1 = { '@type': id, '@id': `terminusdb:///data/${id}/0` }
    await document.insert(agent, { instance: instance1 })
    const instance2 = { '@type': id, '@id': `terminusdb:///data/${id}/1` }
    await document.insert(agent, { instance: instance2 })
    const squash = await agent.post(`/api/squash/admin/${dbName}`).send({
      commit_info: {
        author: 'me',
        message: 'squash',
      },
    })
    const commitId = squash.body['api:commit']
    expect(squash.body['api:status']).to.equal('api:success')
    expect(squash.body['@type']).to.equal('api:SquashResponse')
    const documentsRequest = await agent.get(`/api/document/${commitId}`).query('as_list=true')
    const documents = documentsRequest.body
    expect(documents).to.have.lengthOf(2)
    expect(documents[0]['@id']).to.equal(`${id}/0`)
    expect(documents[1]['@id']).to.equal(`${id}/1`)
    // cleanup
    await agent.delete(`/api/db/admin/${dbName}`)
  })
})
