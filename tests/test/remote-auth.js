const { expect } = require('chai')
const { Agent, db, endpoint } = require('../lib')

describe('remote-auth', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('adds a remote succesfully', async function () {
    const defaults = agent.defaults()
    const { path } = endpoint.db(defaults)
    await db.create(agent, path).then(db.verifyCreateSuccess)
    const remoteResponse = await agent.post(endpoint.remote(defaults).path).send({
      remote_name: 'origin',
      remote_location: 'http://somewhere.com/admin/foo',
    })
    expect(remoteResponse.status).to.equal(200)
    expect(remoteResponse.body['@type']).to.equal('api:RemoteResponse')
    expect(remoteResponse.body['api:status']).to.equal('api:success')
    const remoteGetResponse = await agent.get(endpoint.remote(defaults).path).send({
      remote_name: 'origin',
    })
    expect(remoteGetResponse.body['@type']).to.equal('api:RemoteResponse')
    expect(remoteGetResponse.body['api:status']).to.equal('api:success')
    expect(remoteGetResponse.body['api:remote_names']).to.include('origin')
  })
})
