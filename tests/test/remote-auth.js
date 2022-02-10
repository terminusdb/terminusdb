const { expect } = require('chai')
const { Agent, db, endpoint, remote } = require('../lib')

describe('remote-auth', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  it('adds a remote succesfully', async function () {
    const defaults = agent.defaults()
    const { path } = endpoint.db(defaults)
    await db.create(agent, path).then(db.verifyCreateSuccess)
    await agent.post(endpoint.remote(defaults).path).send({
      remote_name: 'origin',
      remote_location: 'http://somewhere.com/admin/foo',
    }).then(remote.verifySuccess)
    const remoteGetResponse = await agent.get(endpoint.remote(defaults).path).send({
      remote_name: 'origin',
    }).then(remote.verifySuccess)
    expect(remoteGetResponse.body['api:remote_names']).to.include('origin')
  })
})
