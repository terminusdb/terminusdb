const { expect } = require('chai')
const { Agent, db, endpoint, remote, util } = require('../lib')

describe('remote-auth', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    const defaults = agent.defaults()
    const { path } = endpoint.db(defaults)
    await db.create(agent, path).then(db.verifyCreateSuccess)
  })

  after(async function () {
    const defaults = agent.defaults()
    const { path } = endpoint.db(defaults)
    await db.del(agent, path)
  })

  it('adds a remote succesfully', async function () {
    const defaults = agent.defaults()
    await agent.post(endpoint.remote(defaults).path).send({
      remote_name: 'origin',
      remote_location: 'http://somewhere.com/admin/foo',
    }).then(remote.verifySuccess)
    const remoteGetResponse = await agent.get(endpoint.remote(defaults).path).query({
      remote_name: 'origin',
    }).then(remote.verifySuccess)
    expect(remoteGetResponse.body['api:remote_name']).to.equal('origin')
    expect(remoteGetResponse.body['api:remote_url']).to.equal('http://somewhere.com/admin/foo')
  })

  it('get a remote succesfully', async function () {
    const defaults = agent.defaults()
    const remoteName = util.randomString()
    await agent.post(endpoint.remote(defaults).path).send({
      remote_name: remoteName,
      remote_location: 'http://somewhere.com/admin/foo',
    }).then(remote.verifySuccess)
    const remoteGetResponse = await agent.get(endpoint.remote(defaults).path)
      .query({ remote_name: remoteName })
      .then(remote.verifySuccess)
    expect(remoteGetResponse.body['api:remote_name']).to.equal(remoteName)
    expect(remoteGetResponse.body['api:remote_url']).to.equal('http://somewhere.com/admin/foo')
  })

  it('delete a remote succesfully', async function () {
    const defaults = agent.defaults()
    const remoteName = util.randomString()
    await agent.post(endpoint.remote(defaults).path).send({
      remote_name: remoteName,
      remote_location: 'http://somewhere.com/admin/foo',
    }).then(remote.verifySuccess)
    await agent.delete(endpoint.remote(defaults).path)
      .send({ remote_name: remoteName })
      .then(remote.verifySuccess)
    const remoteGetResponse = await agent.get(endpoint.remote(defaults).path)
      .query({ remote_name: remoteName })
    expect(remoteGetResponse.status).to.equal(400)
  })

  it('get a remote list succesfully', async function () {
    const defaults = agent.defaults()
    const remoteName = util.randomString()
    await agent.post(endpoint.remote(defaults).path).send({
      remote_name: remoteName,
      remote_location: 'http://somewhere.com/admin/foo',
    }).then(remote.verifySuccess)
    const remoteGetResponse = await agent.get(endpoint.remote(defaults).path)
      .then(remote.verifySuccess)
    expect(remoteGetResponse.body['api:remote_names']).to.include(remoteName)
  })

  it('sets a remote succesfully', async function () {
    const defaults = agent.defaults()
    const remoteName = util.randomString()
    await agent.post(endpoint.remote(defaults).path).send({
      remote_name: remoteName,
      remote_location: 'http://somewhere.com/admin/foo',
    }).then(remote.verifySuccess)
    await agent.put(endpoint.remote(defaults).path).send({
      remote_name: remoteName,
      remote_location: 'http://somewhere.com/admin/foo2',
    }).then(remote.verifySuccess)
    const remoteGetResponse = await agent.get(endpoint.remote(defaults).path)
      .query({ remote_name: remoteName })
      .then(remote.verifySuccess)
    expect(remoteGetResponse.body['api:remote_name']).to.equal(remoteName)
    expect(remoteGetResponse.body['api:remote_url']).to.equal('http://somewhere.com/admin/foo2')
  })
})
