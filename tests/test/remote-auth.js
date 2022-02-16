const { expect } = require('chai')
const { Agent, db, endpoint, remote, util } = require('../lib')

describe('remote-auth', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    const { path } = endpoint.db(agent.defaults())
    await db.create(agent, path).then(db.verifyCreateSuccess)
  })

  after(async function () {
    const { path } = endpoint.db(agent.defaults())
    await db.del(agent, path)
  })

  it('passes post, get, put, delete, get', async function () {
    const { path } = endpoint.remote(agent.defaults())
    const query = {
      remote_name: util.randomString(),
      remote_location: 'http://example.com/foo',
    }
    // Post a remote
    await agent.post(path).send(query).then(remote.verifySuccess)
    {
      // Query the remote
      const { remote_name, remote_location } = query // eslint-disable-line camelcase
      const r = await agent.get(path).query({ remote_name }).then(remote.verifySuccess)
      expect(r.body['api:remote_name']).to.equal(remote_name)
      expect(r.body['api:remote_url']).to.equal(remote_location)
    }
    {
      // Query all remotes
      const r = await agent.get(path).then(remote.verifySuccess)
      expect(r.body['api:remote_names']).to.be.an('array').that.has.lengthOf(1)
      expect(r.body['api:remote_names'][0]).to.equal(query.remote_name)
    }
    query.remote_location = 'http://example.com/bar'
    // Put a new remote
    await agent.put(path).send(query).then(remote.verifySuccess)
    {
      // Query the remote
      const { remote_name, remote_location } = query // eslint-disable-line camelcase
      const r = await agent.get(path).query({ remote_name }).then(remote.verifySuccess)
      expect(r.body['api:remote_name']).to.equal(remote_name)
      expect(r.body['api:remote_url']).to.equal(remote_location)
    }
    // Delete the remote
    await agent.delete(path).send(query).then(remote.verifySuccess)
    {
      // Query the remote
      const r = await agent.get(path).query(query).then(remote.verifyNotFound)
      expect(r.body['api:error']['@type']).to.equal('api:RemoteDoesNotExist')
      expect(r.body['api:error']['api:remote_name']).to.equal(query.remote_name)
    }
  })
})
