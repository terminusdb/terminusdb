const { expect } = require('chai')
const { Agent, api, db, remote, util } = require('../lib')

describe('remote-auth', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
    await db.create(agent)
  })

  after(async function () {
    await db.delete(agent)
  })

  it('passes add, get, update, delete, get', async function () {
    const query = {
      remote_name: util.randomString(),
      remote_location: 'http://example.com/foo',
    }
    // Post a remote
    await remote.add(agent, query)
    {
      // Query the remote
      const { remote_name, remote_location } = query // eslint-disable-line camelcase
      const r = await remote.get(agent).query({ remote_name })
      expect(r.body['api:remote_name']).to.equal(remote_name)
      expect(r.body['api:remote_url']).to.equal(remote_location)
    }
    {
      // Query all remotes
      const r = await remote.get(agent)
      expect(r.body['api:remote_names']).to.be.an('array').that.has.lengthOf(1)
      expect(r.body['api:remote_names'][0]).to.equal(query.remote_name)
    }
    query.remote_location = 'http://example.com/bar'
    // Put a new remote
    await remote.update(agent, query)
    {
      // Query the remote
      const { remote_name, remote_location } = query // eslint-disable-line camelcase
      const r = await remote.get(agent).query({ remote_name })
      expect(r.body['api:remote_name']).to.equal(remote_name)
      expect(r.body['api:remote_url']).to.equal(remote_location)
    }
    // Delete the remote
    await remote.delete(agent, query)
    // Query the remote
    await remote
      .get(agent)
      .query(query)
      .notFound(api.error.remoteDoesNotExist(query.remote_name))
  })

  it('deletes with post payload', async function () {
    const query = {
      remote_name: util.randomString(),
      remote_location: 'http://example.com/foo',
    }
    // Post a remote
    await remote.add(agent, query)
    // Delete the remote
    const path = api.path.remote(agent)
    await agent.delete(path).send(query)
    // Query the remote
    await remote
      .get(agent)
      .query(query)
      .notFound(api.error.remoteDoesNotExist(query.remote_name))
  })
})
