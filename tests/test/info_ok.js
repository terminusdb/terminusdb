const { expect } = require('chai')
const { Agent, info, util } = require('../lib')

describe('info_ok', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  it('responds to /api/ok with a ready status', async function () {
    const r = await agent.get('/api/ok')
    expect(r.status).to.equal(200)
    expect(r.body).to.have.property('status').that.equals('ready')
  })

  it('responds to /api/info with success (git hash matches binary version)', async function () {
    const terminusdbVersion = await info.terminusdbVersion()
    const gitHash = await info.gitHash()
    // Enterprise withholds storage and version details from anonymous callers,
    // so authenticate to exercise the full response contract.
    if (util.enterprise) {
      agent.auth()
    }
    const r = await agent.get('/api/info')
    expect(r.status).to.equal(200)
    expect(r.body['api:status']).to.equal('api:success')
    expect(r.body['@type']).to.equal('api:InfoResponse')
    expect(r.body['api:info']).to.have.property('authority').that.equals('terminusdb://system/data/User/admin')
    expect(r.body['api:info']).to.have.property('storage').that.is.an('object')
    expect(r.body['api:info'].storage).to.have.property('version').that.equals('2')
    expect(r.body['api:info']).to.have.property('terminusdb').that.is.an('object')
    expect(r.body['api:info'].terminusdb).to.have.property('version').that.equals(terminusdbVersion)
    expect(r.body['api:info'].terminusdb).to.have.property('git_hash').that.equals(gitHash)
    expect(r.body['api:info']).to.have.property('terminusdb_store').that.is.an('object')
    expect(r.body['api:info'].terminusdb_store).to.have.property('version').that.is.a('string').and.lengthOf.greaterThan(0)
    if (util.enterprise) {
      expect(r.body['api:info']).to.have.property('enterprise').that.is.an('object')
    }
  })
})
