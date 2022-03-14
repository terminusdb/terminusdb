const childProcess = require('child_process')
const { expect } = require('chai')
const { Agent } = require('../lib')

describe('info_ok', function () {
  let agent
  let gitHash

  before(function () {
    agent = new Agent()
    gitHash = childProcess
      .execSync('git rev-parse --verify HEAD')
      .toString()
      .trim()
  })

  it('responds to /api/ok with success', async function () {
    const r = await agent.get('/api/ok')
    expect(r.status).to.equal(200)
    expect(r.header['content-length']).to.equal('0')
  })

  it('responds to /api/info with success', async function () {
    const r = await agent.get('/api/info')
    expect(r.status).to.equal(200)
    expect(r.body['api:status']).to.equal('api:success')
    expect(r.body['@type']).to.equal('api:InfoResponse')
    expect(r.body['api:info']).to.have.property('authority').that.equals('anonymous')
    expect(r.body['api:info']).to.have.property('storage').that.is.an('object')
    expect(r.body['api:info'].storage).to.have.property('version').that.is.a('string').and.lengthOf.greaterThan(0)
    expect(r.body['api:info']).to.have.property('terminusdb').that.is.an('object')
    expect(r.body['api:info'].terminusdb).to.have.property('version').that.is.a('string').and.lengthOf.greaterThan(0)
    expect(r.body['api:info'].terminusdb).to.have.property('git_hash').that.equals(gitHash)
    expect(r.body['api:info']).to.have.property('terminusdb_store').that.is.an('object')
    expect(r.body['api:info'].terminusdb_store).to.have.property('version').that.is.a('string').and.lengthOf.greaterThan(0)
  })
})
