const { expect } = require('chai')
const { Agent, util } = require('../lib')

describe('organization', function () {
  it('passes add', async function () {
    const agent = new Agent().auth()
    const orgName = util.randomString()
    const result = await agent.post(`/api/organizations/${orgName}`)
    expect(result.body).to.equal(`terminusdb://system/data/Organization/${orgName}`)
    expect(result.status).to.equal(200)
    await agent.delete(`/api/organizations/${orgName}`)
  })

  it('passes add with pipe in name', async function () {
    const agent = new Agent().auth()
    const orgBase = util.randomString()
    const orgName = orgBase + '|pipe'
    const result = await agent.post(`/api/organizations/${orgName}`)
    expect(result.body).to.equal(`terminusdb://system/data/Organization/${orgBase}%7Cpipe`)
    expect(result.status).to.equal(200)
    await agent.delete(`/api/organizations/${orgName}`)
  })

  it('passes delete', async function () {
    const agent = new Agent().auth()
    const orgName = util.randomString()
    await agent.post(`/api/organizations/${orgName}`)
    const result = await agent.delete(`/api/organizations/${orgName}`)
    expect(result.body).to.deep.equal({ '@type': 'api:RolesResponse', 'api:status': 'api:success' })
    expect(result.status).to.equal(200)
  })

  it('fails delete with unknown organization', async function () {
    const orgName = util.randomString()
    const agent = new Agent({ orgName }).auth()
    const result = await agent.delete(`/api/organizations/${orgName}`)
    expect(result.status).to.equal(404)
    expect(result.body['api:error']).to.deep.equal({
      '@type': 'api:NoIdForOrganizationName',
      'api:organization_name': orgName,
    })
    expect(result.body['@type']).to.equal('api:OrganizationErrorResponse')
  })

  it('passes get', async function () {
    const agent = new Agent().auth()
    const orgName = util.randomString()
    await agent.post(`/api/organizations/${orgName}`)

    const result = await agent.get(`/api/organizations/${orgName}`)
    expect(result.body.name).to.equal(orgName)
    expect(result.status).to.equal(200)
    await agent.delete(`/api/organizations/${orgName}`)
  })

  it('gets everything', async function () {
    const agent = new Agent().auth()
    const result = await agent.get('/api/organizations')
    expect(result.body).to.be.an('Array')
    expect(result.status).to.equal(200)
  })
})
