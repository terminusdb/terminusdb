const { expect } = require('chai')
const { Agent, api, db, organization, util } = require('../lib')

describe('organization', function () {
  describe('fails add with missing parameter', function () {
    let agent

    before(async function () {
      agent = new Agent().auth()
    })
  })

  it('passes add', async function () {
    const agent = new Agent().auth()
    const orgName = util.randomString()
    const result = await agent.post(`/api/organizations/${orgName}`)
    expect(result.body).to.equal(`terminusdb://system/data/Organization/${orgName}`)
    expect(result.status).to.equal(200)
  })

  it('passes add with pipe in name', async function () {
    const agent = new Agent().auth()
    const orgBase = util.randomString()
    const orgName = orgBase + '|pipe'
    const result = await agent.post(`/api/organizations/${orgName}`)
    expect(result.body).to.equal(`terminusdb://system/data/Organization/${orgBase}%7Cpipe`)
    expect(result.status).to.equal(200)
  })

  it('fails delete with unknown organization', async function () {
    const orgName = util.randomString()
    const agent = new Agent({ orgName }).auth()
    const result = await agent.delete(`/api/organizations/${orgName}`)
    expect(result.status).to.equal(404)
    expect(result.body['api:error']).to.deep.equal({ '@type' : 'api:NoIdForOrganizationName',
                                                     'api:organization_name' : orgName })
    expect(result.body['@type']).to.equal('api:OrganizationErrorResponse')
  })
})
