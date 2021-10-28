const { expect } = require('chai')
const { Agent, branch } = require('../lib')

describe('branch-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails on unknown descriptor', async function () {
    const r = await agent
      .post('/api/branch/unknowndesc')
      .send({})
      .then(branch.verifyFailure)
    expect(r.body['api:error']['@type']).to.equal('api:BadTargetAbsoluteDescriptor')
    expect(r.body['api:error']['api:absolute_descriptor']).to.equal('unknowndesc')
  })
})
