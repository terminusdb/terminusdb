const { expect } = require('chai')
const { Agent, db, endpoint, util } = require('../lib')

describe('db-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails on database exists for bad parameter value', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent
      .head(path)
    expect(r.status).to.equal(400)
    expect(r.text).to.be.undefined
  })

  it('fails on database exists with not found', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent
      .head(path)
      .query({ exists: true })
    expect(r.status).to.equal(404)
    expect(r.text).to.be.undefined
  })

  it('fails on missing comment and label', async function () {
    const { path } = endpoint.db(agent.defaults())
    const bodies = [
      {},
      { comment: 'c' },
      { label: 'l' },
    ]
    for (const body of bodies) {
      const r = await agent
        .post(path)
        .send(body)
      expect(r.status).to.equal(400)
      expect(r.body['api:status']).to.equal('api:failure')
      expect(r.body['@type']).to.equal('api:BadAPIDocumentErrorResponse')
      expect(r.body['api:error']['@type']).to.equal('api:RequiredFieldsMissing')
      expect(r.body['api:error']['api:expected'][0]).to.equal('comment')
      expect(r.body['api:error']['api:expected'][1]).to.equal('label')
    }
  })

  it('fails on duplicate field (#603)', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent
      .post(path)
      .type('application/json')
      .send('{"comment":"c","comment":"c","label":"l"}')
    expect(r.status).to.equal(400)
    expect(r.body['@type']).to.equal('api:DuplicateField')
    expect(r.body['api:status']).to.equal('api:failure')
  })

  it('reports unknown organization', async function () {
    const { path, orgName } = endpoint.db(
      Object.assign(agent.defaults(), {
        orgName: 'unknown-' + util.randomString(),
      }),
    )

    {
      const r = await db
        .create(agent, path)
        .then(db.verifyCreateFailure)
      expect(r.body['api:error']['@type']).to.equal('api:UnknownOrganization')
      expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    }
    {
      const r = await db
        .del(agent, path)
        .then(db.verifyDeleteFailure)
      expect(r.body['api:error']['@type']).to.equal('api:UnknownOrganization')
      expect(r.body['api:error']['api:organization_name']).to.equal(orgName)
    }
  })
})
