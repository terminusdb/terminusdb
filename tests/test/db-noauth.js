const { expect } = require('chai')
const { Agent, db, endpoint, util } = require('../lib')

describe('db-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
  })

  it('fails exists with bad parameter value', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent.head(path)
    expect(r.status).to.equal(400)
    expect(r.text).to.be.undefined
  })

  it('fails exists with bad parameter type', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent.head(path).query({ exists: 5 })
    expect(r.status).to.equal(400)
    expect(r.text).to.be.undefined
  })

  it('fails exists with unknown database', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent.head(path).query({ exists: true })
    expect(r.status).to.equal(404)
    expect(r.text).to.be.undefined
  })

  describe('fails create with missing fields', function () {
    const parts = [
      [{ }, 'label'],
      [{ comment: 'a comment' }, 'label'],
    ]
    for (const [body, missingParam] of parts) {
      it(JSON.stringify(body), async function () {
        const { path } = endpoint.db(agent.defaults())
        const r = await agent.post(path).send(body).then(db.verifyCreateFailure)
        expect(r.body['api:error']['@type']).to.equal('api:MissingParameter')
        expect(r.body['api:error']['api:parameter']).to.equal(missingParam)
      })
    }
  })

  describe('fails create with bad prefixes', function () {
    const prefixesList = [
      { '@base': 'x' },
      { '@schema': 'x' },
    ]
    for (const prefixes of prefixesList) {
      it(JSON.stringify(prefixes), async function () {
        const { path } = endpoint.db(agent.defaults())
        const r = await db.create(agent, path, { prefixes }).then(db.verifyCreateFailure)
        expect(r.body['api:error']['@type']).to.equal('api:InvalidPrefix')
        expect(r.body['api:error']['api:prefix_name']).to.equal(Object.keys(prefixes)[0])
        expect(r.body['api:error']['api:prefix_value']).to.equal(Object.values(prefixes)[0])
      })
    }
  })

  it('fails create with invalid JSON', async function () {
    const params = agent.defaults()
    const { path } = endpoint.db(params)
    const body = "{X{'''"
    const r = await agent.post(path).set('Content-Type', 'application/json').send(body)
    expect(r.status).to.equal(400)
    expect(r.body['api:status']).to.equal('api:failure')
    expect(r.body['api:message']).to.equal('Submitted object was not valid JSON')
    expect(r.body['system:object']).to.equal(body)
  })

  it('fails create with duplicate field (#603)', async function () {
    const { path } = endpoint.db(agent.defaults())
    const r = await agent
      .post(path)
      .type('application/json')
      .send('{"comment":"c","comment":"c","label":"l"}')
    expect(r.status).to.equal(400)
    expect(r.body['@type']).to.equal('api:DuplicateField')
    expect(r.body['api:status']).to.equal('api:failure')
  })

  it('fails create with unknown organization', async function () {
    const params = agent.defaults()
    params.orgName = util.randomString()
    const { path } = endpoint.db(params)
    const r = await db.create(agent, path).then(db.verifyCreateNotFound)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownOrganizationName')
    expect(r.body['api:error']['api:organization_name']).to.equal(params.orgName)
  })

  it('fails delete with unknown organization', async function () {
    const params = agent.defaults()
    params.orgName = util.randomString()
    const { path } = endpoint.db(params)
    const r = await db.del(agent, path).then(db.verifyDeleteNotFound)
    expect(r.body['api:error']['@type']).to.equal('api:UnknownOrganizationName')
    expect(r.body['api:error']['api:organization_name']).to.equal(params.orgName)
  })
})
