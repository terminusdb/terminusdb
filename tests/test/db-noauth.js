const { Agent, api, db, util } = require('../lib')

describe('db-noauth', function () {
  let agent

  before(function () {
    agent = new Agent()
    // Change the organization name to trigger unknown organization errors.
    agent.orgName = util.randomString()
  })

  it('fails exists with bad parameter value', async function () {
    await db.exists(agent, { query: {} }).fails()
  })

  it('fails exists with bad parameter type', async function () {
    await db.exists(agent, { query: { exists: 5 } }).fails()
  })

  it('fails exists with unknown database', async function () {
    await db.exists(agent).notFound()
  })

  describe('fails create with missing label', function () {
    const parts = [
      '{}',
      '{"comment":"We test a non-empty comment string here."}',
    ]
    for (const bodyString of parts) {
      it(bodyString, async function () {
        await db.create(agent, { bodyString }).fails(api.error.missingParameter('label'))
      })
    }
  })

  it('fails create with invalid JSON', async function () {
    const bodyString = "{X{'''"
    await db
      .create(agent, { bodyString })
      .unverified()
      .then(api.response.verify(api.response.invalidJSON(bodyString)))
  })

  it('fails create with duplicate field (#603)', async function () {
    const bodyString = '{"comment":"c","comment":"c","label":"l"}'
    await db
      .create(agent, { bodyString })
      .unverified()
      .then(api.response.verify(api.response.duplicateField))
  })

  it('fails create with unknown organization', async function () {
    await db.create(agent).notFound(api.error.unknownOrganization(agent.orgName))
  })

  it('fails delete with unknown organization', async function () {
    await db.delete(agent).notFound(api.error.unknownOrganization(agent.orgName))
  })
})
