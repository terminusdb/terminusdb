const { Agent, api, db, organization, util } = require('../lib')

describe('organization', function () {
  describe('fails add with missing parameter', function () {
    let agent

    before(async function () {
      agent = new Agent().auth()
    })

    const options = [
      ['{"organization_name":"abd"}', 'user_name'],
      ['{"user_name":"adj"}', 'organization_name'],
    ]
    for (const [bodyString, missingParam] of options) {
      it(bodyString, async function () {
        await organization
          .add(agent, { bodyString })
          .fails(api.error.missingParameter(missingParam))
      })
    }
  })

  it('passes add', async function () {
    const agent = new Agent({ orgName: util.randomString() }).auth()
    await organization.add(agent)
    await organization.delete(agent)
  })

  it('passes add with pipe in name', async function () {
    const agent = new Agent({ orgName: util.randomString() + '|pipe' }).auth()
    await organization.add(agent)
    await db.create(agent)
    await db.delete(agent)
    await organization.delete(agent)
  })

  it('fails add with unknown user', async function () {
    const agent = new Agent().auth()
    const user = util.randomString()
    await organization.add(agent, { user }).fails(api.error.unknownUser(user))
  })

  it('fails delete with unknown organization', async function () {
    const orgName = util.randomString()
    const agent = new Agent({ orgName }).auth()
    await organization.delete(agent).notFound(api.error.unknownOrganization(orgName))
  })
})
