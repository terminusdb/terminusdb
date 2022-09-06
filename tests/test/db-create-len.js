const { expect } = require('chai')
const { Agent, db, util } = require('../lib')

describe('db-create-len', function () {
  const totalAllowed = 246 // This is the longest a combination of user and dbname can be. Together with '.label' and the separator '%7c' (encoded '/') this adds up to 255 characters, the maximum file name length on posix systems
  let agent
  before(async function () {
    agent = new Agent().auth()
  })

  beforeEach(function () {
    // Use a unique database for each test.
    agent.dbName = util.randomString()
  })

  it('passes db creation for a name that is just short enough', async function () {
    const namePrefix = util.randomString()
    const orgnameLen = agent.orgName.length
    const name = namePrefix + 'a'.repeat(totalAllowed - namePrefix.length - orgnameLen)
    agent.dbName = name
    await db.create(agent)
    await db.exists(agent)
    await db.delete(agent)
  })

  it('fails db creation for a name that just exceeds the name length limit', async function () {
    const namePrefix = util.randomString()
    const orgnameLen = agent.orgName.length
    const name = namePrefix + 'a'.repeat(totalAllowed - namePrefix.length - orgnameLen + 1)
    agent.dbName = name
    const result = await db.create(agent).unverified()
    expect(result.status).to.equal(400)
    expect(result.body['api:error']['@type']).to.equal('api:DatabaseNameTooLong')
    await !db.exists(agent)
  })
})
