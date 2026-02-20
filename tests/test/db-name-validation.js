const { Agent, api, db } = require('../lib')

const invalidDatabaseNameOperations = [
  { method: 'create', dbName: '' },
  { method: 'create', dbName: 'it|has|pipes' },
  { method: 'delete', dbName: '' },
  { method: 'delete', dbName: 'it|has|pipes' },
]

const invalidOrganizationNameOperations = [
  { method: 'create', orgName: '' },
  { method: 'delete', orgName: '' },
]

describe('db-name-validation', function () {
  let agent

  before(async function () {
    agent = new Agent().auth()
  })

  describe('fails with invalid database name', function () {
    for (const { method, dbName } of invalidDatabaseNameOperations) {
      it(`${method}: '${dbName}'`, async function () {
        agent.dbName = dbName
        await db[method](agent).fails(api.error.invalidDatabaseName(dbName))
      })
    }
  })

  describe('fails with invalid organization name', function () {
    for (const { method, orgName } of invalidOrganizationNameOperations) {
      it(`${method}: '${orgName}'`, async function () {
        agent.orgName = orgName
        await db[method](agent).fails(api.error.invalidOrganizationName(orgName))
      })
    }
  })
})
