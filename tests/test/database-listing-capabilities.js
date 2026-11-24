const { expect } = require('chai')
const { Agent, util } = require('../lib')

/**
 * Comprehensive tests for database listing with database-level capabilities
 *
 * Bug: Users with database-level capabilities cannot see their authorized databases
 * when listing databases via GET /api/db/{org}
 *
 * Root Cause: Path query pattern requires exactly 1 'database' edge hop,
 * but database-level capabilities point directly to the database (0 hops)
 */
describe('database-listing-capabilities', function () {
  it('database-level capability: user can list their authorized database', async function () {
    const adminAgent = new Agent().auth()
    const orgName = util.randomString()
    const userName = util.randomString()
    const dbName1 = util.randomString()
    const dbName2 = util.randomString()

    // Create user
    await adminAgent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    // Create organization
    const orgCreate = await adminAgent.post(`/api/organizations/${orgName}`)
    expect(orgCreate.status).to.equal(200)

    // Create two databases in the organization
    const db1Create = await adminAgent.post(`/api/db/${orgName}/${dbName1}`)
      .send({ label: dbName1, comment: '' })
    expect(db1Create.status).to.equal(200)
    const db2Create = await adminAgent.post(`/api/db/${orgName}/${dbName2}`)
      .send({ label: dbName2, comment: '' })
    expect(db2Create.status).to.equal(200)

    // Get database resource ID from list_databases with verbose flag
    const dbList = await adminAgent.get(`/api/db/${orgName}?verbose=true`)
    expect(dbList.status).to.equal(200)
    const db1 = dbList.body.find(db => db.path === `${orgName}/${dbName1}`)
    expect(db1).to.be.an('object')
    const dbResourceId = db1['@id']
    expect(dbResourceId).to.be.a('string')
    expect(dbResourceId).to.match(/^UserDatabase\//)

    // Grant DATABASE-LEVEL capability using resource ID
    const grantResult = await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: dbResourceId,
        user: `User/${userName}`,
        roles: ['Role/consumer'],
      })
    expect(grantResult.status).to.equal(200)

    // Switch to user's credentials
    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // TEST: User should see dbName1 (authorized) but NOT dbName2 (unauthorized)
    const listResult = await userAgent.get('/api/db')
    expect(listResult.status).to.equal(200)

    const databases = listResult.body
    expect(databases).to.be.an('array')
    // Filter to only this organization's databases
    const orgDatabases = databases.filter(db => db.path.startsWith(`${orgName}/`))
    expect(orgDatabases.length).to.equal(1, 'User should see exactly 1 database')
    expect(orgDatabases[0].path).to.equal(`${orgName}/${dbName1}`)

    // Cleanup
    await adminAgent.delete(`/api/db/${orgName}/${dbName1}`)
    await adminAgent.delete(`/api/db/${orgName}/${dbName2}`)
  })

  it('database-level capability: user cannot see unauthorized databases', async function () {
    const adminAgent = new Agent().auth()
    const orgName = util.randomString()
    const userName = util.randomString()
    const dbName1 = util.randomString()
    const dbName2 = util.randomString()
    const dbName3 = util.randomString()

    // Create user
    await adminAgent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    // Create organization
    await adminAgent.post(`/api/organizations/${orgName}`)

    // Create THREE databases
    const db1Create = await adminAgent.post(`/api/db/${orgName}/${dbName1}`)
      .send({ label: dbName1, comment: '' })
    expect(db1Create.status).to.equal(200)
    const db2Create = await adminAgent.post(`/api/db/${orgName}/${dbName2}`)
      .send({ label: dbName2, comment: '' })
    expect(db2Create.status).to.equal(200)
    const db3Create = await adminAgent.post(`/api/db/${orgName}/${dbName3}`)
      .send({ label: dbName3, comment: '' })
    expect(db3Create.status).to.equal(200)

    // Get database resource ID from list_databases with verbose flag
    const dbList = await adminAgent.get(`/api/db/${orgName}?verbose=true`)
    expect(dbList.status).to.equal(200)
    const db2 = dbList.body.find(db => db.path === `${orgName}/${dbName2}`)
    expect(db2).to.be.an('object')
    const dbResourceId = db2['@id']
    expect(dbResourceId).to.be.a('string')
    expect(dbResourceId).to.match(/^UserDatabase\//)

    // Grant access to ONLY dbName2 using resource ID
    await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: dbResourceId,
        user: `User/${userName}`,
        roles: ['Role/consumer'],
      })

    // Switch to user
    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // User should ONLY see dbName2, not dbName1 or dbName3
    const listResult = await userAgent.get('/api/db')
    expect(listResult.status).to.equal(200)

    const databases = listResult.body
    expect(databases).to.be.an('array')
    // Filter to only this organization's databases
    const orgDatabases = databases.filter(db => db.path.startsWith(`${orgName}/`))
    expect(orgDatabases.length).to.equal(1, 'User should see exactly 1 database')
    expect(orgDatabases[0].path).to.equal(`${orgName}/${dbName2}`)

    // Verify user cannot see dbName1
    const paths = orgDatabases.map(db => db.path)
    expect(paths).to.not.include(`${orgName}/${dbName1}`)
    expect(paths).to.not.include(`${orgName}/${dbName3}`)

    // Cleanup
    await adminAgent.delete(`/api/db/${orgName}/${dbName1}`)
    await adminAgent.delete(`/api/db/${orgName}/${dbName2}`)
    await adminAgent.delete(`/api/db/${orgName}/${dbName3}`)
  })

  it('organization-level capability: user can see all databases (regression test)', async function () {
    const adminAgent = new Agent().auth()
    const orgName = util.randomString()
    const userName = util.randomString()
    const dbName1 = util.randomString()
    const dbName2 = util.randomString()

    // Create user
    await adminAgent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    // Create organization
    await adminAgent.post(`/api/organizations/${orgName}`)

    // Create two databases
    await adminAgent.post(`/api/db/${orgName}/${dbName1}`)
      .send({ label: dbName1, comment: '' })
    await adminAgent.post(`/api/db/${orgName}/${dbName2}`)
      .send({ label: dbName2, comment: '' })

    // Grant ORGANIZATION-LEVEL capability using built-in Consumer Role
    await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgName,
        user: userName,
        roles: ['Consumer Role'],
        scope_type: 'organization',
      })

    // Switch to user
    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // User should see BOTH databases
    const listResult = await userAgent.get('/api/db')
    expect(listResult.status).to.equal(200)

    const databases = listResult.body
    expect(databases).to.be.an('array')
    // Filter to only this organization's databases
    const orgDatabases = databases.filter(db => db.path.startsWith(`${orgName}/`))
    expect(orgDatabases.length).to.equal(2, 'User should see both databases')

    const paths = orgDatabases.map(db => db.path).sort()
    expect(paths).to.deep.equal([`${orgName}/${dbName1}`, `${orgName}/${dbName2}`].sort())

    // Cleanup
    await adminAgent.delete(`/api/db/${orgName}/${dbName1}`)
    await adminAgent.delete(`/api/db/${orgName}/${dbName2}`)
  })

  it('mixed capabilities: user sees union of authorized databases', async function () {
    const adminAgent = new Agent().auth()
    const orgName1 = util.randomString()
    const orgName2 = util.randomString()
    const userName = util.randomString()
    const dbName1 = util.randomString()
    const dbName2 = util.randomString()
    const dbName3 = util.randomString()

    // Create user
    await adminAgent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    // Create two organizations
    await adminAgent.post(`/api/organizations/${orgName1}`)
    await adminAgent.post(`/api/organizations/${orgName2}`)

    // Create databases: 2 in org1, 1 in org2
    const db1Create = await adminAgent.post(`/api/db/${orgName1}/${dbName1}`)
      .send({ label: dbName1, comment: '' })
    expect(db1Create.status).to.equal(200)
    const db2Create = await adminAgent.post(`/api/db/${orgName1}/${dbName2}`)
      .send({ label: dbName2, comment: '' })
    expect(db2Create.status).to.equal(200)
    const db3Create = await adminAgent.post(`/api/db/${orgName2}/${dbName3}`)
      .send({ label: dbName3, comment: '' })
    expect(db3Create.status).to.equal(200)

    // Grant org-level access to org1 using built-in Consumer Role
    await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: orgName1,
        user: userName,
        roles: ['Consumer Role'],
        scope_type: 'organization',
      })

    // Get database resource ID from list_databases with verbose flag
    const dbList = await adminAgent.get(`/api/db/${orgName2}?verbose=true`)
    expect(dbList.status).to.equal(200)
    const db3 = dbList.body.find(db => db.path === `${orgName2}/${dbName3}`)
    expect(db3).to.be.an('object')
    const db3ResourceId = db3['@id']
    expect(db3ResourceId).to.be.a('string')
    expect(db3ResourceId).to.match(/^UserDatabase\//)

    // Grant db-level access to specific db in org2 using resource ID
    await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: db3ResourceId,
        user: `User/${userName}`,
        roles: ['Role/consumer'],
      })

    // Switch to user
    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName: orgName1 }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // User should see both databases in org1
    const listResult = await userAgent.get('/api/db')
    expect(listResult.status).to.equal(200)
    const databases = listResult.body

    const databases1 = databases.filter(db => db.path.startsWith(`${orgName1}/`))
    expect(databases1.length).to.equal(2)

    // User should see dbName3 in org2
    const databases2 = databases.filter(db => db.path.startsWith(`${orgName2}/`))
    expect(databases2.length).to.equal(1)
    expect(databases2[0].path).to.equal(`${orgName2}/${dbName3}`)

    // Cleanup
    await adminAgent.delete(`/api/db/${orgName1}/${dbName1}`)
    await adminAgent.delete(`/api/db/${orgName1}/${dbName2}`)
    await adminAgent.delete(`/api/db/${orgName2}/${dbName3}`)
  })

  it('no capability: user sees empty list', async function () {
    const adminAgent = new Agent().auth()
    const orgName = util.randomString()
    const userName = util.randomString()
    const dbName = util.randomString()

    // Create user (with NO capabilities)
    await adminAgent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    // Create organization and database
    await adminAgent.post(`/api/organizations/${orgName}`)
    await adminAgent.post(`/api/db/${orgName}/${dbName}`)
      .send({ label: dbName, comment: '' })

    // Switch to user (who has no capabilities)
    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // User should see empty list
    const listResult = await userAgent.get('/api/db')
    expect(listResult.status).to.equal(200)
    expect(listResult.body).to.be.an('array')
    // Filter to only this organization's databases
    const orgDatabases = listResult.body.filter(db => db.path.startsWith(`${orgName}/`))
    expect(orgDatabases.length).to.equal(0, 'User with no capabilities should see empty list')

    // Cleanup
    await adminAgent.delete(`/api/db/${orgName}/${dbName}`)
  })

  it('multiple database-level capabilities: user sees all authorized databases', async function () {
    const adminAgent = new Agent().auth()
    const orgName = util.randomString()
    const userName = util.randomString()
    const dbName1 = util.randomString()
    const dbName2 = util.randomString()
    const dbName3 = util.randomString()
    const dbName4 = util.randomString()

    // Create user
    await adminAgent
      .post('/api/users')
      .send({
        name: userName,
        password: userName,
      })

    // Create organization
    await adminAgent.post(`/api/organizations/${orgName}`)

    // Create four databases
    const db1Create = await adminAgent.post(`/api/db/${orgName}/${dbName1}`)
      .send({ label: dbName1, comment: '' })
    expect(db1Create.status).to.equal(200)
    const db2Create = await adminAgent.post(`/api/db/${orgName}/${dbName2}`)
      .send({ label: dbName2, comment: '' })
    expect(db2Create.status).to.equal(200)
    const db3Create = await adminAgent.post(`/api/db/${orgName}/${dbName3}`)
      .send({ label: dbName3, comment: '' })
    expect(db3Create.status).to.equal(200)
    const db4Create = await adminAgent.post(`/api/db/${orgName}/${dbName4}`)
      .send({ label: dbName4, comment: '' })
    expect(db4Create.status).to.equal(200)

    // Get database resource IDs from list_databases with verbose flag
    const dbList = await adminAgent.get(`/api/db/${orgName}?verbose=true`)
    expect(dbList.status).to.equal(200)

    const db1 = dbList.body.find(db => db.path === `${orgName}/${dbName1}`)
    expect(db1).to.be.an('object')
    const db1ResourceId = db1['@id']
    expect(db1ResourceId).to.be.a('string')

    const db2 = dbList.body.find(db => db.path === `${orgName}/${dbName2}`)
    expect(db2).to.be.an('object')
    const db2ResourceId = db2['@id']
    expect(db2ResourceId).to.be.a('string')

    const db3 = dbList.body.find(db => db.path === `${orgName}/${dbName3}`)
    expect(db3).to.be.an('object')
    const db3ResourceId = db3['@id']
    expect(db3ResourceId).to.be.a('string')

    // Grant access to dbName1, dbName2, dbName3 (but NOT dbName4) using resource IDs
    await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: db1ResourceId,
        user: `User/${userName}`,
        roles: ['Role/consumer'],
      })

    await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: db2ResourceId,
        user: `User/${userName}`,
        roles: ['Role/consumer'],
      })

    await adminAgent
      .post('/api/capabilities')
      .send({
        operation: 'grant',
        scope: db3ResourceId,
        user: `User/${userName}`,
        roles: ['Role/consumer'],
      })

    // Switch to user
    const userPass = Buffer.from(`${userName}:${userName}`).toString('base64')
    const userAgent = new Agent({ orgName }).auth()
    userAgent.set('Authorization', `Basic ${userPass}`)

    // User should see exactly 3 databases (not 4)
    const listResult = await userAgent.get('/api/db')
    expect(listResult.status).to.equal(200)

    const databases = listResult.body
    expect(databases).to.be.an('array')
    // Filter to only this organization's databases
    const orgDatabases = databases.filter(db => db.path.startsWith(`${orgName}/`))
    expect(orgDatabases.length).to.equal(3, 'User should see exactly 3 databases')

    const paths = orgDatabases.map(db => db.path).sort()
    const expectedPaths = [
      `${orgName}/${dbName1}`,
      `${orgName}/${dbName2}`,
      `${orgName}/${dbName3}`,
    ].sort()
    expect(paths).to.deep.equal(expectedPaths)

    // Verify dbName4 is NOT in the list
    expect(paths).to.not.include(`${orgName}/${dbName4}`)

    // Cleanup
    await adminAgent.delete(`/api/db/${orgName}/${dbName1}`)
    await adminAgent.delete(`/api/db/${orgName}/${dbName2}`)
    await adminAgent.delete(`/api/db/${orgName}/${dbName3}`)
    await adminAgent.delete(`/api/db/${orgName}/${dbName4}`)
  })

  it('admin can list organization databases', async function () {
    const adminAgent = new Agent().auth()
    const orgName = util.randomString()
    const dbName = util.randomString()

    const orgCreate = await adminAgent.post(`/api/organizations/${orgName}`)
    expect(orgCreate.status).to.equal(200)

    const dbCreate = await adminAgent.post(`/api/db/${orgName}/${dbName}`)
      .send({ label: dbName, comment: '' })
    expect(dbCreate.status).to.equal(200)

    const listResponse = await adminAgent.get(`/api/db/${orgName}`)
    expect(listResponse.status).to.equal(200)
    expect(listResponse.body).to.be.an('array')
    expect(listResponse.body.length).to.be.greaterThan(0)

    await adminAgent.delete(`/api/db/${orgName}/${dbName}`)
  })

  it('admin can retrieve database metadata', async function () {
    const adminAgent = new Agent().auth()
    const orgName = util.randomString()
    const dbName = util.randomString()

    const orgCreate = await adminAgent.post(`/api/organizations/${orgName}`)
    expect(orgCreate.status).to.equal(200)

    const dbCreate = await adminAgent.post(`/api/db/${orgName}/${dbName}`)
      .send({ label: dbName, comment: '' })
    expect(dbCreate.status).to.equal(200)

    const dbList = await adminAgent.get(`/api/db/${orgName}`)
    expect(dbList.status).to.equal(200)
    expect(dbList.body).to.be.an('array')
    expect(dbList.body.length).to.be.greaterThan(0)

    await adminAgent.delete(`/api/db/${orgName}/${dbName}`)
  })
})
