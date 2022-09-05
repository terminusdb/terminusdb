const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-capabilities', function () {
  before(async function () {
    this.timeout(30000)
    process.env.TERMINUSDB_SERVER_DB_PATH = './storage/' + util.randomString()
    {
      const r = await exec('./terminusdb.sh store init --force')
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(process.env.TERMINUSDB_SERVER_DB_PATH, { recursive: true })
    delete process.env.TERMINUSDB_SERVER_DB_PATH
  })

  it('passes grant and revoke organization', async function () {
    const orgName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    await exec(`./terminusdb.sh user create ${userName} --password=${userName}`)

    // org
    await exec(`./terminusdb.sh organization create ${orgName}`)

    // role
    await exec(`./terminusdb.sh role create ${roleName} meta_read_access meta_write_access instance_read_access instance_write_access schema_read_access schema_write_access create_database delete_database`)

    const result1 = await exec(`./terminusdb.sh capability grant ${userName} ${orgName} ${roleName} --scope_type=organization`)
    expect(result1.stdout).to.match(new RegExp(`^Granted.*${roleName}.*to.*${userName}.*over.*${orgName}.*`))


    const result2 = await exec(`./terminusdb.sh capability revoke ${userName} ${orgName} ${roleName} --scope_type=organization`)
    expect(result2.stdout).to.match(/^Capability successfully revoked/)
  })

  it('passes grant and revoke database', async function () {
    const orgName = util.randomString()
    const dbName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    await exec(`./terminusdb.sh user create ${userName} --password=${userName}`)

    // db
    await exec(`./terminusdb.sh db create admin/${dbName}`)

    // role
    await exec(`./terminusdb.sh role create ${roleName} meta_read_access meta_write_access instance_read_access instance_write_access schema_read_access schema_write_access create_database delete_database`)

    const result1 = await exec(`./terminusdb.sh capability grant ${userName} admin/${dbName} ${roleName}`)
    expect(result1.stdout).to.match(new RegExp(`^Granted.*${roleName}.*to.*${userName}.*over.*admin.${dbName}.*`))

    const result2 = await exec(`./terminusdb.sh capability revoke ${userName} admin/${dbName} ${roleName}`)
    expect(result2.stdout).to.match(/^Capability successfully revoked/)
  })
})
