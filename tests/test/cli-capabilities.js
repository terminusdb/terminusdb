const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-capabilities', function () {
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }


  before(async function () {
    this.timeout(200000)
    dbPath = './storage/' + util.randomString()
    envs = { ...process.env, TERMINUSDB_SERVER_DB_PATH: dbPath }
    {
      const r = await execEnv('./terminusdb.sh store init --force')
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true })
  })

  it('passes grant and revoke organization', async function () {
    const orgName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    await execEnv(`./terminusdb.sh user create ${userName} --password=${userName}`)

    // org
    await execEnv(`./terminusdb.sh organization create ${orgName}`)

    // role
    await execEnv(`./terminusdb.sh role create ${roleName} meta_read_access meta_write_access instance_read_access instance_write_access schema_read_access schema_write_access create_database delete_database`)

    const result1 = await execEnv(`./terminusdb.sh capability grant ${userName} ${orgName} ${roleName} --scope_type=organization`)
    expect(result1.stdout).to.match(new RegExp(`^Granted.*${roleName}.*to.*${userName}.*over.*${orgName}.*`))

    const result2 = await execEnv(`./terminusdb.sh capability revoke ${userName} ${orgName} ${roleName} --scope_type=organization`)
    expect(result2.stdout).to.match(/^Capability successfully revoked/)
  })

  it('passes grant and revoke database', async function () {
    const dbName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    await execEnv(`./terminusdb.sh user create ${userName} --password=${userName}`)

    // db
    await execEnv(`./terminusdb.sh db create admin/${dbName}`)

    // role
    await execEnv(`./terminusdb.sh role create ${roleName} meta_read_access meta_write_access instance_read_access instance_write_access schema_read_access schema_write_access create_database delete_database`)

    const result1 = await execEnv(`./terminusdb.sh capability grant ${userName} admin/${dbName} ${roleName}`)
    expect(result1.stdout).to.match(new RegExp(`^Granted.*${roleName}.*to.*${userName}.*over.*admin.${dbName}.*`))

    const result2 = await execEnv(`./terminusdb.sh capability revoke ${userName} admin/${dbName} ${roleName}`)
    expect(result2.stdout).to.match(/^Capability successfully revoked/)
  })
})
