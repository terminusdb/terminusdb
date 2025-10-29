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
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  it('passes grant and revoke organization', async function () {
    const orgName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    await execEnv(`${util.terminusdbScript()} user create ${userName} --password=${userName}`)

    // org
    await execEnv(`${util.terminusdbScript()} organization create ${orgName}`)

    // role
    await execEnv(`${util.terminusdbScript()} role create ${roleName} meta_read_access meta_write_access instance_read_access instance_write_access schema_read_access schema_write_access create_database delete_database`)

    const result1 = await execEnv(`${util.terminusdbScript()} capability grant ${userName} ${orgName} ${roleName} --scope_type=organization`)
    expect(result1.stdout).to.match(new RegExp(`^Granted.*${roleName}.*to.*${userName}.*over.*${orgName}.*`))

    const result2 = await execEnv(`${util.terminusdbScript()} capability revoke ${userName} ${orgName} ${roleName} --scope_type=organization`)
    expect(result2.stdout).to.match(/^Capability successfully revoked/)
  })

  it('passes grant and revoke database', async function () {
    const dbName = util.randomString()
    const userName = util.randomString()
    const roleName = util.randomString()

    // user
    await execEnv(`${util.terminusdbScript()} user create ${userName} --password=${userName}`)

    // db
    await execEnv(`${util.terminusdbScript()} db create admin/${dbName}`)

    // role
    await execEnv(`${util.terminusdbScript()} role create ${roleName} meta_read_access meta_write_access instance_read_access instance_write_access schema_read_access schema_write_access create_database delete_database`)

    const result1 = await execEnv(`${util.terminusdbScript()} capability grant ${userName} admin/${dbName} ${roleName}`)
    expect(result1.stdout).to.match(new RegExp(`^Granted.*${roleName}.*to.*${userName}.*over.*admin.${dbName}.*`))

    const result2 = await execEnv(`${util.terminusdbScript()} doc get admin/${dbName} --impersonate ${userName} && echo success`)
    expect(result2.stdout).to.match(/^success/)

    const result3 = await execEnv(`${util.terminusdbScript()} capability revoke ${userName} admin/${dbName} ${roleName}`)
    expect(result3.stdout).to.match(/^Capability successfully revoked/)

    const result4 = await execEnv(`${util.terminusdbScript()} doc get admin/${dbName} --impersonate ${userName} || echo failure`)
    expect(result4.stdout).to.match(/^failure/)
  })
})
