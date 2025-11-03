const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-user', function () {
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    dbPath = path.resolve(testDir, 'storage', util.randomString())
    envs = { ...process.env, TERMINUSDB_SERVER_DB_PATH: dbPath }
    {
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  it('add db, change password, list db', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    const r1 = await execEnv(`${util.terminusdbScript()} user get admin -c -j`)
    const Users1 = JSON.parse(r1.stdout)

    await execEnv(`${util.terminusdbScript()} user password admin -pfoo`)
    const r2 = await execEnv(`${util.terminusdbScript()} user get admin -c -j`)
    const Users2 = JSON.parse(r2.stdout)
    expect(Users2[0].capability).to.deep.equal(Users1[0].capability)
  })
})
