const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-options', function () {
  let dbSpec
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    dbPath = util.testDbPath(testDir)
    envs = { ...process.env, TERMINUSDB_SERVER_DB_PATH: dbPath }
    {
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
    dbSpec = `admin/${util.randomString()}`
    {
      const r = await execEnv(`${util.terminusdbScript()} db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
    }
  })

  after(async function () {
    const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
    expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  describe('command line options report correct errors', function () {
    // This may depend on prolog version!
    it('nice error for bad switch', async function () {
      const r = await execEnv(`${util.terminusdbScript()} db create admin/bar -g foo`)
      const errorPattern = /(ERROR: The command line option "g" does not exist for the command "db create")|(ERROR: Unknown command line option).*/
      expect(r.stderr).to.match(new RegExp(errorPattern))
    })

    it('nice error for bad type', async function () {
      const r = await execEnv(`${util.terminusdbScript()} db create admin/bar --public=foo`)
      const errorPattern = /ERROR: flag 'public': expected atom parsable as boolean, found 'foo'.*/
      expect(r.stdout).to.match(new RegExp(errorPattern))
    })

    it('nice error for bad shortflag option', async function () {
      const r = await execEnv(`${util.terminusdbScript()} db list admin/bar -b=true`)
      const errorPattern = /ERROR: The command line does not accept -<shortflag>=<value> syntax in the command "db list".*/
      expect(r.stderr).to.match(new RegExp(errorPattern))
    })
  })
})
