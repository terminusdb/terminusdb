const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-triples', function () {
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    const rootDir = path.join(testDir, '..')
    const terminusdbExec = path.join(rootDir, 'terminusdb')

    dbPath = util.testDbPath(testDir)
    envs = {
      ...process.env,
      TERMINUSDB_SERVER_DB_PATH: dbPath,
      // Use existing TERMINUSDB_EXEC_PATH if set (e.g., snap), otherwise default to local binary
      TERMINUSDB_EXEC_PATH: process.env.TERMINUSDB_EXEC_PATH || terminusdbExec,
    }
    {
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  it('load non-existent file', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    const r = await execEnv(`${util.terminusdbScript()} triples load admin/${db}/local/branch/main/instance ${db} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: File not found: ${db}`))
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('load trig file', async function () {
    // Use relative path for Docker compatibility (working directory is /app/terminusdb/tests inside container)
    const trigFile = 'served/MW00KG01635.trig'
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db} --schema=false`)
    const r = await execEnv(`${util.terminusdbScript()} triples load admin/${db}/local/branch/main/instance ${trigFile}`)
    const escapedPath = trigFile.replace(/\\/g, '\\\\')
    expect(r.stdout).to.match(new RegExp(`Successfully inserted triples from '${escapedPath}'`))
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })
})
