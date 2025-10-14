const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-branch', function () {
  let dbPath
  let envs
  let terminusdbSh

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    terminusdbSh = path.join(testDir, 'terminusdb.sh')
    const rootDir = path.join(testDir, '..')
    const terminusdbExec = path.join(rootDir, 'terminusdb')

    dbPath = './storage/' + util.randomString()
    envs = {
      ...process.env,
      TERMINUSDB_SERVER_DB_PATH: dbPath,
      TERMINUSDB_EXEC_PATH: terminusdbExec,
    }
    {
      const r = await execEnv(`${terminusdbSh} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true })
  })

  it('does not allow branch "main" to be deleted', async function () {
    const db = util.randomString()
    await execEnv(`${terminusdbSh} db create admin/${db}`)
    // turn off schema
    const r = await execEnv(`${terminusdbSh} branch delete admin/${db} | true`)
    expect(r.stderr).to.match(/Error: Branch 'main' should not be deleted as it is special/)
    await execEnv(`${terminusdbSh} db delete admin/${db}`, { env: envs })
  })

  it('allows other branch than "main" to be deleted', async function () {
    const db = util.randomString()
    await execEnv(`${terminusdbSh} db create admin/${db}`)
    await execEnv(`${terminusdbSh} branch create admin/${db}/local/branch/foo`)
    await execEnv(`${terminusdbSh} branch delete admin/${db}/local/branch/foo`)
    await execEnv(`${terminusdbSh} db delete admin/${db}`)
  })

  it('is graceful on deleting non-existing branch', async function () {
    const db = util.randomString()
    await execEnv(`${terminusdbSh} db create admin/${db}`)
    // turn off schema
    const r = await execEnv(`${terminusdbSh} branch delete admin/${db}/local/branch/foo | true`)
    expect(r.stderr).to.match(/Error: Branch foo does not exist/)
    await execEnv(`${terminusdbSh} db delete admin/${db}`)
  })
})
