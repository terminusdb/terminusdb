const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-branch', function () {
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

  it('does not allow branch "main" to be deleted', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db}`)
    // turn off schema
    const r = await execEnv(`./terminusdb.sh branch delete admin/${db} | true`)
    expect(r.stderr).to.match(/Error: Branch 'main' should not be deleted as it is special/)
    await execEnv(`./terminusdb.sh db delete admin/${db}`, { env: envs })
  })

  it('allows other branch than "main" to be deleted', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db}`)
    await execEnv(`./terminusdb.sh branch create admin/${db}/local/branch/foo`)
    // turn off schema
    const r = await execEnv(`./terminusdb.sh branch delete admin/${db}/local/branch/foo`)
    console.log(r.stdout)
    await execEnv(`./terminusdb.sh db delete admin/${db}`)
  })

  it('is graceful on deleting non-existing branch', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db}`)
    // turn off schema
    const r = await execEnv(`./terminusdb.sh branch delete admin/${db}/local/branch/foo | true`)
    expect(r.stderr).to.match(/Error: Branch foo does not exist/)
    await execEnv(`./terminusdb.sh db delete admin/${db}`)
  })
})
