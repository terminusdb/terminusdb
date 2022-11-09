const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-branch', function () {
  before(async function () {
    this.timeout(90000)
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

  it('does not allow branch "main" to be deleted', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    // turn off schema
    const r = await exec(`./terminusdb.sh branch delete admin/${db} | true`)
    expect(r.stderr).to.match(/Error: Branch 'main' should not be deleted as it is special/)
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })

  it('allows other branch than "main" to be deleted', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    await exec(`./terminusdb.sh branch create admin/${db}/local/branch/foo`)
    // turn off schema
    const r = await exec(`./terminusdb.sh branch delete admin/${db}/local/branch/foo`)
    console.log(r.stdout)
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })

  it('is graceful on deleting non-existing branch', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    // turn off schema
    const r = await exec(`./terminusdb.sh branch delete admin/${db}/local/branch/foo | true`)
    expect(r.stderr).to.match(/Error: Branch foo does not exist/)
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })
})
