const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-triples', function () {
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

  it('load non-existent file', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    const r = await exec(`./terminusdb.sh triples load admin/${db}/local/branch/main/instance ${db} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: File not found: ${db}`))
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })

  it('load trig file', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db} --schema=false`)
    const r = await exec(`./terminusdb.sh triples load admin/${db}/local/branch/main/instance served/MW00KG01635.trig`)
    expect(r.stdout).to.match(new RegExp(`^Successfully inserted triples from \'served/MW00KG01635.trig`))
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })
})
