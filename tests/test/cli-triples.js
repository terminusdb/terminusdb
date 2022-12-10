const fs = require('fs/promises')
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

  it('load non-existent file', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db}`)
    const r = await execEnv(`./terminusdb.sh triples load admin/${db}/local/branch/main/instance ${db} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: File not found: ${db}`))
    await execEnv(`./terminusdb.sh db delete admin/${db}`)
  })

  it('load trig file', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db} --schema=false`)
    const r = await execEnv(`./terminusdb.sh triples load admin/${db}/local/branch/main/instance served/MW00KG01635.trig`)
    expect(r.stdout).to.match(/Successfully inserted triples from 'served\/MW00KG01635.trig'/)
    await execEnv(`./terminusdb.sh db delete admin/${db}`)
  })
})
