const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-user', function () {
  before(async function () {
    this.timeout(200000)
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

  it('add db, change password, list db', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    const r1 = await exec('./terminusdb.sh user get admin -c -j')
    const Users1 = JSON.parse(r1.stdout)

    await exec('./terminusdb.sh user password admin -pfoo')
    const r2 = await exec('./terminusdb.sh user get admin -c -j')
    const Users2 = JSON.parse(r2.stdout)
    expect(Users2[0].capability).to.deep.equal(Users1[0].capability)
  })
})
