const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-db', function () {
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

  it('lists a db', async function () {
    const r1 = await exec('./terminusdb.sh db list')
    expect(r1.stdout).to.match(/^TerminusDB\n/)
  })

  it('lists an existing db', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    const r1 = await exec(`./terminusdb.sh db list admin/${db}`)
    expect(r1.stdout).to.match(new RegExp(`^TerminusDB\n│\n└── admin/${db}`))
  })

  it('lists two dbs', async function () {
    const db1 = util.randomString()
    const db2 = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db1}`)
    await exec(`./terminusdb.sh db create admin/${db2}`)
    const r1 = await exec(`./terminusdb.sh db list admin/${db1} admin/${db2}`)
    expect(r1.stdout).to.match(/^TerminusDB/)
  })

  it('lists an existing db and branches', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    const r1 = await exec(`./terminusdb.sh db list admin/${db} --branches`)
    expect(r1.stdout).to.match(new RegExp(`^TerminusDB\n│\n└── admin/${db}\n    └── main`))
  })

  it('updates name of an existing db', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    const r1 = await exec(`./terminusdb.sh db update admin/${db} --label goo --comment gah`)
    expect(r1.stdout).to.match(new RegExp(`^Database updated: admin/${db}`))
    const r2 = await exec(`./terminusdb.sh db list admin/${db} -v -j`)
    const dbObjs = JSON.parse(r2.stdout)
    expect(dbObjs[0].label).to.equal('goo')
    expect(dbObjs[0].comment).to.equal('gah')
  })
})
