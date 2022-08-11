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
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })

  it('lists two dbs', async function () {
    const db1 = util.randomString()
    const db2 = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db1}`)
    await exec(`./terminusdb.sh db create admin/${db2}`)
    const r1 = await exec(`./terminusdb.sh db list admin/${db1} admin/${db2}`)
    expect(r1.stdout).to.match(/^TerminusDB/)
    await exec(`./terminusdb.sh db delete admin/${db1}`)
    await exec(`./terminusdb.sh db delete admin/${db2}`)
  })

  it('lists an existing db and branches', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    const r1 = await exec(`./terminusdb.sh db list admin/${db} --branches`)
    expect(r1.stdout).to.match(new RegExp(`^TerminusDB\n│\n└── admin/${db}\n    └── main`))
    await exec(`./terminusdb.sh db delete admin/${db}`)
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
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })

  it('puts database in schema free mode and returns it', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    // turn off schema
    await exec(`./terminusdb.sh db update admin/${db} --schema false`)
    // demonstrate success
    const r3 = await exec(`./terminusdb.sh query admin/${db}/local/branch/main "t('terminusdb://data/Schema', rdf:type, X, schema)"`)
    expect(r3.stdout).to.match(/^X\nrdf:nil/)
    await exec(`./terminusdb.sh db update admin/${db} --schema true`)
    // demonstrate success
    const r4 = await exec(`./terminusdb.sh query admin/${db}/local/branch/main "t('terminusdb://data/Schema', rdf:type, X, schema)"`)
    expect(r4.stdout).to.match(/^X\n$/)
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })

  it('puts database a database in public and returns it', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    // turn off schema
    await exec(`./terminusdb.sh db update admin/${db} --public true`)
    const q = `./terminusdb.sh query _system "
            t(DB_Uri, name, \\"${db}\\"^^xsd:string),
            t(Cap_Id, scope, DB_Uri),
            t(Cap_Id, role, 'Role/consumer'),
            t('User/anonymous', capability, Cap_Id)"`
    // demonstrate success
    const r3 = await exec(q)
    expect(r3.stdout).to.match(/^DB_Uri\s+Cap_Id\n.*\n$/)
    await exec(`./terminusdb.sh db update admin/${db} --public false`)
    // demonstrate success
    const r4 = await exec(q)
    expect(r4.stdout).to.match(/^DB_Uri\s+Cap_Id\n$/)
    await exec(`./terminusdb.sh db delete admin/${db}`)
  })

  it('gives a graceful bad path error', async function () {
    const rand = util.randomString()
    const r = await exec(`./terminusdb.sh db list ${rand} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: Bad descriptor path: ${rand}`))
  })

  it('gives a graceful non-existence error', async function () {
    const rand = util.randomString()
    const r = await exec(`./terminusdb.sh db list admin/${rand} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: Invalid database name: 'admin/${rand}'`))
  })
})
