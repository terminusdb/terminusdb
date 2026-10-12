const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-db', function () {
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  // This is added to test with really funky data that would normally be rejected by the schema check
  // but we want to test that the flag works and that this mode is viable (completely unsupported though)
  // TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'true'
  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    dbPath = util.testDbPath(testDir)
    envs = {
      ...process.env,
      TERMINUSDB_SERVER_DB_PATH: dbPath,
      TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'true',
    }
    {
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  it('lists a db', async function () {
    const r1 = await execEnv(`${util.terminusdbScript()} db list`)
    expect(r1.stdout).to.match(/^TerminusDB\n/)
  })

  it('lists an existing db', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    const r1 = await execEnv(`${util.terminusdbScript()} db list admin/${db}`)
    expect(r1.stdout).to.match(new RegExp(`^TerminusDB\n│\n└── admin/${db}`))
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('lists two dbs', async function () {
    const db1 = util.randomString()
    const db2 = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db1}`)
    await execEnv(`${util.terminusdbScript()} db create admin/${db2}`)
    const r1 = await execEnv(`${util.terminusdbScript()} db list admin/${db1} admin/${db2}`)
    expect(r1.stdout).to.match(/^TerminusDB/)
    await execEnv(`${util.terminusdbScript()} db delete admin/${db1}`)
    await execEnv(`${util.terminusdbScript()} db delete admin/${db2}`)
  })

  it('lists an existing db and branches', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    const r1 = await execEnv(`${util.terminusdbScript()} db list admin/${db} --branches`)
    expect(r1.stdout).to.match(new RegExp(`^TerminusDB\n│\n└── admin/${db}\n    └── main`))
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('updates name of an existing db', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    const r1 = await execEnv(`${util.terminusdbScript()} db update admin/${db} --label goo --comment gah`)
    expect(r1.stdout).to.match(new RegExp(`^Database updated: admin/${db}`))
    const r2 = await execEnv(`${util.terminusdbScript()} db list admin/${db} -v -j`)
    const dbObjs = JSON.parse(r2.stdout)
    expect(dbObjs[0].label).to.equal('goo')
    expect(dbObjs[0].comment).to.equal('gah')
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('puts database in schema free mode and returns it', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    // turn off schema
    await execEnv(`${util.terminusdbScript()} db update admin/${db} --schema false`)
    // demonstrate success
    const r3 = await execEnv(`${util.terminusdbScript()} query admin/${db}/local/branch/main "t('terminusdb://data/Schema', rdf:type, X, schema)"`)
    expect(r3.stdout).to.match(/^X\nrdf:nil/)
    await execEnv(`${util.terminusdbScript()} db update admin/${db} --schema true`)
    // demonstrate success
    const r4 = await execEnv(`${util.terminusdbScript()} query admin/${db}/local/branch/main "t('terminusdb://data/Schema', rdf:type, X, schema)"`)
    expect(r4.stdout).to.match(/^X\n$/)
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('accepts invalid documents when schema is off and rejects them when on', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)

    const schemaFile = path.join(dbPath, `${db}-schema.json`)
    const schema = '{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n{"@id":"Person","@type":"Class","@key":{"@type":"Lexical","@fields":["name"]},"name":"xsd:string","age":"xsd:integer"}\n'
    await fs.writeFile(schemaFile, schema)
    await execEnv(`${util.terminusdbScript()} doc insert admin/${db} --graph-type schema --full-replace < ${schemaFile}`)

    // Valid document is accepted with schema on
    const validFile = path.join(dbPath, `${db}-valid.json`)
    await fs.writeFile(validFile, '{"@type":"Person","name":"Alice","age":30}\n')
    const r1 = await execEnv(`${util.terminusdbScript()} doc insert admin/${db} < ${validFile}`)
    expect(r1.stdout).to.match(/^Documents inserted:\n 1: terminusdb:\/\/\/data\/Person\/Alice/)

    // Invalid document is rejected with schema on
    const invalidFile = path.join(dbPath, `${db}-invalid.json`)
    await fs.writeFile(invalidFile, '{"@type":"Person","name":"Bob","age":"thirty"}\n')
    let threw = false
    try {
      await execEnv(`${util.terminusdbScript()} doc insert admin/${db} < ${invalidFile}`)
    } catch (err) {
      threw = true
      expect(err.stderr).to.match(/Schema check failure/)
    }
    expect(threw).to.equal(true)

    // Turn schema off
    await execEnv(`${util.terminusdbScript()} db update admin/${db} --schema false`)

    // Invalid document is accepted with schema off
    const invalidFile2 = path.join(dbPath, `${db}-invalid2.json`)
    await fs.writeFile(invalidFile2, '{"@type":"Person","name":"Charlie","age":"forty"}\n')
    const r2 = await execEnv(`${util.terminusdbScript()} doc insert admin/${db} < ${invalidFile2}`)
    expect(r2.stdout).to.match(/^Documents inserted:\n 1: terminusdb:\/\/\/data\/Person\/Charlie/)

    // Key generation still works with schema off
    const keygenFile = path.join(dbPath, `${db}-keygen.json`)
    await fs.writeFile(keygenFile, '{"@type":"Person","name":"Dana","age":25}\n')
    const r3 = await execEnv(`${util.terminusdbScript()} doc insert admin/${db} < ${keygenFile}`)
    expect(r3.stdout).to.match(/^Documents inserted:\n 1: terminusdb:\/\/\/data\/Person\/Dana/)

    // Turn schema back on (must not have schema-violating data)
    await execEnv(`${util.terminusdbScript()} db update admin/${db} --schema true`)

    // Invalid document is rejected again
    const invalidFile3 = path.join(dbPath, `${db}-invalid3.json`)
    await fs.writeFile(invalidFile3, '{"@type":"Person","name":"Eve","age":"fifty"}\n')
    threw = false
    try {
      await execEnv(`${util.terminusdbScript()} doc insert admin/${db} < ${invalidFile3}`)
    } catch (err) {
      threw = true
      expect(err.stderr).to.match(/Schema check failure/)
    }
    expect(threw).to.equal(true)

    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('puts database in public and returns it', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    // turn off schema
    await execEnv(`${util.terminusdbScript()} db update admin/${db} --public true`)
    const q = `${util.terminusdbScript()} query _system "
            t(DB_Uri, name, \\"${db}\\"^^xsd:string),
            t(Cap_Id, scope, DB_Uri),
            t(Cap_Id, role, 'Role/consumer'),
            t('User/anonymous', capability, Cap_Id)"`
    // demonstrate success
    const r3 = await execEnv(q)
    expect(r3.stdout).to.match(/^Cap_Id\s+DB_Uri\n.*\n$/)
    await execEnv(`${util.terminusdbScript()} db update admin/${db} --public false`)
    // demonstrate success
    const r4 = await execEnv(q)
    expect(r4.stdout).to.match(/^Cap_Id\s+DB_Uri\n$/)
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('gives a graceful bad path error', async function () {
    const rand = util.randomString()
    const r = await execEnv(`${util.terminusdbScript()} db list ${rand} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: Bad descriptor path: ${rand}`))
  })

  it('gives a graceful non-existence error', async function () {
    const rand = util.randomString()
    const r = await execEnv(`${util.terminusdbScript()} db list admin/${rand} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: Invalid database name: 'admin/${rand}'`))
  })

  it('cannot delete organization with databases', async function () {
    const org = util.randomString()
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} organization create ${org}`)
    await execEnv(`${util.terminusdbScript()} db create ${org}/${db}`)
    const r = await execEnv(`${util.terminusdbScript()} organization delete ${org} | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: The organization ${org}.*`))
  })
})
