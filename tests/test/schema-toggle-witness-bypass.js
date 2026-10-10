const fs = require('node:fs/promises')
const path = require('node:path')
const exec = require('node:util').promisify(require('node:child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

/**
 * Integration test for the schema-toggle shape-check flag.
 *
 * Documents the acceptance/rejection behaviour of the six witnesses used in the
 * phase 1-5 benchmark. It confirms that the flag
 * TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED only bypasses the inference-layer
 * shape check for two of the witnesses, while the other four are rejected by the
 * JSON layer regardless of the flag.
 */

describe('schema-toggle-witness-bypass', function () {
  let dbPath
  let envs

  async function execEnv (command, envOverrides) {
    return exec(command, { env: { ...envs, ...envOverrides } })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    dbPath = util.testDbPath(testDir)
    envs = {
      ...process.env,
      TERMINUSDB_SERVER_DB_PATH: dbPath,
      TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'true',
    }
    const r = await execEnv(`${util.terminusdbScript()} store init --force`)
    expect(r.stdout).to.match(/^Successfully initialised database/)
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  async function makeDb () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    return db
  }

  async function insertSchema (db, schema) {
    const schemaFile = path.join(dbPath, `${db}-schema.json`)
    const context = '{"@type":"@context","@base":"terminusdb:///data/","@schema":"terminusdb:///schema#"}\n'
    await fs.writeFile(schemaFile, context + schema)
    await execEnv(`${util.terminusdbScript()} doc insert admin/${db} --graph-type schema --full-replace < ${schemaFile}`)
  }

  async function insertDoc (db, doc, expectSuccess, envOverrides = {}) {
    const docFile = path.join(dbPath, `${db}-doc.json`)
    await fs.writeFile(docFile, JSON.stringify(doc) + '\n')
    try {
      const r = await execEnv(`${util.terminusdbScript()} doc insert admin/${db} < ${docFile}`, envOverrides)
      expect(expectSuccess, `expected failure but got success: ${r.stdout}`).to.equal(true)
      return r.stdout
    } catch (err) {
      expect(expectSuccess, `expected success but got failure: ${err.stderr}`).to.equal(false)
      return err.stderr
    }
  }

  async function setSchema (db, schemaOn) {
    await execEnv(`${util.terminusdbScript()} db update admin/${db} --schema ${schemaOn}`)
  }

  it('required_field_does_not_exist_in_document is bypassed when schema is off and shape check disabled', async function () {
    this.timeout(60000)
    const db = await makeDb()
    await insertSchema(db, '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","age":"xsd:integer"}\n')
    const doc = { '@type': 'Person', age: 1 }

    const rejection = await insertDoc(db, doc, false)
    expect(rejection).to.match(/Schema check failure/)

    await setSchema(db, false)
    const rejectionFlagEnabled = await insertDoc(db, doc, false, { TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'false' })
    expect(rejectionFlagEnabled).to.match(/Schema check failure/)

    const acceptance = await insertDoc(db, doc, true)
    expect(acceptance).to.match(/Documents inserted:/)
  })

  it('could_not_interpret_as_type is bypassed when schema is off and shape check disabled', async function () {
    this.timeout(60000)
    const db = await makeDb()
    await insertSchema(db, '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","age":"xsd:integer"}\n')
    const doc = { '@type': 'Person', name: 'Bob', age: 'not-an-integer' }

    const rejection = await insertDoc(db, doc, false)
    expect(rejection).to.match(/Schema check failure/)

    await setSchema(db, false)
    const rejectionFlagEnabled = await insertDoc(db, doc, false, { TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'false' })
    expect(rejectionFlagEnabled).to.match(/Schema check failure/)

    const acceptance = await insertDoc(db, doc, true)
    expect(acceptance).to.match(/Documents inserted:/)
  })

  it('unknown_property_for_type is still rejected when schema is off and shape check disabled', async function () {
    this.timeout(60000)
    const db = await makeDb()
    await insertSchema(db, '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","age":"xsd:integer"}\n')
    const doc = { '@type': 'Person', name: 'Bob', age: 1, nickname: 'Bobby' }

    const rejectionOn = await insertDoc(db, doc, false)
    expect(rejectionOn).to.match(/Schema check failure/)

    await setSchema(db, false)
    const rejectionFlagEnabled = await insertDoc(db, doc, false, { TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'false' })
    expect(rejectionFlagEnabled).to.match(/Schema check failure/)

    const rejectionFlagDisabled = await insertDoc(db, doc, false)
    expect(rejectionFlagDisabled).to.match(/Type error|should be dict/)
  })

  it('instance_not_cardinality_one is still rejected when schema is off and shape check disabled', async function () {
    this.timeout(60000)
    const db = await makeDb()
    await insertSchema(db, '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","age":"xsd:integer"}\n')
    const doc = { '@type': 'Person', name: 'Bob', age: [1, 2] }

    const rejectionOn = await insertDoc(db, doc, false)
    expect(rejectionOn).to.match(/Schema check failure/)

    await setSchema(db, false)
    const rejectionFlagEnabled = await insertDoc(db, doc, false, { TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'false' })
    expect(rejectionFlagEnabled).to.match(/Schema check failure/)

    const rejectionFlagDisabled = await insertDoc(db, doc, false)
    expect(rejectionFlagDisabled).to.match(/Schema check failure/)
  })

  it('enum_violation is still rejected when schema is off and shape check disabled', async function () {
    this.timeout(60000)
    const db = await makeDb()
    await insertSchema(
      db,
      '{"@id":"Color","@type":"Enum","@value":["Red","Green","Blue"]}\n' +
      '{"@id":"Person","@type":"Class","@key":{"@type":"Random"},"name":"xsd:string","favoriteColor":"Color"}\n',
    )
    const doc = { '@type': 'Person', name: 'Bob', favoriteColor: 'Purple' }

    const rejectionOn = await insertDoc(db, doc, false)
    expect(rejectionOn).to.match(/Schema check failure/)

    await setSchema(db, false)
    const rejectionFlagEnabled = await insertDoc(db, doc, false, { TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'false' })
    expect(rejectionFlagEnabled).to.match(/Schema check failure/)

    const rejectionFlagDisabled = await insertDoc(db, doc, false)
    expect(rejectionFlagDisabled).to.match(/Type error|should be dict/)
  })

  it('abstract_class_violation is still rejected when schema is off and shape check disabled', async function () {
    this.timeout(60000)
    const db = await makeDb()
    await insertSchema(
      db,
      '{"@id":"Animal","@type":"Class","@abstract":[]}\n' +
      '{"@id":"Dog","@type":"Class","@inherits":"Animal","@key":{"@type":"Random"},"name":"xsd:string"}\n',
    )
    const doc = { '@type': 'Animal', name: 'Rex' }

    const rejectionOn = await insertDoc(db, doc, false)
    expect(rejectionOn).to.match(/Schema check failure/)

    await setSchema(db, false)
    const rejectionFlagEnabled = await insertDoc(db, doc, false, { TERMINUSDB_SCHEMALESS_SHAPE_CHECK_DISABLED: 'false' })
    expect(rejectionFlagEnabled).to.match(/Schema check failure/)

    const rejectionFlagDisabled = await insertDoc(db, doc, false)
    expect(rejectionFlagDisabled).to.match(/Type error|should be dict/)
  })
})
