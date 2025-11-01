const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-schema-migration', function () {
  let dbSpec
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    const rootDir = path.join(testDir, '..')
    const terminusdbExec = path.join(rootDir, 'terminusdb')

    dbPath = util.testDbPath(testDir)
    envs = {
      ...process.env,
      TERMINUSDB_SERVER_DB_PATH: dbPath,
      // Use existing TERMINUSDB_EXEC_PATH if set (e.g., snap), otherwise default to local binary
      TERMINUSDB_EXEC_PATH: process.env.TERMINUSDB_EXEC_PATH || terminusdbExec,
    }
    {
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  beforeEach(async function () {
    dbSpec = `admin/${util.randomString()}`
    {
      const r = await execEnv(`${util.terminusdbScript()} db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
    }
    const schema = [
      {
        '@type': 'Class',
        '@id': 'String',
        string: 'xsd:string',
      },
      {
        '@type': 'Class',
        '@id': 'Int',
        int: 'xsd:int',
      },
      {
        '@type': 'Class',
        '@id': 'NonNegativeInteger',
        nonNegativeInteger: 'xsd:nonNegativeInteger',
      },
      {
        '@type': 'Class',
        '@id': 'OptionalString',
        optionalString: {
          '@type': 'Optional',
          '@class': 'xsd:string',
        },
      },
    ]
    await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
  })

  afterEach(async function () {
    const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
    expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
  })

  it('target a migration of a branch', async function () {
    const instance = [
      {
        '@id': 'String/1',
        '@type': 'String',
        string: 'asdf',
      },
      {
        '@id': 'String/2',
        '@type': 'String',
        string: 'fdsa',
      },
      {
        '@id': 'Int/1',
        '@type': 'Int',
        int: 23,
      },
    ]
    await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
    await execEnv(`${util.terminusdbScript()} branch create ${dbSpec}/local/branch/foo`)
    const branchInstance = [
      {
        '@id': 'String/3',
        '@type': 'String',
        string: 'new',
      },
      {
        '@id': 'Int/2',
        '@type': 'Int',
        int: 42,
      },
    ]
    await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec}/local/branch/foo --data='${JSON.stringify(branchInstance)}'`)
    const operations = [
      {
        '@type': 'DeleteClassProperty',
        class: 'String',
        property: 'string',
      },
      {
        '@type': 'CreateClassProperty',
        class: 'String',
        property: 'string',
        type: { '@type': 'Optional', '@class': 'String' },
      },
    ]
    const rMigration = await execEnv(`${util.terminusdbScript()} migration ${dbSpec} --operations='${JSON.stringify(operations)}'`)
    const rMigrationResult = JSON.parse(rMigration.stdout)
    expect(rMigrationResult.instance_operations).to.equal(2)
    expect(rMigrationResult.schema_operations).to.equal(2)

    const rMain = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list`)
    const docsMain = JSON.parse(rMain.stdout)
    expect(docsMain).to.deep.equal([
      { '@id': 'Int/1', '@type': 'Int', int: 23 },
      { '@id': 'String/1', '@type': 'String' },
      { '@id': 'String/2', '@type': 'String' },
    ])

    const rBranchMigration = await execEnv(`${util.terminusdbScript()} migration ${dbSpec}/local/branch/foo --target ${dbSpec}`)
    const rBranchMigrationResult = JSON.parse(rBranchMigration.stdout)
    expect(rBranchMigrationResult.instance_operations).to.equal(3)
    expect(rBranchMigrationResult.schema_operations).to.equal(2)

    const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec}/local/branch/foo --as-list`)
    const docs = JSON.parse(r.stdout)
    expect(docs).to.deep.equal([{ '@id': 'Int/1', '@type': 'Int', int: 23 },
      { '@id': 'Int/2', '@type': 'Int', int: 42 },
      { '@id': 'String/1', '@type': 'String' },
      { '@id': 'String/2', '@type': 'String' },
      { '@id': 'String/3', '@type': 'String' }])
  })

  it('target a migration of a branch with weakening', async function () {
    const instance = [
      {
        '@id': 'String/1',
        '@type': 'String',
        string: 'asdf',
      },
      {
        '@id': 'String/2',
        '@type': 'String',
        string: 'fdsa',
      },
      {
        '@id': 'Int/1',
        '@type': 'Int',
        int: 23,
      },
    ]
    await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
    await execEnv(`${util.terminusdbScript()} branch create ${dbSpec}/local/branch/foo`)
    const branchInstance = [
      {
        '@id': 'String/3',
        '@type': 'String',
        string: 'new',
      },
      {
        '@id': 'Int/2',
        '@type': 'Int',
        int: 42,
      },
    ]
    await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec}/local/branch/foo --data='${JSON.stringify(branchInstance)}'`)
    const operations = [
      {
        '@type': 'UpcastClassProperty',
        class: 'String',
        property: 'string',
        type: { '@type': 'Optional', '@class': 'xsd:string' },
      },
    ]
    const rMigration = await execEnv(`${util.terminusdbScript()} migration ${dbSpec} --operations='${JSON.stringify(operations)}'`)
    const rMigrationResult = JSON.parse(rMigration.stdout)
    expect(rMigrationResult.instance_operations).to.equal(2)
    expect(rMigrationResult.schema_operations).to.equal(1)

    const rMain = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list`)
    const docsMain = JSON.parse(rMain.stdout)
    expect(docsMain).to.deep.equal([
      { '@id': 'Int/1', '@type': 'Int', int: 23 },
      { '@id': 'String/1', '@type': 'String', string: 'asdf' },
      { '@id': 'String/2', '@type': 'String', string: 'fdsa' },
    ])

    const rBranchMigration = await execEnv(`${util.terminusdbScript()} migration ${dbSpec}/local/branch/foo --target ${dbSpec}`)
    const rBranchMigrationResult = JSON.parse(rBranchMigration.stdout)
    expect(rBranchMigrationResult.instance_operations).to.equal(3)
    expect(rBranchMigrationResult.schema_operations).to.equal(1)

    const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec}/local/branch/foo --as-list`)
    const docs = JSON.parse(r.stdout)
    expect(docs).to.deep.equal([{ '@id': 'Int/1', '@type': 'Int', int: 23 },
      { '@id': 'Int/2', '@type': 'Int', int: 42 },
      { '@id': 'String/1', '@type': 'String', string: 'asdf' },
      { '@id': 'String/2', '@type': 'String', string: 'fdsa' },
      { '@id': 'String/3', '@type': 'String', string: 'new' }])
  })
})
