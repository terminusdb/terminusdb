const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-diff', function () {
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    dbPath = util.testDbPath(testDir)
    envs = { ...process.env, TERMINUSDB_SERVER_DB_PATH: dbPath }
    {
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  it('compares two branches with added raw JSON document using CLI', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    await execEnv(`${util.terminusdbScript()} branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)
    await execEnv(`${util.terminusdbScript()} doc insert -j admin/${db}/local/branch/test -d '{ "name": "test" }'`)
    const r1 = await execEnv(`${util.terminusdbScript()} diff admin/${db} --before-commit=main --after-commit=test`)
    const parsedJson = JSON.parse(r1.stdout)
    expect(parsedJson).to.have.lengthOf(1)
    expect(parsedJson[0]['@op']).to.equal('Insert')
    expect(parsedJson[0]['@insert'].name).to.equal('test')
  })

  it('compares two commits with added raw JSON document using CLI', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    await execEnv(`${util.terminusdbScript()} branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)
    await execEnv(`${util.terminusdbScript()} doc insert -j admin/${db}/local/branch/test -d '{ "name": "test" }'`)
    await execEnv(`${util.terminusdbScript()} doc insert -j admin/${db}/local/branch/test -d '{ "name": "test2" }'`)
    const log = await execEnv(`${util.terminusdbScript()} log admin/${db}/local/branch/test --json`)
    const logJson = JSON.parse(log.stdout)
    const commitBefore = logJson[1].identifier
    const commitAfter = logJson[0].identifier
    const diffCommand = await execEnv(`${util.terminusdbScript()} diff admin/${db}/local/branch/test --before-commit=${commitBefore} --after-commit=${commitAfter}`)
    const parsedJson = JSON.parse(diffCommand.stdout)
    expect(parsedJson).to.have.lengthOf(1)
    expect(parsedJson[0]['@op']).to.equal('Insert')
    expect(parsedJson[0]['@insert'].name).to.equal('test2')
  })

  it('compares two branches with removing a raw JSON document using CLI', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    const r1 = await execEnv(`${util.terminusdbScript()} doc insert -j admin/${db}/local/branch/main -d '{ "name": "test" }'`)
    const documentId = r1.stdout.split('1: ')[1].trim()
    await execEnv(`${util.terminusdbScript()} branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)

    // Verify document exists before deletion
    const beforeDelete = await execEnv(`${util.terminusdbScript()} doc get admin/${db}/local/branch/test --id=${documentId}`)
    expect(beforeDelete.stdout).to.include('"name"')
    expect(beforeDelete.stdout).to.include('"test"')

    await execEnv(`${util.terminusdbScript()} doc delete admin/${db}/local/branch/test --id=${documentId}`)

    // Verify document no longer exists after deletion
    const afterDelete = await execEnv(`${util.terminusdbScript()} doc get admin/${db}/local/branch/test --id=${documentId} || true`)
    expect(afterDelete.stderr || afterDelete.stdout, 'Document should not exist after deletion').to.match(/not found|does not exist/i)

    const r2 = await execEnv(`${util.terminusdbScript()} diff admin/${db} --before-commit=main --after-commit=test`)
    const parsedJson = JSON.parse(r2.stdout)
    expect(parsedJson).to.have.lengthOf(1)
    expect(parsedJson[0]['@op']).to.equal('Delete')
    expect(parsedJson[0]['@delete'].name).to.equal('test')
  })

  it('compares two branches with no differences using CLI', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    await execEnv(`${util.terminusdbScript()} branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)
    const r1 = await execEnv(`${util.terminusdbScript()} diff admin/${db} --before-commit=main --after-commit=test`)
    const parsedJson = JSON.parse(r1.stdout)
    expect(parsedJson).to.have.lengthOf(0)
  })

  it('gets an error on apply', async function () {
    const db = util.randomString()
    const r1 = await execEnv(`${util.terminusdbScript()} apply admin/${db} | true`)
    expect(r1.stderr).to.match(new RegExp(`^Error: Unknown database: admin/${db}.*`))
  })
})
