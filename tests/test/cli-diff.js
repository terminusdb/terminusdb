const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-diff', function () {
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

  it('compares two branches with added raw JSON document using CLI', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    await exec(`./terminusdb.sh branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)
    await exec(`./terminusdb.sh doc insert -j admin/${db}/local/branch/test -d '{ "name": "test" }'`)
    const r1 = await exec(`./terminusdb.sh diff admin/${db} --before-commit=main --after-commit=test`)
    const parsedJson = JSON.parse(r1.stdout)
    expect(parsedJson).to.have.lengthOf(1)
    expect(parsedJson[0]['@op']).to.equal('Insert')
    expect(parsedJson[0]['@insert'].name).to.equal('test')
  })

  it('compares two commits with added raw JSON document using CLI', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    await exec(`./terminusdb.sh branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)
    await exec(`./terminusdb.sh doc insert -j admin/${db}/local/branch/test -d '{ "name": "test" }'`)
    await exec(`./terminusdb.sh doc insert -j admin/${db}/local/branch/test -d '{ "name": "test2" }'`)
    const log = await exec(`./terminusdb.sh log admin/${db}/local/branch/test --json`)
    const logJson = JSON.parse(log.stdout)
    const commitBefore = logJson[1].identifier
    const commitAfter = logJson[0].identifier
    const diffCommand = await exec(`./terminusdb.sh diff admin/${db}/local/branch/test --before-commit=${commitBefore} --after-commit=${commitAfter}`)
    const parsedJson = JSON.parse(diffCommand.stdout)
    expect(parsedJson).to.have.lengthOf(1)
    expect(parsedJson[0]['@op']).to.equal('Insert')
    expect(parsedJson[0]['@insert'].name).to.equal('test2')
  })

  it('compares two branches with removing a raw JSON document using CLI', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    const r1 = await exec(`./terminusdb.sh doc insert -j admin/${db}/local/branch/main -d '{ "name": "test" }'`)
    const documentId = r1.stdout.split('1: ')[1]
    await exec(`./terminusdb.sh branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)
    await exec(`./terminusdb.sh doc delete admin/${db}/local/branch/test --id=${documentId}`)
    const r2 = await exec(`./terminusdb.sh diff admin/${db} --before-commit=main --after-commit=test`)
    const parsedJson = JSON.parse(r2.stdout)
    expect(parsedJson).to.have.lengthOf(1)
    expect(parsedJson[0]['@op']).to.equal('Delete')
    expect(parsedJson[0]['@delete'].name).to.equal('test')
  })

  it('compares two branches with no differences using CLI', async function () {
    const db = util.randomString()
    await exec(`./terminusdb.sh db create admin/${db}`)
    await exec(`./terminusdb.sh branch create admin/${db}/local/branch/test --origin=admin/${db}/local/branch/main`)
    const r1 = await exec(`./terminusdb.sh diff admin/${db} --before-commit=main --after-commit=test`)
    const parsedJson = JSON.parse(r1.stdout)
    expect(parsedJson).to.have.lengthOf(0)
  })

  it('gets an error on apply', async function () {
    const db = util.randomString()
    const r1 = await exec(`./terminusdb.sh apply admin/${db} | true`)
    expect(r1.stderr).to.match(new RegExp(`^Error: Unknown database: admin/${db}.*`))
  })
})
