const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-query', function () {
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

  it('queries with a list response', async function () {
    const r = await exec('./terminusdb.sh query _system \'split("A,B,C"^^xsd:string,","^^xsd:string,R)\'')
    expect(r.stdout).to.match(/["A"^^'xsd:string',"B"^^'xsd:string',"C"^^'xsd:string']/)
  })

  it('errors on non-existent branch', async function () {
    const db = util.randomString()
    const r = await exec(`./terminusdb.sh query admin/${db} 'sum(1,2)' | true`)
    expect(r.stderr).to.match(new RegExp(`^Error: The following descriptor could not be resolved to a resource: 'admin/${db}/local/branch/main'`))
  })

  it('catches existence error', async function () {
    const r = await exec('./terminusdb.sh query _system \'sum(1,2)\' | true')
    expect(r.stderr).to.match(/^Error: The program: .* used a predicate with unhandled arguments/)
  })
})
