const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-reset', function () {
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    this.timeout(200000)
    const testDir = path.join(__dirname, '..')
    dbPath = path.resolve(testDir, 'storage', util.randomString())
    envs = { ...process.env, TERMINUSDB_SERVER_DB_PATH: dbPath }
    {
      const r = await execEnv(`${util.terminusdbScript()} store init --force`)
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  it('resets to previous commit from commit id', async function () {
    const db = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    const schema = JSON.stringify(
      {
        '@type': 'Class',
        '@id': 'Thing',
        '@key': {
          '@type': 'Lexical',
          '@fields': ['name'],
        },
        name: 'xsd:string',
      })
    await execEnv(`${util.terminusdbScript()} doc insert -g schema admin/${db} -d '${schema}'`)
    const doc1 = JSON.stringify(
      { name: 'doc1' })
    await execEnv(`${util.terminusdbScript()} doc insert admin/${db} -d '${doc1}'`)
    const doc2 = JSON.stringify(
      { name: 'doc2' })
    await execEnv(`${util.terminusdbScript()} doc insert -j admin/${db} -d '${doc2}'`)
    const logResult = await execEnv(`${util.terminusdbScript()} log admin/${db} -j`)
    const commits = JSON.parse(logResult.stdout)
    const lastButOne = commits[commits.length - 2]
    const lastButOneCommit = lastButOne.identifier
    // test with commit only
    const result1 = await execEnv(`${util.terminusdbScript()} reset admin/${db} ${lastButOneCommit}`)
    // test with full path
    expect(result1.stdout).to.match(/^Succesfully reset/)
    const last = commits[commits.length - 1]
    const lastCommit = last.identifier
    const result2 = await execEnv(`${util.terminusdbScript()} reset admin/${db} admin/${db}/local/commit/${lastCommit}`)
    expect(result2.stdout).to.match(/^Succesfully reset/)
    await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
  })

  it('it errors gracefully on bad commit descriptors', async function () {
    const db = util.randomString()
    const garbage = util.randomString()
    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    // test with bad commit
    const result = await execEnv(`${util.terminusdbScript()} reset admin/${db} ${garbage}|true`)
    expect(result.stderr).to.match(/Error: Invalid ref path/)
  })

  it('it errors gracefully on bad db descriptors', async function () {
    const db = util.randomString()
    const garbage = util.randomString()
    const schema = JSON.stringify(
      {
        '@type': 'Class',
        '@id': 'Thing',
        '@key': {
          '@type': 'Lexical',
          '@fields': ['name'],
        },
        name: 'xsd:string',
      })

    await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
    await execEnv(`${util.terminusdbScript()} doc insert -g schema admin/${db} -d '${schema}'`)
    const logResult = await execEnv(`${util.terminusdbScript()} log admin/${db} -j`)
    const commits = JSON.parse(logResult.stdout)
    const last = commits[commits.length - 1]
    const lastCommit = last.identifier
    // test with bad commit
    const result = await execEnv(`${util.terminusdbScript()} reset admin/${garbage} ${lastCommit}|true`)
    expect(result.stderr).to.match(/Error: Unable to resolve an invalid absolute path for descriptor/)
  })
})
