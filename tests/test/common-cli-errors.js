const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('common-cli-errors', function () {
  before(async function () {
    process.env.TERMINUSDB_SERVER_DB_PATH = './storage/' + util.randomString()
    const r = await exec('./terminusdb.sh store init --force')
    expect(r.stdout).to.match(/^Successfully initialised database/)
  })

  after(async function () {
    await fs.rm(process.env.TERMINUSDB_SERVER_DB_PATH, { recursive: true })
    delete process.env.TERMINUSDB_SERVER_DB_PATH
  })

  describe('fails for bad descriptor path', function () {
    const commands = [
      'fetch --user=admin --password=root',
      'pull --user=admin --password=root',
      'push --user=admin --password=root',
    ]
    for (const command of commands) {
      it(command, async function () {
        const branch = util.randomString()
        await exec(`./terminusdb.sh ${command} ${branch}`)
          .then((r) => {
            expect.fail(JSON.stringify(r))
          })
          .catch((r) => {
            expect(r.code).to.not.equal(0)
            expect(r.stderr).to.match(new RegExp('^Error: Bad descriptor path: ' + branch))
          })
      })
    }
  })

  describe('fails for unknown database', function () {
    const commands = [
      'pull --user=admin --password=root',
      'push --user=admin --password=root',
      'remote list',
      'doc insert --data=""',
      'doc delete --data=""',
      'doc get',
    ]
    for (const command of commands) {
      it(command, async function () {
        const dbSpec = util.randomString() + '/' + util.randomString()
        await exec(`./terminusdb.sh ${command} ${dbSpec}`)
          .then((r) => {
            expect.fail(JSON.stringify(r))
          })
          .catch((r) => {
            expect(r.code).to.not.equal(0)
            expect(r.stderr).to.match(new RegExp('^Error: Unknown database: ' + dbSpec))
          })
      })
    }
  })
})
