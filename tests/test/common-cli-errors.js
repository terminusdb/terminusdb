const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('common-cli-errors', function () {
  before(async function () {
    process.env.TERMINUSDB_SERVER_DB_PATH = `./storage/${util.randomString()}`
    const r = await exec('./terminusdb.sh store init --force')
    expect(r.stdout).to.match(/^Successfully initialised database/)
  })

  after(async function () {
    await fs.rm(process.env.TERMINUSDB_SERVER_DB_PATH, { recursive: true })
    delete process.env.TERMINUSDB_SERVER_DB_PATH
  })

  describe('fails for unknown database', function () {
    const commands = [
      'pull --user=admin --password=root',
      'push --user=admin --password=root',
      'remote list',
    ]
    for (const command of commands) {
      it(command, async function () {
        const dbSpec = util.randomString() + '/' + util.randomString()
        const r = await exec(`./terminusdb.sh ${command} ${dbSpec}`)
          .catch((result) => {
            expect(result.code).to.not.equal(0)
            expect(result.stdout).to.equal('')
            expect(result.stderr).to.match(new RegExp('^Error: Unknown database: ' + dbSpec))
            return true
          })
        expect(r).to.be.true
      })
    }
  })
})
