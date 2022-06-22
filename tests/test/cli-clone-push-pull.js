const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const process = require('process')
const { expect } = require('chai')
const { Agent, db, document, util } = require('../lib')

describe('cli-clone-push-pull', function () {
  before(async function () {
    process.env.TERMINUSDB_SERVER_DB_PATH = `./storage/${util.randomString()}`
    const r = await exec('./terminusdb.sh store init --force')
    expect(r.stdout).to.match(/^Successfully initialised database/)
  })

  after(async function () {
    await fs.rm(process.env.TERMINUSDB_SERVER_DB_PATH, { recursive: true })
    delete process.env.TERMINUSDB_SERVER_DB_PATH
  })

  it('fails clone with socket error', async function () {
    const cmd = `./terminusdb.sh clone --user=${util.randomString()} --password=${util.randomString()}`
    const urls = [
      // Connection refused
      'http://localhost:65535/' + util.randomString(),
      // Name or service not known
      'http://' + util.randomString() + ':65535/' + util.randomString(),
    ]
    // We run this at least twice to make sure that the error is the same.
    // Previously, the database would be created during the first run, and the
    // second run had a different error.
    for (const url of urls) {
      await exec(cmd + ' ' + url)
        .then((r) => {
          expect.fail(JSON.stringify(r))
        })
        .catch((r) => {
          expect(r.code).to.not.equal(0)
          expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
          expect(r.stderr).to.match(/^Error: HTTP request failed with socket error/)
        })
    }
  })

  it('fails push to unknown remote database', async function () {
    this.timeout(20000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    const url = agent.baseUrl + '/' + dbSpec
    // Create the remote database.
    await db.create(agent)
    { // Clone to a local database.
      const r = await exec(`./terminusdb.sh clone --user=${agent.user} --password=${agent.password} ${url}`)
      expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
      expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
    }
    // Delete the remote database.
    await db.delete(agent)
    // Attempt to push to the remote database.
    await exec(`./terminusdb.sh push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      .then((r) => {
        expect.fail(JSON.stringify(r))
      })
      .catch((r) => {
        expect(r.code).to.not.equal(0)
        expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
        expect(r.stderr).to.match(new RegExp(`^Error: Remote connection failed: Unknown database: ${dbSpec}`))
      })
    { // Delete the local database.
      const r = await exec(`./terminusdb.sh db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    }
  })

  it('fails push if remote history has diverged', async function () {
    this.timeout(20000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    const url = agent.baseUrl + '/' + dbSpec
    // Create the remote database.
    await db.create(agent)
    { // Clone to a local database.
      const r = await exec(`./terminusdb.sh clone --user=${agent.user} --password=${agent.password} ${url}`)
      expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
      expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
    }
    // Insert doc in remote database.
    const schema = { '@type': 'Class', '@id': util.randomString() }
    await document.insert(agent, { schema })
    // Attempt to push to the remote database.
    await exec(`./terminusdb.sh push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      .then((r) => {
        expect.fail(JSON.stringify(r))
      })
      .catch((r) => {
        expect(r.code).to.not.equal(0)
        expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
        expect(r.stderr).to.match(/^Error: Remote connection failed: Remote history has diverged/)
      })
    { // Delete the local database.
      const r = await exec(`./terminusdb.sh db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    }
    // Delete the remote database.
    await db.delete(agent)
  })

  it('passes push twice', async function () {
    this.timeout(20000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    const url = agent.baseUrl + '/' + dbSpec
    // Create the remote database.
    await db.create(agent)
    { // Clone to a local database.
      const r = await exec(`./terminusdb.sh clone --user=${agent.user} --password=${agent.password} ${url}`)
      expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
      expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
    }
    { // Push once.
      const r = await exec(`./terminusdb.sh push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
      expect(r.stdout).to.match(/Remote updated \(head is .*\)/)
    }
    { // Push again.
      const r = await exec(`./terminusdb.sh push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
      expect(r.stdout).to.match(/Remote already up to date \(head is .*\)/)
    }
    { // Delete the local database.
      const r = await exec(`./terminusdb.sh db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    }
    // Delete the remote database.
    await db.delete(agent)
  })

  describe('empty remote database', function () {
    let agent
    let dbSpec
    let url

    before(async function () {
      agent = new Agent().auth()
      await db.create(agent)
      dbSpec = agent.orgName + '/' + agent.dbName
      url = agent.baseUrl + '/' + dbSpec
    })

    after(async function () {
      await db.delete(agent)
    })

    it('fails clone with auth error (#1127)', async function () {
      await exec(`./terminusdb.sh clone --user=${util.randomString()} --password=${util.randomString()} ${url}`)
        .then((r) => {
          expect.fail(JSON.stringify(r))
        })
        .catch((r) => {
          expect(r.code).to.not.equal(0)
          expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
          expect(r.stderr).to.match(/^Error: Remote authentication failed: Incorrect authentication information/)
        })
    })

    describe('clone', function () {
      before(async function () {
        const r = await exec(`./terminusdb.sh clone --user=${agent.user} --password=${agent.password} ${url}`)
        expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
        expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
      })

      after(async function () {
        const r = await exec(`./terminusdb.sh db delete ${dbSpec}`)
        expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
      })

      it('fails push with auth error', async function () {
        await exec(`./terminusdb.sh push --user=${util.randomString()} --password=${util.randomString()} ${dbSpec}`)
          .then((r) => {
            expect.fail(JSON.stringify(r))
          })
          .catch((r) => {
            expect(r.code).to.not.equal(0)
            expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
            expect(r.stderr).to.match(new RegExp(`^Error: HTTP request authentication failed for URL: ${agent.baseUrl}/api/files`))
          })
      })

      it('passes pull twice', async function () {
        const branchSpec = `${dbSpec}/local/branch/main`
        const cmd = `./terminusdb.sh pull --user=${agent.user} --password=${agent.password} ${branchSpec}`
        {
          const r = await exec(cmd)
          expect(r.stdout).to.match(/^Pulling from remote 'origin'/)
          expect(r.stdout).to.match(/Remote up to date/)
          expect(r.stdout).to.match(/Local branch up to date/)
        }
        {
          const r = await exec(cmd)
          expect(r.stdout).to.match(/^Pulling from remote 'origin'/)
          expect(r.stdout).to.match(/Remote up to date/)
          expect(r.stdout).to.match(/Local branch up to date/)
        }
      })

      describe('fails with database already exists (#1127)', function () {
        for (let cmd of ['db create', 'clone']) {
          it(cmd, async function () {
            let expectedStdout
            switch (cmd) {
              case 'db create':
                cmd = `./terminusdb.sh db create ${dbSpec}`
                expectedStdout = ''
                break
              case 'clone':
                cmd = `./terminusdb.sh clone --user=${agent.user} --password=${agent.password} ${url}`
                expectedStdout = 'Cloning the remote \'origin\'\n'
                break
              default:
                throw new Error(`Unknown cmd: ${cmd}`)
            }
            await exec(cmd)
              .then((r) => {
                expect.fail(JSON.stringify(r))
              })
              .catch((r) => {
                expect(r.code).to.not.equal(0)
                expect(r.stdout).to.equal(expectedStdout)
                expect(r.stderr).to.match(/^Error: Database already exists\./)
              })
          })
        }
      })
    })
  })

  describe('empty local database', function () {
    let agent
    let dbSpec

    before(async function () {
      agent = new Agent().auth()
      dbSpec = agent.orgName + '/' + agent.dbName
      const r = await exec(`./terminusdb.sh db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
    })

    after(async function () {
      const r = await exec(`./terminusdb.sh db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    })

    it('fails push with unknown remote repository', async function () {
      await exec('./terminusdb.sh push --user=' + agent.user + ' --password=' + agent.password + ' ' + dbSpec)
        .then((r) => {
          expect.fail(JSON.stringify(r))
        })
        .catch((r) => {
          expect(r.code).to.not.equal(0)
          expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
          expect(r.stderr).to.match(/^Error: Unknown remote repository: origin/)
        })
    })

    it('fails pull with unknown local branch', async function () {
      const branchSpec = `${dbSpec}/remote/branch/${util.randomString()}`
      await exec(`./terminusdb.sh pull --user=${agent.user} --password=${agent.password} ${branchSpec}`)
        .then((r) => {
          expect.fail(JSON.stringify(r))
        })
        .catch((r) => {
          expect(r.code).to.not.equal(0)
          expect(r.stdout).to.match(/^Pulling from remote 'origin'/)
          expect(r.stderr).to.match(new RegExp(`^Error: Unknown local branch: ${branchSpec}`))
        })
    })
  })
})
