const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const process = require('process')
const { expect } = require('chai')
const { Agent, db, document, util } = require('../lib')

describe('cli-clone-push-pull', function () {
  let dbPath
  let envs

  async function execEnv (command) {
    return exec(command, { env: envs })
  }

  before(async function () {
    const testDir = path.join(__dirname, '..')
    dbPath = path.resolve(testDir, 'storage', util.randomString())
    envs = { ...process.env, TERMINUSDB_SERVER_DB_PATH: dbPath }
    const r = await execEnv(`${util.terminusdbScript()} store init --force`)
    expect(r.stdout).to.match(/^Successfully initialised database/)
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  it('fails clone with socket error', async function () {
    const cmd = `${util.terminusdbScript()} clone --user=${util.randomString()} --password=${util.randomString()}`
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
      await execEnv(cmd + ' ' + url)
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
    this.timeout(200000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    const url = agent.baseUrl + '/' + dbSpec
    // Create the remote database.
    await db.create(agent)
    { // Clone to a local database.
      const r = await execEnv(`${util.terminusdbScript()} clone --user=${agent.user} --password=${agent.password} ${url}`)
      expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
      expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
    }
    // Delete the remote database.
    await db.delete(agent)
    // Attempt to push to the remote database.
    await execEnv(`${util.terminusdbScript()} push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      .then((r) => {
        expect.fail(JSON.stringify(r))
      })
      .catch((r) => {
        expect(r.code).to.not.equal(0)
        expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
        expect(r.stderr).to.match(new RegExp(`^Error: Remote connection failed: Unknown database: ${dbSpec}`))
      })
    { // Delete the local database.
      const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    }
  })

  it('fails push if remote history has diverged', async function () {
    this.timeout(200000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    const url = agent.baseUrl + '/' + dbSpec
    // Create the remote database.
    await db.create(agent)
    { // Clone to a local database.
      const r = await execEnv(`${util.terminusdbScript()} clone --user=${agent.user} --password=${agent.password} ${url}`)
      expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
      expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
    }
    // Insert doc in remote database.
    const schema = { '@type': 'Class', '@id': util.randomString() }
    await document.insert(agent, { schema })
    // Attempt to push to the remote database.
    await execEnv(`${util.terminusdbScript()} push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      .then((r) => {
        expect.fail(JSON.stringify(r))
      })
      .catch((r) => {
        expect(r.code).to.not.equal(0)
        expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
        expect(r.stderr).to.match(/^Error: Remote connection failed: Remote history has diverged/)
      })
    { // Delete the local database.
      const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    }
    // Delete the remote database.
    await db.delete(agent)
  })

  it('returns semantic error response when remote history diverged (error structure)', async function () {
    this.timeout(200000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    const url = agent.baseUrl + '/' + dbSpec

    // Create the remote database
    await db.create(agent)

    // Clone to a local database (CLI creates it in CLI storage)
    const cloneResult = await execEnv(`${util.terminusdbScript()} clone --user=${agent.user} --password=${agent.password} ${url}`)
    expect(cloneResult.stdout).to.match(/^Cloning the remote 'origin'/)

    // Insert doc in remote database to create divergence
    const remoteSchema = { '@type': 'Class', '@id': 'RemoteClass_' + util.randomString() }
    await document.insert(agent, { schema: remoteSchema })

    // Now attempt to push via CLI - this should fail with remote_diverged error
    // The CLI will output the error which we verify contains the right message
    await execEnv(`${util.terminusdbScript()} push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      .then((r) => {
        expect.fail(JSON.stringify(r))
      })
      .catch((r) => {
        expect(r.code).to.not.equal(0)
        expect(r.stdout).to.match(/^Pushing to remote 'origin'/)

        // Verify the error message is semantic and helpful (not a stack trace)
        expect(r.stderr).to.match(/diverged/)
        expect(r.stderr).to.not.include('prolog_stack')
        expect(r.stderr).to.not.include('context(')

        // The error should mention it's a remote connection issue
        expect(r.stderr).to.match(/Remote/)
      })

    // Delete the local database
    await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)

    // Delete the remote database
    await db.delete(agent)
  })

  it('catches push has no head', async function () {
    this.timeout(200000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    // Create the db
    await execEnv(`${util.terminusdbScript()} db create ${dbSpec}`)
    // Add remote
    await execEnv(`${util.terminusdbScript()} remote add ${dbSpec} testremote example.org`)
    await execEnv(`${util.terminusdbScript()} push --user=${agent.user} --password=${agent.password} ${dbSpec} --remote testremote`)
      .catch((r) => {
        expect(r.stderr).to.match(/^Error: The following repository has no head:/)
      })
  })

  it('passes push twice', async function () {
    this.timeout(200000)
    const agent = new Agent().auth()
    const dbSpec = agent.orgName + '/' + agent.dbName
    const url = agent.baseUrl + '/' + dbSpec
    // Create the remote database.
    await db.create(agent)
    { // Clone to a local database.
      const r = await execEnv(`${util.terminusdbScript()} clone --user=${agent.user} --password=${agent.password} ${url}`)
      expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
      expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
    }
    { // Push once.
      const r = await execEnv(`${util.terminusdbScript()} push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
      expect(r.stdout).to.match(/Remote updated \(head is .*\)/)
    }
    { // Push again.
      const r = await execEnv(`${util.terminusdbScript()} push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
      expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
      expect(r.stdout).to.match(/Remote already up to date \(head is .*\)/)
    }
    { // Delete the local database.
      const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
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
      await execEnv(`${util.terminusdbScript()} clone --user=${util.randomString()} --password=${util.randomString()} ${url}`)
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
        const r = await execEnv(`${util.terminusdbScript()} clone --user=${agent.user} --password=${agent.password} ${url}`)
        expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
        expect(r.stdout).to.match(new RegExp(`Database created: ${dbSpec}`))
      })

      after(async function () {
        const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
        expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
      })

      it('fails push with auth error', async function () {
        await execEnv(`${util.terminusdbScript()} push --user=${util.randomString()} --password=${util.randomString()} ${dbSpec}`)
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
        const cmd = `${util.terminusdbScript()} pull --user=${agent.user} --password=${agent.password} ${branchSpec}`
        {
          const r = await execEnv(cmd)
          expect(r.stdout).to.match(/^Pulling from remote 'origin'/)
          expect(r.stdout).to.match(/Remote up to date/)
          expect(r.stdout).to.match(/Local branch up to date/)
        }
        {
          const r = await execEnv(cmd)
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
                cmd = `${util.terminusdbScript()} db create ${dbSpec}`
                expectedStdout = ''
                break
              case 'clone':
                cmd = `${util.terminusdbScript()} clone --user=${agent.user} --password=${agent.password} ${url}`
                expectedStdout = 'Cloning the remote \'origin\'\n'
                break
              default:
                throw new Error(`Unknown cmd: ${cmd}`)
            }
            await execEnv(cmd)
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

  describe('local clone', function () {
    let agent
    let dbSpec
    let localDbSpec

    before(async function () {
      agent = new Agent().auth()
      dbSpec = agent.orgName + '/' + agent.dbName
      localDbSpec = dbSpec + '_local'
      const r = await execEnv(`${util.terminusdbScript()} db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
      const schema = [
        {
          '@id': 'Test',
          '@type': 'Class',
          name: 'xsd:string',
        },
      ]
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --data='${JSON.stringify(schema)}'`)
      const instance = { name: 'bar' }
      await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
    })

    after(async function () {
      const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    })

    it('performs local clone', async function () {
      const r = await execEnv(`${util.terminusdbScript()} clone ${dbSpec} ${localDbSpec}`)
      expect(r.stdout).to.match(/^Cloning the remote 'origin'/)
    })

    it('document is in local clone', async function () {
      const r = await execEnv(`${util.terminusdbScript()} doc get ${localDbSpec}`)
      expect(r.stdout).to.match(/^.*"name":"bar"/)
    })

    it('updates the local clone', async function () {
      const instance = { name: 'foo' }
      const r = await execEnv(`${util.terminusdbScript()} doc insert ${localDbSpec} --data='${JSON.stringify(instance)}'`)
      expect(r.stdout).to.match(/^Documents inserted:/)
    })

    it('can push the update', async function () {
      const r = await execEnv(`${util.terminusdbScript()} push ${localDbSpec}`)
      expect(r.stdout).to.match(/^Pushing to remote 'origin'/)
    })

    it('can see the update in origin', async function () {
      const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec}`)
      expect(r.stdout).to.match(/^.*"name":"bar".*\n.*"name":"foo"/)
    })
  })

  describe('empty local database', function () {
    let agent
    let dbSpec

    before(async function () {
      agent = new Agent().auth()
      dbSpec = agent.orgName + '/' + agent.dbName
      const r = await execEnv(`${util.terminusdbScript()} db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
    })

    after(async function () {
      const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    })

    it('fails push with unknown remote repository', async function () {
      await execEnv(`${util.terminusdbScript()} push --user=${agent.user} --password=${agent.password} ${dbSpec}`)
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
      await execEnv(`${util.terminusdbScript()} pull --user=${agent.user} --password=${agent.password} ${branchSpec}`)
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
