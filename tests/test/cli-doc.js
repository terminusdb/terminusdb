const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-doc', function () {
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
    dbSpec = `admin/${util.randomString()}`
    {
      const r = await execEnv(`${util.terminusdbScript()} db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
    }
  })

  after(async function () {
    const r = await execEnv(`${util.terminusdbScript()} db delete ${dbSpec}`)
    expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    await fs.rm(dbPath, { recursive: true, force: true })
  })

  describe('passes schema insert, get, replace, delete, branch, apply', function () {
    const schema = { '@type': 'Class', negativeInteger: 'xsd:negativeInteger' }

    before(async function () {
      this.timeout(1000000)
      schema['@id'] = util.randomString()
      {
        const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: ${schema['@id']}`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --graph_type=schema`)
        const docs = r.stdout.split('\n').filter((line) => line.length > 0).map(JSON.parse)
        expect(docs[0]).to.deep.equal(util.defaultContext)
        expect(docs[1]).to.deep.equal(schema)
      }
      schema.hexBinary = { '@type': 'Optional', '@class': 'xsd:hexBinary' }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc replace ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: ${schema['@id']}`))
      }
    })

    beforeEach(async function () {
      await execEnv(`${util.terminusdbScript()} doc delete ${dbSpec} --nuke`)
    })

    after(async function () {
      // Clean up any remaining instance documents before deleting schema
      await execEnv(`${util.terminusdbScript()} doc delete ${dbSpec} --nuke`)
      {
        const r = await execEnv(`${util.terminusdbScript()} doc delete ${dbSpec} --graph_type=schema --id=${schema['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${schema['@id']}`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --graph_type=schema`)
        expect(JSON.parse(r.stdout)).to.deep.equal(util.defaultContext)
      }
    })

    it('passes doc query', async function () {
      const r = await execEnv(`${util.terminusdbScript()} doc get _system -q '{ "@type" : "User", "name" : "admin"}'`)
      const j = JSON.parse(r.stdout)
      expect(j['@id']).to.equal('User/admin')
    })

    it('passes instance insert, get, replace, delete', async function () {
      this.timeout(300000)
      const instance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -88 }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      instance.negativeInteger = '-255'
      {
        const r = await execEnv(`${util.terminusdbScript()} doc replace ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --graph_type=instance`)
        const result = JSON.parse(r.stdout)
        // xsd:negativeInteger is returned as JSON number per JSON_SERIALIZATION_RULES.md
        const expectedInstance = { ...instance, negativeInteger: -255 }
        expect(result).to.deep.equal(expectedInstance)
      }
      instance.hexBinary = 'deadbeef'
      {
        const r = await execEnv(`${util.terminusdbScript()} doc replace ${dbSpec} --graph_type=instance --create --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --graph_type=instance`)
        const result = JSON.parse(r.stdout)
        // xsd:negativeInteger is returned as JSON number per JSON_SERIALIZATION_RULES.md
        const expectedInstance = { ...instance, negativeInteger: -255 }
        expect(result).to.deep.equal(expectedInstance)
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc delete ${dbSpec} --graph_type=instance --id=${instance['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${instance['@id']}`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --graph_type=instance`)
        expect(r.stdout).to.equal('')
      }
      instance['@id'] = `${schema['@id']}/${util.randomString()}`
      {
        const r = await execEnv(`${util.terminusdbScript()} doc replace ${dbSpec} --graph_type=instance --create --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --graph_type=instance`)
        const result = JSON.parse(r.stdout)
        // xsd:negativeInteger is returned as JSON number per JSON_SERIALIZATION_RULES.md
        const expectedInstance = { ...instance, negativeInteger: -255 }
        expect(result).to.deep.equal(expectedInstance)
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc delete ${dbSpec} --graph_type=instance --id=${instance['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${instance['@id']}`))
      }
    })

    it('passes insert, branch, insert apply', async function () {
      this.timeout(300000)
      const instance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -88 }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} branch create ${dbSpec}/local/branch/test --origin=${dbSpec}/local/branch/main`)
        expect(r.stdout).to.match(new RegExp(`^${dbSpec}/local/branch/test branch created`))
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc replace ${dbSpec}/local/branch/test --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const newInstance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -42 }
        const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec}/local/branch/test --data='${JSON.stringify(newInstance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${newInstance['@id']}`))
      }
      {
        const r1 = await execEnv(`${util.terminusdbScript()} log ${dbSpec}/local/branch/test -j`)
        const log = JSON.parse(r1.stdout)
        const latestCommit = log[0].identifier
        const previousCommit = log[1].identifier
        const r2 = await execEnv(`${util.terminusdbScript()} apply ${dbSpec} --before_commit=${previousCommit} --after_commit=${latestCommit}`)
        const regexp = /^Successfully applied/
        expect(r2.stdout).to.match(regexp)
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} -l --graph_type=instance`)
        const j = JSON.parse(r.stdout)
        expect(j.length).to.equal(2)
      }
      {
        const r = await execEnv(`${util.terminusdbScript()} doc delete ${dbSpec} --nuke`)
        const regexp = /^Documents nuked/
        expect(r.stdout).to.match(regexp)
      }
    })
  })
})
