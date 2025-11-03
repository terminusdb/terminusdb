const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-log', function () {
  let dbSpec
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

  describe('checks logs', function () {
    const schema = { '@type': 'Class', negativeInteger: 'xsd:negativeInteger' }

    before(async function () {
      this.timeout(1000000)
      schema['@id'] = util.randomString()
      {
        const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: ${schema['@id']}`))
      }
    })

    it('gets a truncated log', async function () {
      const db = `admin/${util.randomString()}`
      await execEnv(`${util.terminusdbScript()} db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await execEnv(`${util.terminusdbScript()} doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await execEnv(`${util.terminusdbScript()} log ${db} -j -s 0 -c 3`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(3)

      const r2 = await execEnv(`${util.terminusdbScript()} log ${db} -j -s 2 -c 3`)
      const log2 = JSON.parse(r2.stdout)
      expect(log2.length).to.equal(2)

      await execEnv(`${util.terminusdbScript()} db delete ${db}`)
    })

    it('gets all the logs even with higher count than amount of entries', async function () {
      const db = `admin/${util.randomString()}`
      await execEnv(`${util.terminusdbScript()} db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await execEnv(`${util.terminusdbScript()} doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await execEnv(`${util.terminusdbScript()} log ${db} -j -s 0 -c 100`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(4)

      await execEnv(`${util.terminusdbScript()} db delete ${db}`)
    })

    it('gets empty log list with higher start than amount of entries', async function () {
      const db = `admin/${util.randomString()}`
      await execEnv(`${util.terminusdbScript()} db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await execEnv(`${util.terminusdbScript()} doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await execEnv(`${util.terminusdbScript()} log ${db} -j -s 10 -c 100`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(0)

      await execEnv(`${util.terminusdbScript()} db delete ${db}`)
    })

    it('gets empty log when count is zero', async function () {
      const db = `admin/${util.randomString()}`
      await execEnv(`${util.terminusdbScript()} db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await execEnv(`${util.terminusdbScript()} doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      const r1 = await execEnv(`${util.terminusdbScript()} log ${db} -j -s 1 -c 0`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(0)

      await execEnv(`${util.terminusdbScript()} db delete ${db}`)
    })

    it('gets two entries when count is two', async function () {
      const db = `admin/${util.randomString()}`
      await execEnv(`${util.terminusdbScript()} db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await execEnv(`${util.terminusdbScript()} doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await execEnv(`${util.terminusdbScript()} doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await execEnv(`${util.terminusdbScript()} log ${db} -j -s 0 -c 2`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(2)

      await execEnv(`${util.terminusdbScript()} db delete ${db}`)
    })
  })
})
