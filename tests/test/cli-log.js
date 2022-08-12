const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-log', function () {
  let dbSpec

  before(async function () {
    this.timeout(30000)
    process.env.TERMINUSDB_SERVER_DB_PATH = './storage/' + util.randomString()
    {
      const r = await exec('./terminusdb.sh store init --force')
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
    dbSpec = `admin/${util.randomString()}`
    {
      const r = await exec(`./terminusdb.sh db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
    }
  })

  after(async function () {
    const r = await exec(`./terminusdb.sh db delete ${dbSpec}`)
    expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    await fs.rm(process.env.TERMINUSDB_SERVER_DB_PATH, { recursive: true })
    delete process.env.TERMINUSDB_SERVER_DB_PATH
  })

  describe('checks logs', function () {
    const schema = { '@type': 'Class', negativeInteger: 'xsd:negativeInteger' }

    before(async function () {
      this.timeout(50000)
      schema['@id'] = util.randomString()
      {
        const r = await exec(`./terminusdb.sh doc insert ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: ${schema['@id']}`))
      }
    })

    it('gets a truncated log', async function () {
      const db = `admin/${util.randomString()}`
      await exec(`./terminusdb.sh db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await exec(`./terminusdb.sh doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await exec(`./terminusdb.sh log ${db} -j -s 0 -c 3`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(3)

      const r2 = await exec(`./terminusdb.sh log ${db} -j -s 2 -c 3`)
      const log2 = JSON.parse(r2.stdout)
      expect(log2.length).to.equal(2)

      await exec(`./terminusdb.sh db delete ${db}`)
    })

    it('gets all the logs even with higher count than amount of entries', async function () {
      const db = `admin/${util.randomString()}`
      await exec(`./terminusdb.sh db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await exec(`./terminusdb.sh doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await exec(`./terminusdb.sh log ${db} -j -s 0 -c 100`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(4)

      await exec(`./terminusdb.sh db delete ${db}`)
    })

    it('gets empty log list with higher start than amount of entries', async function () {
      const db = `admin/${util.randomString()}`
      await exec(`./terminusdb.sh db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await exec(`./terminusdb.sh doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await exec(`./terminusdb.sh log ${db} -j -s 10 -c 100`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(0)

      await exec(`./terminusdb.sh db delete ${db}`)
    })

    it('gets empty log when count is zero', async function () {
      const db = `admin/${util.randomString()}`
      await exec(`./terminusdb.sh db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await exec(`./terminusdb.sh doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      const r1 = await exec(`./terminusdb.sh log ${db} -j -s 1 -c 0`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(0)

      await exec(`./terminusdb.sh db delete ${db}`)
    })

    it('gets two entries when count is two', async function () {
      const db = `admin/${util.randomString()}`
      await exec(`./terminusdb.sh db create ${db}`)
      const schema = {
        '@id': 'Thing',
        '@type': 'Class',
        negativeInteger: 'xsd:negativeInteger',
      }

      await exec(`./terminusdb.sh doc insert ${db} -g schema --data='${JSON.stringify(schema)}'`)

      const instance = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance2 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      const instance3 = { '@id': `Thing/${util.randomString()}`, negativeInteger: -88 }
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance2)}'`)
      await exec(`./terminusdb.sh doc insert ${db} --data='${JSON.stringify(instance3)}'`)
      const r1 = await exec(`./terminusdb.sh log ${db} -j -s 0 -c 2`)
      const log1 = JSON.parse(r1.stdout)
      expect(log1.length).to.equal(2)

      await exec(`./terminusdb.sh db delete ${db}`)
    })
  })
})
