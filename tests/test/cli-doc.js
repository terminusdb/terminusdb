const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-doc', function () {
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

  describe('passes schema insert, get, replace, delete', function () {
    const schema = { '@type': 'Class', negativeInteger: 'xsd:negativeInteger' }

    before(async function () {
      this.timeout(50000)
      schema['@id'] = util.randomString()
      {
        const r = await exec(`./terminusdb.sh doc insert ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: ${schema['@id']}`))
      }
      {
        const r = await exec(`./terminusdb.sh doc get ${dbSpec} --graph_type=schema`)
        const docs = r.stdout.split('\n').filter((line) => line.length > 0).map(JSON.parse)
        expect(docs[0]).to.deep.equal(util.defaultContext)
        expect(docs[1]).to.deep.equal(schema)
      }
      schema.hexBinary = { '@type': 'Optional', '@class': 'xsd:hexBinary' }
      {
        const r = await exec(`./terminusdb.sh doc replace ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: ${schema['@id']}`))
      }
    })

    after(async function () {
      {
        const r = await exec(`./terminusdb.sh doc delete ${dbSpec} --graph_type=schema --id=${schema['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${schema['@id']}`))
      }
      {
        const r = await exec(`./terminusdb.sh doc get ${dbSpec} --graph_type=schema`)
        expect(JSON.parse(r.stdout)).to.deep.equal(util.defaultContext)
      }
    })

    it('passes instance insert, get, replace, delete', async function () {
      this.timeout(100000)
      const instance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -88 }
      {
        const r = await exec(`./terminusdb.sh doc insert ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      instance.negativeInteger = -255
      {
        const r = await exec(`./terminusdb.sh doc replace ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await exec(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(JSON.parse(r.stdout)).to.deep.equal(instance)
      }
      instance.hexBinary = 'deadbeef'
      {
        const r = await exec(`./terminusdb.sh doc replace ${dbSpec} --graph_type=instance --create --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await exec(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(JSON.parse(r.stdout)).to.deep.equal(instance)
      }
      {
        const r = await exec(`./terminusdb.sh doc delete ${dbSpec} --graph_type=instance --id=${instance['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${instance['@id']}`))
      }
      {
        const r = await exec(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(r.stdout).to.equal('')
      }
      instance['@id'] = `${schema['@id']}/${util.randomString()}`
      {
        const r = await exec(`./terminusdb.sh doc replace ${dbSpec} --graph_type=instance --create --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await exec(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(JSON.parse(r.stdout)).to.deep.equal(instance)
      }
      {
        const r = await exec(`./terminusdb.sh doc delete ${dbSpec} --graph_type=instance --id=${instance['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${instance['@id']}`))
      }
    })
  })
})
