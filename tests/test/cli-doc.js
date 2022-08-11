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

  describe('passes schema insert, get, replace, delete, branch, apply', function () {
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

    it('passes doc query', async function () {
      const r = await exec('./terminusdb.sh doc get _system -q \'{ "@type" : "User", "name" : "admin"}\'')
      const j = JSON.parse(r.stdout)
      expect(j['@id']).to.equal('User/admin')
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

    it('passes insert, branch, insert apply', async function () {
      this.timeout(100000)
      const instance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -88 }
      {
        const r = await exec(`./terminusdb.sh doc insert ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await exec(`./terminusdb.sh branch create ${dbSpec}/local/branch/test --origin=${dbSpec}/local/branch/main`)
        expect(r.stdout).to.match(new RegExp(`^${dbSpec}/local/branch/test branch created`))
      }
      {
        const r = await exec(`./terminusdb.sh doc replace ${dbSpec}/local/branch/test --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const newInstance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -42 }
        const r = await exec(`./terminusdb.sh doc insert ${dbSpec}/local/branch/test --data='${JSON.stringify(newInstance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${newInstance['@id']}`))
      }
      {
        const r1 = await exec(`./terminusdb.sh log ${dbSpec}/local/branch/test -j`)
        const log = JSON.parse(r1.stdout)
        const latestCommit = log[0].identifier
        const previousCommit = log[1].identifier
        const r2 = await exec(`./terminusdb.sh apply ${dbSpec} --before_commit=${previousCommit} --after_commit=${latestCommit}`)
        const regexp = /^Successfully applied/
        expect(r2.stdout).to.match(regexp)
      }
      {
        const r = await exec(`./terminusdb.sh doc get ${dbSpec} -l --graph_type=instance`)
        const j = JSON.parse(r.stdout)
        expect(j.length).to.equal(2)
      }
      {
        const r = await exec(`./terminusdb.sh doc delete ${dbSpec} --nuke`)
        const regexp = /^Documents nuked/
        expect(r.stdout).to.match(regexp)
      }
    })
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
      const schema = { '@id' : 'Thing',
                       '@type': 'Class',
                       negativeInteger: 'xsd:negativeInteger' }

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
  })
})
