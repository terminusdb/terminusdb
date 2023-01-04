const fs = require('fs/promises')
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
    dbPath = './storage/' + util.randomString()
    envs = { ...process.env, TERMINUSDB_SERVER_DB_PATH: dbPath }
    {
      const r = await execEnv('./terminusdb.sh store init --force')
      expect(r.stdout).to.match(/^Successfully initialised database/)
    }
    dbSpec = `admin/${util.randomString()}`
    {
      const r = await execEnv(`./terminusdb.sh db create ${dbSpec}`)
      expect(r.stdout).to.match(new RegExp(`^Database created: ${dbSpec}`))
    }
  })

  after(async function () {
    const r = await execEnv(`./terminusdb.sh db delete ${dbSpec}`)
    expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    await fs.rm(dbPath, { recursive: true })
  })

  describe('passes schema insert, get, replace, delete, branch, apply', function () {
    const schema = { '@type': 'Class', negativeInteger: 'xsd:negativeInteger' }

    before(async function () {
      this.timeout(1000000)
      schema['@id'] = util.randomString()
      {
        const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: ${schema['@id']}`))
      }
      {
        const r = await execEnv(`./terminusdb.sh doc get ${dbSpec} --graph_type=schema`)
        const docs = r.stdout.split('\n').filter((line) => line.length > 0).map(JSON.parse)
        expect(docs[0]).to.deep.equal(util.defaultContext)
        expect(docs[1]).to.deep.equal(schema)
      }
      schema.hexBinary = { '@type': 'Optional', '@class': 'xsd:hexBinary' }
      {
        const r = await execEnv(`./terminusdb.sh doc replace ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: ${schema['@id']}`))
      }
    })

    beforeEach(async function () {
      await execEnv(`./terminusdb.sh doc delete ${dbSpec} --nuke`)
    })

    after(async function () {
      {
        const r = await execEnv(`./terminusdb.sh doc delete ${dbSpec} --graph_type=schema --id=${schema['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${schema['@id']}`))
      }
      {
        const r = await execEnv(`./terminusdb.sh doc get ${dbSpec} --graph_type=schema`)
        expect(JSON.parse(r.stdout)).to.deep.equal(util.defaultContext)
      }
    })

    it('passes doc query', async function () {
      const r = await execEnv('./terminusdb.sh doc get _system -q \'{ "@type" : "User", "name" : "admin"}\'')
      const j = JSON.parse(r.stdout)
      expect(j['@id']).to.equal('User/admin')
    })

    it('passes instance insert, get, replace, delete', async function () {
      this.timeout(300000)
      const instance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -88 }
      {
        const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      instance.negativeInteger = -255
      {
        const r = await execEnv(`./terminusdb.sh doc replace ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(JSON.parse(r.stdout)).to.deep.equal(instance)
      }
      instance.hexBinary = 'deadbeef'
      {
        const r = await execEnv(`./terminusdb.sh doc replace ${dbSpec} --graph_type=instance --create --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(JSON.parse(r.stdout)).to.deep.equal(instance)
      }
      {
        const r = await execEnv(`./terminusdb.sh doc delete ${dbSpec} --graph_type=instance --id=${instance['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${instance['@id']}`))
      }
      {
        const r = await execEnv(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(r.stdout).to.equal('')
      }
      instance['@id'] = `${schema['@id']}/${util.randomString()}`
      {
        const r = await execEnv(`./terminusdb.sh doc replace ${dbSpec} --graph_type=instance --create --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`./terminusdb.sh doc get ${dbSpec} --graph_type=instance`)
        expect(JSON.parse(r.stdout)).to.deep.equal(instance)
      }
      {
        const r = await execEnv(`./terminusdb.sh doc delete ${dbSpec} --graph_type=instance --id=${instance['@id']}`)
        expect(r.stdout).to.match(new RegExp(`^Documents deleted:\n 1: ${instance['@id']}`))
      }
    })

    it('passes insert, branch, insert apply', async function () {
      this.timeout(300000)
      const instance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -88 }
      {
        const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --graph_type=instance --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const r = await execEnv(`./terminusdb.sh branch create ${dbSpec}/local/branch/test --origin=${dbSpec}/local/branch/main`)
        expect(r.stdout).to.match(new RegExp(`^${dbSpec}/local/branch/test branch created`))
      }
      {
        const r = await execEnv(`./terminusdb.sh doc replace ${dbSpec}/local/branch/test --data='${JSON.stringify(instance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents replaced:\n 1: terminusdb:///data/${instance['@id']}`))
      }
      {
        const newInstance = { '@type': schema['@id'], '@id': `${schema['@id']}/${util.randomString()}`, negativeInteger: -42 }
        const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec}/local/branch/test --data='${JSON.stringify(newInstance)}'`)
        expect(r.stdout).to.match(new RegExp(`^Documents inserted:\n 1: terminusdb:///data/${newInstance['@id']}`))
      }
      {
        const r1 = await execEnv(`./terminusdb.sh log ${dbSpec}/local/branch/test -j`)
        const log = JSON.parse(r1.stdout)
        const latestCommit = log[0].identifier
        const previousCommit = log[1].identifier
        const r2 = await execEnv(`./terminusdb.sh apply ${dbSpec} --before_commit=${previousCommit} --after_commit=${latestCommit}`)
        const regexp = /^Successfully applied/
        expect(r2.stdout).to.match(regexp)
      }
      {
        const r = await execEnv(`./terminusdb.sh doc get ${dbSpec} -l --graph_type=instance`)
        const j = JSON.parse(r.stdout)
        expect(j.length).to.equal(2)
      }
      {
        const r = await execEnv(`./terminusdb.sh doc delete ${dbSpec} --nuke`)
        const regexp = /^Documents nuked/
        expect(r.stdout).to.match(regexp)
      }
    })
  })

  describe('backlinks', function () {
    beforeEach(async function () {
      await execEnv(`./terminusdb.sh doc delete ${dbSpec} --nuke`)
    })

    it('is able to link document with backlinks', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'My Thing',
      },
      {
        '@type': 'Other',
        '@linked-by': { '@ref': 'My Thing', '@property': 'other' },
        name: 'My Name',
      },
      ]
      await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`./terminusdb.sh doc get ${dbSpec} --as-list=true`)
      const docs = JSON.parse(r2.stdout)
      expect(docs).has.length(2)
      const r3 = await execEnv(`./terminusdb.sh doc get ${dbSpec} --as-list=true --type=Other`)
      const [other] = JSON.parse(r3.stdout)
      const otherId = other['@id']
      const r4 = await execEnv(`./terminusdb.sh doc get ${dbSpec} --as-list=true --type=Thing`)
      const [thing] = JSON.parse(r4.stdout)
      expect(thing.other).to.equal(otherId)
    })

    it('links back to two documents', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'Thing1',
      },
      {
        '@type': 'Thing',
        '@capture': 'Thing2',
      },
      {
        '@type': 'Other',
        '@linked-by': [{ '@ref': 'Thing1', '@property': 'other' },
          { '@ref': 'Thing2', '@property': 'other' }],
        name: 'My Name',
      },
      ]
      await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`./terminusdb.sh doc get ${dbSpec} --as-list=true`)
      const docs = JSON.parse(r2.stdout)
      expect(docs).has.length(3)
      const r3 = await execEnv(`./terminusdb.sh doc get ${dbSpec} --as-list=true --type=Other`)
      const [other] = JSON.parse(r3.stdout)
      const otherId = other['@id']
      const r4 = await execEnv(`./terminusdb.sh doc get ${dbSpec} --as-list=true --type=Thing`)
      const [thing1, thing2] = JSON.parse(r4.stdout)
      expect(thing1.other).to.equal(otherId)
      expect(thing2.other).to.equal(otherId)
    })

    it('is able to link subdocument with backlinks', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'My Thing',
      },
      {
        '@type': 'Other',
        '@linked-by': { '@ref': 'My Thing', '@property': 'other' },
        name: 'My Name',
      },
      ]
      await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`./terminusdb.sh doc get ${dbSpec} --as-list=true`)
      const [doc] = JSON.parse(r2.stdout)
      expect(doc.other.name).to.equal('My Name')
    })

    it('fails to link subdocument with no backlinks', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Other',
        '@linked-by': [],
        name: 'My Name',
      },
      ]
      const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
      expect(r.stderr).to.match(/^Error: A sub-document has parent cardinality other than one.*/)
    })

    it('fails to link subdocument already in document', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'Thing1',
      },
      {
        '@type': 'Thing',
        other: {
          '@type': 'Other',
          '@linked-by': { '@ref': 'Thing1', '@property': 'other' },
          name: 'My Name',
        },
      }]
      await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
      // expect(r.stderr).to.match(/^Error: A sub-document has parent cardinality other than one.*/)
      await execEnv(`./terminusdb.sh triples dump ${dbSpec}/local/branch/main/instance`)
    })

    it('fails to link subdocument with backlinks twice', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'Thing1',
      },
      {
        '@type': 'Thing',
        '@capture': 'Thing2',
      },
      {
        '@type': 'Other',
        '@linked-by': [{ '@ref': 'Thing1', '@property': 'other' },
          { '@ref': 'Thing2', '@property': 'other' }],
        name: 'My Name',
      },
      ]
      const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
      expect(r.stderr).to.match(/^Error: A sub-document has parent cardinality other than one.*/)
    })

    it('fails to find property', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'Thing1',
      },
      {
        '@type': 'Thing',
        '@capture': 'Thing2',
      },
      {
        '@type': 'Other',
        '@linked-by': [{ '@ref': 'Thing1' },
          { '@ref': 'Thing2', '@property': 'other' }],
        name: 'My Name',
      },
      ]
      const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
      expect(r.stderr).to.match(/^Error: A sub-document has parent cardinality other than one.*/)
    })

    it('fails to find ref or id', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'Thing',
      },
      {
        '@type': 'Other',
        '@linked-by': [{ '@property': 'other' }],
        name: 'My Name',
      },
      ]
      const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
      expect(r.stderr).to.match(/^Error: Back links were used with no ref or id.*/)
    })

    it('has malformed link id', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@subdocument': [],
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'Thing',
      },
      {
        '@type': 'Other',
        '@linked-by': [{ '@property': 'other', '@id': [] }],
        name: 'My Name',
      },
      ]
      const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
      expect(r.stderr).to.match(/^Error: The link Id did not have a valid form.*/)
    })

    it('fails to make backlink with unknown property', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'foo://base/',
        '@schema': 'foo://schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Thing',
        other: {
          '@type': 'Optional',
          '@class': 'Other',
        },
      },
      {
        '@type': 'Class',
        '@key': { '@type': 'Random' },
        '@id': 'Other',
        name: 'xsd:string',
      }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'My Thing',
      },
      {
        '@type': 'Other',
        '@linked-by': { '@ref': 'My Thing', '@property': 'tother' },
        name: 'My Name',
      },
      ]
      const r = await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(instance)}'| true`)
      expect(r.stderr).to.match(/^Error: Schema check failure(.|\n)*unknown_property_for_type.*/)
    })
  })

  describe('schema manipulation', function () {
    it('adds an xsd:Name', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Test',
        name: 'xsd:Name',
      }]
      const db = util.randomString()
      await execEnv(`./terminusdb.sh db create admin/${db}`)
      await execEnv(`./terminusdb.sh doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = { name: 'Test' }
      await execEnv(`./terminusdb.sh doc insert admin/${db} --data='${JSON.stringify(instance)}'`)
      const r = await execEnv(`./terminusdb.sh doc get admin/${db}`)
      const js = JSON.parse(r.stdout)
      expect(js.name).to.equal('Test')
      await execEnv(`./terminusdb.sh db delete admin/${db}`)
    })

    it('adds a broken context', async function () {
      const schema = [
        {
          '@base': 'terminusdb:///data/',
          '@schema': 'terminusdb:///schema#',
          '@type': '@context',
          pfx: 'abfab',
        },
        {
          '@id': 'pfx:somethign',
          '@type': 'Class',
        },
      ]
      const db = util.randomString()
      await execEnv(`./terminusdb.sh db create admin/${db}`)
      const r = await execEnv(`./terminusdb.sh doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}' | true`)
      expect(r.stderr).to.match(/^Error: The prefix pfx used in the context does not resolve to a URI.*/)
      await execEnv(`./terminusdb.sh db delete admin/${db}`)
    })

    it('uses schema metadata', async function () {
      const schema = [
        {
          '@base': 'terminusdb:///data/',
          '@schema': 'terminusdb:///schema#',
          '@type': '@context',
          '@metadata': { some_meta_key: 'some_meta_value' },
        },
      ]
      const db = util.randomString()
      await execEnv(`./terminusdb.sh db create admin/${db}`)
      await execEnv(`./terminusdb.sh doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}'`)
      await execEnv(`./terminusdb.sh doc get -g schema admin/${db}`)
      await execEnv(`./terminusdb.sh db delete admin/${db}`)
    })

    it('adds a bad language', async function () {
      const schema = {
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
        '@type': '@context',
        '@documentation': {
          '@language': 'bogus',
          '@title': 'Example Schema',
          '@description': 'This is an example schema. We are using it to demonstrate the ability to display information in multiple languages about the same semantic content.',
          '@authors': ['Gavin Mendel-Gleason'],
        },
      }
      const r = await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}' | true`)
      expect(r.stderr).to.match(/^Error: value "bogus" could not be casted to a .*/)
    })

    it('adds a lang string', async function () {
      const schema = [
        {
          '@type': '@context',
          '@base': 'terminusdb://asdf/',
          '@schema': 'terminusdb://schema',
          rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        },
        {
          '@id': 'Note',
          '@type': 'Class',
          noteText: {
            '@class': 'rdf:langString',
            '@type': 'Set',
          },
        }]
      await execEnv(`./terminusdb.sh doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const doc = {
        noteText: [
          {
            '@lang': 'ka',
            '@value': 'មរនមាត្តា',
          },
          {
            '@lang': 'hi',
            '@value': 'ksajd',
          },
        ],
        '@type': 'Note',
      }
      await execEnv(`./terminusdb.sh doc insert ${dbSpec} --data='${JSON.stringify(doc)}'`)
      const r = await execEnv(`./terminusdb.sh doc get ${dbSpec}`)
      const js = JSON.parse(r.stdout)
      const result = [
        {
          '@lang': 'hi',
          '@value': 'ksajd',
        },
        {
          '@lang': 'ka',
          '@value': 'មរនមាត្តា',
        },
      ]
      expect(js.noteText).to.deep.equal(result)
    })
  })

  describe('escape works ok', function () {
    it('double escape', async function () {
      const schema = [{
        '@type': '@context',
        '@base': 'terminusdb:///data/',
        '@schema': 'terminusdb:///schema#',
      },
      {
        '@type': 'Class',
        '@id': 'Test',
        test: 'xsd:string',
      }]
      const db = util.randomString()
      await execEnv(`./terminusdb.sh db create admin/${db}`)
      await execEnv(`./terminusdb.sh doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = { test: 'hello\n world' }
      await execEnv(`./terminusdb.sh doc insert admin/${db} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`./terminusdb.sh doc get admin/${db}`)
      const res = JSON.parse(r2.stdout)
      expect(res.test).to.equal('hello\n world')
      await execEnv(`./terminusdb.sh db delete admin/${db}`)
    })
  })
})
