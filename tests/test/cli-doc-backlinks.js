const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-doc backlinks', function () {
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

  describe('backlinks', function () {
    beforeEach(async function () {
      await execEnv(`${util.terminusdbScript()} doc delete ${dbSpec} --nuke`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list=true`)
      const docs = JSON.parse(r2.stdout)
      expect(docs).has.length(2)
      const r3 = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list=true --type=Other`)
      const [other] = JSON.parse(r3.stdout)
      const otherId = other['@id']
      const r4 = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list=true --type=Thing`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list=true`)
      const docs = JSON.parse(r2.stdout)
      expect(docs).has.length(3)
      const r3 = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list=true --type=Other`)
      const [other] = JSON.parse(r3.stdout)
      const otherId = other['@id']
      const r4 = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list=true --type=Thing`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec} --as-list=true`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Other',
        '@linked-by': [],
        name: 'My Name',
      },
      ]
      const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
      // expect(r.stderr).to.match(/^Error: A sub-document has parent cardinality other than one.*/)
      await execEnv(`${util.terminusdbScript()} triples dump ${dbSpec}/local/branch/main/instance`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}' | true`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = [{
        '@type': 'Thing',
        '@capture': 'My Thing',
      },
      {
        '@type': 'Other',
        '@linked-by': [{ '@ref': 'My Thing', '@property': 'tother' }],
        name: 'My Name',
      },
      ]
      const r = await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(instance)}'| true`)
      expect(r.stderr).to.match(/^Error: Schema check failure(.|\n)*unknown_property_for_type.*/)
    })
  })
})
