const fs = require('fs/promises')
const path = require('path')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-doc schema manipulation', function () {
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
      await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
      await execEnv(`${util.terminusdbScript()} doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = { name: 'Test' }
      await execEnv(`${util.terminusdbScript()} doc insert admin/${db} --data='${JSON.stringify(instance)}'`)
      const r = await execEnv(`${util.terminusdbScript()} doc get admin/${db}`)
      const js = JSON.parse(r.stdout)
      expect(js.name).to.equal('Test')
      await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
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
      await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
      const r = await execEnv(`${util.terminusdbScript()} doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}' | true`)
      expect(r.stderr).to.match(/^Error: The prefix pfx used in the context does not resolve to a URI.*/)
      await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
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
      await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
      await execEnv(`${util.terminusdbScript()} doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}'`)
      const res = await execEnv(`${util.terminusdbScript()} doc get -g schema admin/${db}`)
      const context = JSON.parse(res.stdout)
      expect(context['@metadata']).to.deep.equal({ some_meta_key: 'some_meta_value' })
      await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
    })

    it('cant insert context', async function () {
      const schema = [
        {
          '@base': 'terminusdb:///data/',
          '@schema': 'terminusdb:///schema#',
          '@type': '@context',
        },
      ]
      const db = util.randomString()
      await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
      const res = await execEnv(`${util.terminusdbScript()} doc insert -g schema admin/${db} --data='${JSON.stringify(schema)}' | true`)
      expect(res.stderr).to.match(/Error: Inserting contexts.*/)
      await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
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
      const r = await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}' | true`)
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
      await execEnv(`${util.terminusdbScript()} doc insert -g schema ${dbSpec} --full-replace --data='${JSON.stringify(schema)}'`)
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
      await execEnv(`${util.terminusdbScript()} doc insert ${dbSpec} --data='${JSON.stringify(doc)}'`)
      const r = await execEnv(`${util.terminusdbScript()} doc get ${dbSpec}`)
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
      await execEnv(`${util.terminusdbScript()} db create admin/${db}`)
      await execEnv(`${util.terminusdbScript()} doc insert -g schema admin/${db} --full-replace --data='${JSON.stringify(schema)}'`)
      const instance = { test: 'hello\n world' }
      await execEnv(`${util.terminusdbScript()} doc insert admin/${db} --data='${JSON.stringify(instance)}'`)
      const r2 = await execEnv(`${util.terminusdbScript()} doc get admin/${db}`)
      const res = JSON.parse(r2.stdout)
      expect(res.test).to.equal('hello\n world')
      await execEnv(`${util.terminusdbScript()} db delete admin/${db}`)
    })
  })
})
