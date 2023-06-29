const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-merge', function () {
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
  })

  after(async function () {
    await fs.rm(dbPath, { recursive: true })
  })

  it('catches a missing instance in merge candidates', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db}`)
    const schema = JSON.stringify(
      {
        '@type': 'Class',
        '@id': 'Thing',
        '@key': {
          '@type': 'Lexical',
          '@fields': ['name'],
        },
        name: 'xsd:string',
      })
    await execEnv(`./terminusdb.sh doc insert -g schema admin/${db} -d '${schema}'`)
    const db2 = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db2}`)

    const result = await execEnv(`echo "admin/${db}" | ./terminusdb.sh concat admin/${db2} || true`)
    expect(result.stderr).to.match(/Error: One of the descriptors used in the merge operation did not have an associated instance layer/)
  })

  it('reports a readable error for a non-base layer', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db}`)
    const schema = JSON.stringify(
      {
        '@type': 'Class',
        '@id': 'Thing',
        '@key': {
          '@type': 'Lexical',
          '@fields': ['name'],
        },
        name: 'xsd:string',
      })
    await execEnv(`./terminusdb.sh doc insert -g schema admin/${db} -d '${schema}'`)
    const doc1 = JSON.stringify(
      { name: 'doc1' })
    await execEnv(`./terminusdb.sh doc insert admin/${db} -d '${doc1}'`)
    const doc2 = JSON.stringify(
      { name: 'doc2' })
    await execEnv(`./terminusdb.sh doc insert -j admin/${db} -d '${doc2}'`)

    const db2 = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db2}`)
    await execEnv(`./terminusdb.sh doc insert -g schema admin/${db2} -d '${schema}'`)
    const doc3 = JSON.stringify(
      { name: 'doc3' })
    await execEnv(`./terminusdb.sh doc insert admin/${db2} -d '${doc1}'`)
    const doc4 = JSON.stringify(
      { name: 'doc4' })
    await execEnv(`./terminusdb.sh doc insert -j admin/${db2} -d '${doc3}'`)
    await execEnv(`./terminusdb.sh doc insert -j admin/${db2} -d '${doc4}'`)

    const mergeDB = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${mergeDB}`)
    const result = await execEnv(`echo "admin/${db}" "admin/${db2}" | ./terminusdb.sh concat admin/${mergeDB} || true`)
    expect(result.stderr).to.match(/^Error: One of the descriptors/)
  })

  it('merges two layers', async function () {
    const db = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${db}`)
    const schema = JSON.stringify(
      {
        '@type': 'Class',
        '@id': 'Thing',
        '@key': {
          '@type': 'Lexical',
          '@fields': ['name'],
        },
        name: 'xsd:string',
      })
    await execEnv(`./terminusdb.sh doc insert -g schema admin/${db} -d '${schema}'`)
    const doc1 = JSON.stringify(
      { name: 'doc1' })
    await execEnv(`./terminusdb.sh doc insert admin/${db} -d '${doc1}'`)
    const db2 = util.randomString()

    await execEnv(`./terminusdb.sh db create admin/${db2}`)
    await execEnv(`./terminusdb.sh doc insert -g schema admin/${db2} -d '${schema}'`)
    const doc2 = JSON.stringify(
      { name: 'doc2' })
    await execEnv(`./terminusdb.sh doc insert admin/${db2} -d '${doc2}'`)

    const mergeDB = util.randomString()
    await execEnv(`./terminusdb.sh db create admin/${mergeDB}`)
    const result = await execEnv(`echo "admin/${db}" "admin/${db2}" | ./terminusdb.sh concat admin/${mergeDB}`)

    const matchResult = result.stdout.match(/^\nSuccessfully concatenated layers into commit_id: "(?<commit>[a-z0-9]+)"/)

    const docResults = await execEnv(`./terminusdb.sh doc get -l admin/${mergeDB}/local/commit/${matchResult.groups.commit}`)

    const docs = JSON.parse(docResults.stdout)
    expect(docs).length(2)
    expect(docs).to.deep.equal([
      { '@id': 'Thing/doc1', '@type': 'Thing', name: 'doc1' },
      { '@id': 'Thing/doc2', '@type': 'Thing', name: 'doc2' },
    ])
  })
})
