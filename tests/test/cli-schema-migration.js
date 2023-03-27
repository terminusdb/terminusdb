const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { util } = require('../lib')

describe('cli-schema-migration', function () {
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
    const schema = [
      {
        '@type': 'Class',
        '@id' : 'String',
        string: 'xsd:string',
      },
      {
        '@type': 'Class',
        '@id': 'Int',
        int: 'xsd:int',
      },
      {
        '@type': 'Class',
        '@id': 'NonNegativeInteger',
        nonNegativeInteger: 'xsd:nonNegativeInteger',
      },
      {
        '@type': 'Class',
        '@id': 'OptionalString',
        optionalString: {
          '@type': 'Optional',
          '@class': 'xsd:string',
        },
      },
    ]
    await execEnv(`./terminusdb.sh doc insert ${dbSpec} --graph_type=schema --data='${JSON.stringify(schema)}'`)
  })

  after(async function () {
    const r = await execEnv(`./terminusdb.sh db delete ${dbSpec}`)
    expect(r.stdout).to.match(new RegExp(`^Database deleted: ${dbSpec}`))
    await fs.rm(dbPath, { recursive: true })
  })

  it('target a migration of a branch', async function () {
    const instance = [
      {
        '@type': 'String',
        string: 'asdf',
      },
      {
        '@type': 'String',
        string: 'fdsa',
      },
      {
        '@type': 'Int',
        int: 23,
      },
    ]
    await execEnv(`./terminusdb.sh doc insert admin/${dbSpec} --data='${JSON.stringify(instance)}'`)
    await execEnv(`./terminusdb.sh branch create admin/${dbSpec}/local/branch/foo`)
    const branchInstance = [
      {
        '@type': 'String',
        string: 'new',
      },
      {
        '@type': 'Int',
        int: 42,
      },
    ]
    await execEnv(`./terminusdb.sh doc insert admin/${dbSpec}/local/branch/foo --data='${JSON.stringify(branchInstance)}'`)

    await execEnv(`./terminusdb.sh migrate admin/${dbSpec} --operations='[drop_class_property("String", "string"), create_class_property("String", "string", {"@type" : "Optional", "@class" : "String"})]'`)
    await execEnv(`./terminusdb.sh migrate admin/${dbSpec}/local/branch/foo --target admin/${dbSpec}`)
    const r = execEnv(`./terminusdb.sh doc get admin/${dbSpec}/local/branch/foo`)
    console.log(r.stdout)
  })
})
