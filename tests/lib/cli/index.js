const exec = require('util').promisify(require('child_process').exec)
const fs = require('fs/promises')
const path = require('path')

const { Params } = require('../params.js')
const util = require('../util.js')

const Bundle = require('./bundle.js')
const Db = require('./db.js')
const Doc = require('./doc.js')
const Store = require('./store.js')
const Unbundle = require('./unbundle.js')

class Cli {
  constructor (params) {
    this.params = params
    this.filesToDelete = []
    const dbPath = `./storage/${util.randomString()}`
    const testDir = path.join(__dirname, '..', '..')
    const rootDir = path.join(testDir, '..')
    const terminusdbExec = path.join(rootDir, 'terminusdb')
    this.terminusdbSh = path.join(testDir, 'terminusdb.sh')
    this.envs = {
      ...process.env,
      TERMINUSDB_SERVER_DB_PATH: dbPath,
      TERMINUSDB_EXEC_PATH: terminusdbExec,
    }
  }

  async deleteFiles () {
    for (const fileName of this.filesToDelete) {
      await fs.rm(fileName, { force: true, recursive: true })
    }
  }

  async cleanup () {
    await this.deleteFiles()
  }

  builder () {
    return new CommandBuilder(this.filesToDelete, this.params, this.terminusdbSh, this.envs)
  }

  get store () {
    return new Store(this.builder())
  }

  get db () {
    return new Db(this.builder())
  }

  get doc () {
    return new Doc(this.builder())
  }

  bundle (params) {
    return new Bundle(this.builder(), params)
  }

  unbundle (params) {
    return new Unbundle(this.builder(), params)
  }
}

class CommandBuilder {
  constructor (filesToDelete, params, terminusdbSh, envs) {
    params = new Params({ ...params })
    this.debugCommand = params.boolean('debugCommand', false)
    this.debugOutput = params.boolean('debugOutput', false)
    this.command = [terminusdbSh]
    this.filesToDelete = filesToDelete
    this.envs = envs
  }

  arg (a) {
    this.command.push(a)
    return this
  }

  registerFileForCleanup (fileName) {
    this.filesToDelete.push(fileName)
  }

  async exec () {
    const command = this.command.join(' ')
    if (this.debugCommand) {
      console.error('>>>', command)
    }
    const r = await exec(command, { env: this.envs })
    if (this.debugOutput) {
      console.error(r)
    }
    return r
  }
}

module.exports = Cli
