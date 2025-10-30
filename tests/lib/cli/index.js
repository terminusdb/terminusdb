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
    const testDir = path.join(__dirname, '..', '..')
    const rootDir = path.join(testDir, '..')
    // Use absolute path to avoid snap working directory ambiguity
    const dbPath = path.resolve(testDir, 'storage', util.randomString())
    const terminusdbExec = path.join(rootDir, 'terminusdb')
    this.terminusdbSh = path.join(testDir, 'terminusdb.sh')
    this.envs = {
      ...process.env,
      TERMINUSDB_SERVER_DB_PATH: dbPath,
      // Use existing TERMINUSDB_EXEC_PATH if set (e.g., snap), otherwise default to local binary
      TERMINUSDB_EXEC_PATH: process.env.TERMINUSDB_EXEC_PATH || terminusdbExec,
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
    try {
      const r = await exec(command, { env: this.envs })
      if (this.debugOutput) {
        console.error(r)
      }
      return r
    } catch (error) {
      // Always log stdout/stderr on command failure for debugging
      console.error('❌ Command failed:', command)
      console.error('Exit code:', error.code)
      console.error('STDOUT:', error.stdout || '(empty)')
      console.error('STDERR:', error.stderr || '(empty)')
      console.error('Environment:', {
        TERMINUSDB_EXEC_PATH: this.envs.TERMINUSDB_EXEC_PATH,
        TERMINUSDB_SERVER_DB_PATH: this.envs.TERMINUSDB_SERVER_DB_PATH,
      })
      throw error
    }
  }
}

module.exports = Cli
