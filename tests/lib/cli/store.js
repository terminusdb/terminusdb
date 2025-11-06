const { expect } = require('chai')

const { Params } = require('../params.js')

class Store {
  constructor (cli) {
    this.cli = cli.arg('store')
  }

  init (params) {
    return new Init(this.cli, params)
  }
}

class Init {
  constructor (cli, params) {
    params = new Params({ ...params })
    const force = params.boolean('force', true)

    this.cli = cli.arg('init')
    if (force) {
      this.cli.arg('--force')
    }
  }

  then (resolve) {
    resolve(this.cli.exec().then((r) => {
      expect(r.stderr).to.equal('')
      expect(r.stdout).to.equal('Successfully initialised database.\n')
      this.cli.registerFileForCleanup(this.cli.dbPath)
      return r
    }))
  }
}

module.exports = Store
