const { expect } = require('chai')

const { Params } = require('../params.js')

class Db {
  constructor (cli) {
    this.cli = cli.arg('db')
  }

  create (params) {
    return new Create(this.cli, params)
  }

  delete (params) {
    return new Delete(this.cli, params)
  }
}

class Create {
  constructor (cli, params) {
    params = new Params({ ...params })
    this.orgName = params.string('orgName')
    this.dbName = params.string('dbName')

    this.cli = cli.arg('create').arg(`${this.orgName}/${this.dbName}`)
  }

  then (resolve) {
    resolve(this.cli.exec().then((r) => {
      expect(r.stdout).to.equal(`Database created: ${this.orgName}/${this.dbName}\n`)
      return r
    }))
  }
}

class Delete {
  constructor (cli, params) {
    params = new Params({ ...params })
    this.orgName = params.string('orgName')
    this.dbName = params.string('dbName')

    this.cli = cli.arg('delete').arg(`${this.orgName}/${this.dbName}`)
  }

  then (resolve) {
    resolve(this.cli.exec().then((r) => {
      expect(r.stdout).to.equal(`Database deleted: ${this.orgName}/${this.dbName}\n`)
      return r
    }))
  }
}

module.exports = Db
