const { expect } = require('chai')

const { Params } = require('../params.js')

class Unbundle {
  constructor (cli, params) {
    params = new Params({ ...params })
    const orgName = params.string('orgName')
    const dbName = params.string('dbName')
    this.fileName = params.string('fileName', `${orgName}%2f${dbName}.bundle`)

    this.cli = cli.arg('unbundle').arg(`${orgName}/${dbName}`).arg(this.fileName)
  }

  then (resolve) {
    resolve(this.cli.exec().then((r) => {
      expect(r.stderr).to.equal('')
      return r
    }))
  }
}

module.exports = Unbundle
