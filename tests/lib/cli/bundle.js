const { expect } = require('chai')

const { Params } = require('../params.js')

class Bundle {
  constructor (cli, params) {
    params = new Params({ ...params })
    const orgName = params.string('orgName')
    const dbName = params.string('dbName')
    this.fileName = params.string('fileName', `${orgName}%2f${dbName}.bundle`)

    this.cli = cli.arg('bundle').arg(`${orgName}/${dbName}`)
  }

  then (resolve) {
    resolve(this.cli.exec().then((r) => {
      expect(r.stderr).to.equal('')
      expect(r.stdout).to.equal(
        `Writing to '${this.fileName}'\nBundle successful\n`,
      )
      this.cli.registerFileForCleanup(this.fileName)
      r.fileName = this.fileName
      return r
    }))
  }
}

module.exports = Bundle
