const { expect } = require('chai')

const { Params } = require('../params.js')
const util = require('../util.js')

class Doc {
  constructor (cli) {
    this.cli = cli.arg('doc')
  }

  insert (params, data, extra) {
    return new Insert(this.cli, params, data, extra)
  }

  get (params, extra) {
    return new Get(this.cli, params, extra)
  }
}

class Get {
  constructor (cli, params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.string('orgName')
    const dbName = params.string('dbName')
    this.graphType = params.string('graphType')

    this.cli = cli.arg('get').arg(`${orgName}/${dbName}`)
    if (util.isDefined(this.graphType)) {
      this.cli.arg(`--graph_type=${this.graphType}`)
    }
  }

  then (resolve) {
    resolve(this.cli.exec().then((r) => {
      expect(r.stderr).to.equal('')
      return r.stdout.trimEnd().split('\n').map(JSON.parse)
    }))
  }
}

class Insert {
  constructor (cli, params, data, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.string('orgName')
    const dbName = params.string('dbName')
    const graphType = params.string('graphType')

    this.data = data

    this.cli = cli.arg('insert').arg(`${orgName}/${dbName}`)
    if (util.isDefined(graphType)) {
      this.cli.arg(`--graph-type=${graphType}`)
    }
    if (util.isDefined(this.data)) {
      this.cli.arg(`--data='${JSON.stringify(this.data)}'`)
    }
  }

  then (resolve) {
    resolve(this.cli.exec().then((r) => {
      expect(r.stderr).to.equal('')
      expect(r.stdout).to.match(/^Documents inserted:/)
      const lineCount = r.stdout.trimEnd().split('\n').length
      let expectedLineCount = 1
      if (util.isObject(this.data)) {
        expectedLineCount++
      } else if (util.isArray(this.data)) {
        expectedLineCount += this.data.length
      }
      expect(lineCount, 'number of documents').to.equal(expectedLineCount)
      return r
    }))
  }
}

module.exports = Doc
