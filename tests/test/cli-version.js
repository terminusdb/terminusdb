const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { info } = require('../lib')

describe('cli-version', function () {
  it('passes string match', async function () {
    const terminusdbVersion = await info.terminusdbVersion()
    const gitHash = await info.gitHash()
    const r = await exec('./terminusdb.sh --version')
    expect(r.stdout).to.match(new RegExp(
      `^TerminusDB v${terminusdbVersion} \\(${gitHash}\\)\nterminusdb-store v.*`,
    ))
  })
})
