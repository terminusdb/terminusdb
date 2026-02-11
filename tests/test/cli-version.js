const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')
const { info, util } = require('../lib')

describe('cli-version', function () {
  it('passes string match (git hash matches binary)', async function () {
    // Skip in snap environment - git hash check only makes sense in dev/build environment
    // Snap packages have no .git directory, and the git hash is baked in at build time,
    // so there's no meaningful verification to perform in packaged environments
    if (process.env.TERMINUSDB_EXEC_PATH && process.env.TERMINUSDB_EXEC_PATH.includes('/snap/')) {
      console.log('  ⊘ Skipping git hash verification - not applicable in packaged snap environment')
      this.skip()
      return
    }

    const terminusdbVersion = await info.terminusdbVersion()
    const gitHash = await info.gitHash()
    const r = await exec(`${util.terminusdbScript()} --version`)
    expect(r.stdout).to.match(new RegExp(
      `^TerminusDB v${terminusdbVersion} \\(${gitHash}\\)\nterminusdb-store v.*`,
    ))
  })
})
