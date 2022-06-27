const exec = require('util').promisify(require('child_process').exec)
const { expect } = require('chai')

describe('cli-options', function () {
  it('nice error for bad switch', async function () {
    const r = await exec('./terminusdb.sh db create admin/bar -g foo')
    const errorPattern = /ERROR: The command line option "g" does not exist for the command "db create".*/
    expect(r.stderr).to.match(new RegExp(errorPattern))
  })

  it('nice error for bad type', async function () {
    const r = await exec('./terminusdb.sh db create admin/bar --public=foo')
    const errorPattern = /ERROR: flag 'public': expected atom parsable as boolean, found 'foo'.*/
    expect(r.stdout).to.match(new RegExp(errorPattern))
  })

})
