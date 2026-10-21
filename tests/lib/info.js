const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)

async function terminusdbVersion () {
  const path = require('path')
  const configPath = path.join(__dirname, '../../src/config/terminus_config.pl')
  const content = await fs.readFile(configPath, 'utf8')
  return content.match(/^terminusdb_version\('(.*)'\)/m)[1]
}

async function gitHash () {
  // Tests running inside a container have no .git; the workflow passes the
  // tested commit as TERMINUSDB_GIT_HASH.
  if (process.env.TERMINUSDB_GIT_HASH) {
    return process.env.TERMINUSDB_GIT_HASH
  }
  const { stdout: result } = await exec('git rev-parse --verify HEAD')
  return result.toString().trim()
}

module.exports = {
  gitHash,
  terminusdbVersion,
}
