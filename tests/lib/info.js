const fs = require('fs/promises')
const exec = require('util').promisify(require('child_process').exec)

async function terminusdbVersion () {
  const content = await fs.readFile('../src/config/terminus_config.pl', 'utf8')
  return content.match(/^terminusdb_version\('(.*)'\)/m)[1]
}

async function gitHash () {
  const { stdout: result } = await exec('git rev-parse --verify HEAD')
  return result.toString().trim()
}

module.exports = {
  gitHash,
  terminusdbVersion,
}
