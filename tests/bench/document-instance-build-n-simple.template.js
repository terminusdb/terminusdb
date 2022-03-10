const fs = require('fs')
const { util } = require('../lib')

module.exports = (n) => {
  // Load the schema.
  const schema = JSON.parse(fs.readFileSync('./json/simple-schema.json'))
  // Build the instances.
  const instance = []
  for (let i = 0; i < n; i++) {
    instance.push({
      '@type': schema['@id'],
      '@id': schema['@id'] + '/' + util.randomString(),
      name: util.randomString(),
    })
  }
  return [schema, instance]
}
