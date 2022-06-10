const fs = require('fs')
const { util } = require('../lib')

module.exports = (n) => {
  // Load the template schema.
  const schemaTemplate = JSON.parse(fs.readFileSync('./json/simple-schema.json'))
  // Build the schemas.
  const schema = []
  for (let i = 0; i < n; i++) {
    // Clone the schema template.
    const s = { ...schemaTemplate }
    // Give it a unique identifier.
    s['@id'] = util.randomString()
    schema.push(s)
  }
  return schema
}
