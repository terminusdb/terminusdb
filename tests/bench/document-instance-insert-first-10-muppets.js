const fs = require('fs')

const schema = JSON.parse(fs.readFileSync('./json/muppets-schema.json'))
const instance = JSON.parse(fs.readFileSync('./json/muppets-instances-10a.json'))

module.exports = require('./document-instance-insert-first.template.js')(schema, instance)
