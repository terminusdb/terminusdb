const fs = require('fs')

const schema = JSON.parse(fs.readFileSync('./json/muppets-schema.json'))
const a = JSON.parse(fs.readFileSync('./json/muppets-instances-10a.json'))
const b = JSON.parse(fs.readFileSync('./json/muppets-instances-10b.json'))
const instance = a.concat(b)

module.exports = require('./document-instance-replace-first.template.js')(schema, instance)
