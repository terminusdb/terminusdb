const { util } = require('../lib')

const n = util.firstCapture(__filename, /document-schema-insert-first-(\d+)-simple/)

const schema = require('./document-schema-build-n-simple.template.js')(n)
module.exports = require('./document-schema-insert-first.template.js')(schema)
