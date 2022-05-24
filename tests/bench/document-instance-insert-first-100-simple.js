const { util } = require('../lib')

const n = util.firstCapture(__filename, /document-instance-insert-first-(\d+)-simple/)

const [schema, instance] = require('./document-instance-build-n-simple.template.js')(n)
module.exports = require('./document-instance-insert-first.template.js')(schema, instance)
