const { util } = require('../lib')

const n = util.firstCapture(__filename, /document-instance-replace-first-(\d+)-simple/)

const [schema, instance] = require('./document-instance-build-n-simple.template.js')(n)
module.exports = require('./document-instance-replace-first.template.js')(schema, instance)
