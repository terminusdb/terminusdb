const { expect } = require('chai')

function verifyFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:BranchErrorResponse')
  return r
}

module.exports = {
  verifyFailure,
}
