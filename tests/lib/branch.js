const { expect } = require('chai')

function verifySuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body['api:status']).to.equal('api:success')
  expect(r.body['@type']).to.equal('api:BranchResponse')
}

function verifyFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:BranchErrorResponse')
  return r
}

module.exports = {
  verifyFailure,
  verifySuccess,
}
