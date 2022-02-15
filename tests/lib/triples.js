const { expect } = require('chai')

function verifyInsertSuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body['api:status']).to.equal('api:success')
  expect(r.body['@type']).to.equal('api:TriplesInsertResponse')
  return r
}

function verifyFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:TriplesErrorResponse')
  return r
}

module.exports = {
  verifyInsertSuccess,
  verifyFailure,
}
