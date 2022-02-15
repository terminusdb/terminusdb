const { expect } = require('chai')

function verifySuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body['@type']).to.equal('api:RemoteResponse')
  expect(r.body['api:status']).to.equal('api:success')
  return r
}

function verifyFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['@type']).to.equal('api:RemoteErrorResponse')
  expect(r.body['api:status']).to.equal('api:failure')
  return r
}

function verifyNotFound (r) {
  expect(r.status).to.equal(404)
  expect(r.body['@type']).to.equal('api:RemoteErrorResponse')
  expect(r.body['api:status']).to.equal('api:not_found')
  return r
}

module.exports = {
  verifySuccess,
  verifyFailure,
  verifyNotFound,
}
