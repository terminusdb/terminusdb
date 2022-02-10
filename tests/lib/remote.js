const { expect } = require('chai')

function verifySuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body['@type']).to.equal('api:RemoteResponse')
  expect(r.body['api:status']).to.equal('api:success')
  return r
}

module.exports = {
  verifySuccess,
}
