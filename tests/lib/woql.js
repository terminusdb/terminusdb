const { expect } = require('chai')

const { Params } = require('./params.js')

function post (agent, path, params) {
  params = new Params(params)
  const body = {}
  body.query = params.object('query')
  body.commit_info = params.object('commit_info')
  params.assertEmpty()

  return agent
    .post(path)
    .send(body)
}

function verifyGetSuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body['api:status']).to.equal('api:success')
  expect(r.body['@type']).to.equal('api:WoqlResponse')
  return r
}

function verifyGetFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:WoqlErrorResponse')
  return r
}

module.exports = {
  post,
  verifyGetSuccess,
  verifyGetFailure,
}
