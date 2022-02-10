const { expect } = require('chai')
const { Params } = require('./params.js')

const util = require('./util.js')

function create (agent, path, params) {
  params = new Params(params)
  const comment = params.string('comment', 'default comment')
  const label = params.string('label', 'default label')
  const prefixes = params.object('prefixes')
  const schema = params.boolean('schema')
  params.assertEmpty()

  const body = { comment, label }
  if (util.isDefined(prefixes)) {
    body.prefixes = prefixes
  }
  if (util.isDefined(schema)) {
    body.schema = schema
  }

  return agent
    .post(path)
    .send(body)
}

function del (agent, path) {
  return agent.delete(path)
}

function delUnverified (agent, path) {
  return agent.delete(path).ok((r) => r.status === 200 || r.status === 404)
}

function createAfterDel (agent, path, params) {
  return (async () => {
    await delUnverified(agent, path)
    const r = await create(agent, path, params)
    verifyCreateSuccess(r)
    return r
  })()
}

function verifyCreateSuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body['api:status']).to.equal('api:success')
  expect(r.body['@type']).to.equal('api:DbCreateResponse')
  return r
}

function verifyCreateFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['@type']).to.equal('api:DbCreateErrorResponse')
  expect(r.body['api:status']).to.equal('api:failure')
  return r
}

function verifyDeleteSuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body['api:status']).to.equal('api:success')
  expect(r.body['@type']).to.equal('api:DbDeleteResponse')
  return r
}

function verifyDeleteFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:DbDeleteErrorResponse')
  return r
}

function verifyDeleteNotFound (r) {
  expect(r.status).to.equal(404)
  expect(r.body['api:status']).to.equal('api:not_found')
  expect(r.body['@type']).to.equal('api:DbDeleteErrorResponse')
  return r
}

module.exports = {
  create,
  del,
  delUnverified,
  createAfterDel,
  verifyCreateSuccess,
  verifyCreateFailure,
  verifyDeleteSuccess,
  verifyDeleteFailure,
  verifyDeleteNotFound,
}
