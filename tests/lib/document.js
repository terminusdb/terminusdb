const assert = require('assert')
const { expect } = require('chai')

const { Params } = require('./params.js')
const util = require('./util.js')

function get (agent, path, params) {
  params = new Params(params)
  const query = {}
  query.graph_type = params.string('graph_type')
  query.type = params.string('type')
  query.id = params.string('id')
  params.assertEmpty()

  const request = agent
    .get(path)
    .query(query)

  return request
}

function insert (agent, path, params) {
  params = new Params(params)
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const schema = params.object('schema')
  const instance = params.object('instance')
  params.assertEmpty()

  assert(
    !(schema && instance),
    'Both \'schema\' and \'instance\' parameters found. Only one allowed.',
  )

  const schemaOrInstance = schema || instance
  assert(
    schemaOrInstance,
    'Missing \'schema\' or \'instance\' parameter. One is required.',
  )
  const graphType = schema ? 'schema' : 'instance'

  const request = agent
    .post(path)
    .query({
      graph_type: graphType,
      author: author,
      message: message,
    })
    .send(schemaOrInstance)

  return request
}

function replace (agent, path, params) {
  params = new Params(params)
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const schema = params.object('schema')
  const instance = params.object('instance')
  const create = params.string('create', 'false')
  params.assertEmpty()

  assert(
    !(schema && instance),
    'Both \'schema\' and \'instance\' parameters found. Only one allowed.',
  )

  const schemaOrInstance = schema || instance
  assert(
    schemaOrInstance,
    'Missing \'schema\' or \'instance\' parameter. One is required.',
  )
  const graphType = schema ? 'schema' : 'instance'

  const request = agent
    .put(path)
    .query({
      graph_type: graphType,
      author: author,
      message: message,
      create: create,
    })
    .send(schemaOrInstance)

  return request
}

function del (agent, path, params) {
  params = new Params(params)
  const query = {}
  query.author = params.string('author', 'default_author')
  query.message = params.string('message', 'default_message')
  query.graph_type = params.string('graph_type')
  query.id = params.string('id')
  params.assertEmpty()

  const request = agent
    .delete(path)
    .query(query)

  return request
}

// Verify that, if a request includes an `@id`, that value is the suffix of the
// value in the response.
function verifyId (requestId, responseId) {
  if (requestId) {
    // The request ID may contain regular expression symbols, so we escape them
    // according to:
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#escaping
    requestId = requestId.replace(/[.*+?^${}()|[\]\\]/g, '\\$&') + '$'
    expect(responseId).to.match(new RegExp(requestId))
  }
}

function verifyGetSuccess (r) {
  expect(r.status).to.equal(200)
  return r
}

function verifyInsertSuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body).to.be.an('array')

  // Verify the `@id` values are the ones expected.
  if (Array.isArray(r.request._data)) {
    expect(r.body.length).to.equal(r.request._data.length)

    for (let i = 0; i < r.body.length; i++) {
      verifyId(r.request._data[i]['@id'], r.body[i])
    }
  } else if (util.isObject(r.request._data)) {
    expect(r.body.length).to.equal(1)
    verifyId(r.request._data['@id'], r.body[0])
  }
  return r
}

function verifyInsertFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:InsertDocumentErrorResponse')
  return r
}

function verifyReplaceFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:ReplaceDocumentErrorResponse')
  return r
}

function verifyDelSuccess (r) {
  expect(r.status).to.equal(200)
  return r
}

module.exports = {
  get,
  insert,
  replace,
  del,
  verifyGetSuccess,
  verifyInsertSuccess,
  verifyInsertFailure,
  verifyReplaceFailure,
  verifyDelSuccess,
}
