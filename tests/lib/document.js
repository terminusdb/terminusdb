const assert = require('assert')
const { expect } = require('chai')

const { Params } = require('./params.js')
const util = require('./util.js')

function commonGetParams (params) {
  const result = {}
  result.graph_type = params.string('graph_type')
  result.as_list = params.boolean('as_list')
  result.type = params.string('type')
  result.id = params.string('id')
  result.count = params.integer('count')
  result.skip = params.integer('skip')
  result.compress_ids = params.boolean('compress_ids')
  result.prefixed = params.boolean('prefixed')
  params.assertEmpty()
  return result
}

function get (agent, path, params) {
  params = new Params(params)
  const queryString = params.string('queryString')
  const query = commonGetParams(new Params(params.object('query')))
  const bodyString = params.string('bodyString')
  const bodyParams = new Params(params.object('body'))
  const docQuery = bodyParams.object('query')
  const body = commonGetParams(bodyParams)
  params.assertEmpty()

  // This is not a parameter common to both the query string and body, so we add
  // it back in.
  if (docQuery) {
    body.query = docQuery
  }

  const request = agent.get(path)
  if (queryString) {
    request.query(queryString)
  } else if (Object.keys(query).length) {
    request.query(query)
  }
  if (bodyString) {
    request.type('json').send(bodyString)
  } else if (Object.keys(body).length) {
    request.send(body)
  }
  return request
}

function schemaOrInstance (schema, instance) {
  assert(
    !(schema && instance),
    'Both \'schema\' and \'instance\' parameters found. Only one allowed.',
  )
  const result = schema || instance
  assert(
    result,
    'Missing \'schema\' or \'instance\' parameter. One is required.',
  )
  return result
}

function insert (agent, path, params) {
  params = new Params(params)
  const queryString = params.string('queryString')
  const bodyString = params.string('bodyString')
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const fullReplace = params.boolean('fullReplace')
  const schema = params.object('schema')
  const instance = params.object('instance')
  const json = params.boolean('json')
  params.assertEmpty()

  const request = agent.post(path)

  if (util.isDefined(queryString)) {
    request.query(queryString)
  } else {
    request.query({
      graph_type: util.isDefined(schema) ? 'schema' : 'instance',
      author,
      message,
      json,
    })
  }

  if (util.isDefined(fullReplace)) {
    request.query({ full_replace: fullReplace })
  }

  if (util.isDefined(bodyString)) {
    request.type('json').send(bodyString)
  } else {
    request.send(schemaOrInstance(schema, instance))
  }

  return request
}

function replace (agent, path, params) {
  params = new Params(params)
  const queryString = params.string('queryString')
  const bodyString = params.string('bodyString')
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const schema = params.object('schema')
  const instance = params.object('instance')
  const create = params.boolean('create')
  params.assertEmpty()

  const request = agent.put(path)

  if (util.isDefined(queryString)) {
    request.query(queryString)
  } else {
    request.query({
      graph_type: schema ? 'schema' : 'instance',
      author,
      message,
    })
    if (util.isDefined(create)) {
      request.query({ create })
    }
  }

  if (util.isDefined(bodyString)) {
    request.type('json').send(bodyString)
  } else {
    request.send(schemaOrInstance(schema, instance))
  }

  return request
}

function del (agent, path, params) {
  params = new Params(params)
  const queryString = params.string('queryString')
  const queryParams = new Params(params.object('query'))
  const query = {}
  query.author = queryParams.string('author', 'default_author')
  query.message = queryParams.string('message', 'default_message')
  query.graph_type = queryParams.string('graph_type')
  query.id = queryParams.string('id')
  queryParams.assertEmpty()
  const bodyString = params.string('bodyString')
  const body = params.stringOrArray('body')
  params.assertEmpty()

  const request = agent.delete(path)
  if (util.isDefined(queryString)) {
    request.query(queryString)
  } else {
    request.query(query)
  }
  if (util.isDefined(bodyString)) {
    request.type('json').send(bodyString)
  } else if (util.isDefined(body)) {
    request.send(body)
  }
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

function verifyGetFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:GetDocumentErrorResponse')
  return r
}

function verifyInsertSuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body).to.be.an('array')

  // Verify the `@id` values are the ones expected.
  if (Array.isArray(r.request._data)) {
    let data = r.request._data

    // Support fullReplace with @context as the first element.
    if (data.length > 0 && data[0]['@type'] === '@context') {
      data = data.slice(1)
    }

    expect(r.body.length).to.equal(data.length)

    for (let i = 0; i < r.body.length; i++) {
      verifyId(data[i]['@id'], r.body[i])
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

const verifyReplaceSuccess = verifyInsertSuccess

function verifyReplaceFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['api:status']).to.equal('api:failure')
  expect(r.body['@type']).to.equal('api:ReplaceDocumentErrorResponse')
  return r
}

function verifyReplaceNotFound (r) {
  expect(r.status).to.equal(404)
  expect(r.body['api:status']).to.equal('api:not_found')
  expect(r.body['@type']).to.equal('api:ReplaceDocumentErrorResponse')
  return r
}

function verifyDelSuccess (r) {
  expect(r.status).to.equal(200)
  return r
}

function verifyDelNotFound (r) {
  expect(r.status).to.equal(404)
  expect(r.body['api:status']).to.equal('api:not_found')
  expect(r.body['@type']).to.equal('api:DeleteDocumentErrorResponse')
  return r
}

function expectMissingField (r, field, object) {
  expect(r.body['api:error']['@type']).to.equal('api:MissingField')
  expect(r.body['api:error']['api:field']).to.equal(field)
  expect(r.body['api:error']['api:document']).to.deep.equal(object)
  return r
}

function expectMissingParameter (r, param) {
  expect(r.body['api:error']['@type']).to.equal('api:MissingParameter')
  expect(r.body['api:error']['api:parameter']).to.equal(param)
  return r
}

module.exports = {
  get,
  insert,
  replace,
  del,
  verifyGetSuccess,
  verifyGetFailure,
  verifyInsertSuccess,
  verifyInsertFailure,
  verifyReplaceSuccess,
  verifyReplaceFailure,
  verifyReplaceNotFound,
  verifyDelSuccess,
  verifyDelNotFound,
  expectMissingField,
  expectMissingParameter,
}
