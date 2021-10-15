const assert = require('assert')
const { expect } = require('chai')

const { Params } = require('./params.js')
const util = require('./util.js')

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

function verifyInsertSuccess (r) {
  expect(r.status).to.equal(200)
  expect(r.body).to.be.an('array')

  // Verify that the `@id` values in the request equal those in the response.
  if (Array.isArray(r.request._data)) {
    expect(r.body.length).to.equal(r.request._data.length)

    for (let i = 0; i < r.body.length; i++) {
      const id = r.request._data[i]['@id']
      if (id) {
        expect(r.body[i]).to.equal(id)
      }
    }
  } else if (util.isObject(r.request._data)) {
    expect(r.body.length).to.equal(1)

    const id = r.request._data['@id']
    if (id) {
      expect(r.body[0]).to.equal(id)
    }
  }
}

function verifyInsertFailure (r) {
  expect(r.status).to.equal(400)
  expect(r.body['@type']).to.equal('api:InsertDocumentErrorResponse')
  expect(r.body['api:status']).to.equal('api:failure')
}

module.exports = {
  insert,
  verifyInsertSuccess,
  verifyInsertFailure,
}
