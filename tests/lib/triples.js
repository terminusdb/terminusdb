const assert = require('assert')

const api = require('./api')
const { Params } = require('./params.js')
const util = require('./util.js')

function get (agent, params) {
  params = new Params(params)
  const descriptor = params.string('descriptor')
  const path = params.string(
    'path',
    util.isDefined(descriptor) ? api.path.triples(descriptor) : undefined,
  )
  params.assertEmpty()

  util.assertDefined('descriptor or path', path)

  const request = agent.get(path)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.triples.getSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.triples.failure(error)))
    },
    unverified () {
      return request
    },
  }
}

function getFromBranch (agent, params) {
  const path = api.path.triplesBranch(agent, params)
  return get(agent, { path, ...params })
}

function getFromSystem (agent, params) {
  const path = api.path.triplesSystem()
  return get(agent, { path, ...params })
}

function insert (agent, path, turtle, params) {
  params = new Params(params)
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')

  assert(
    util.isString(turtle),
    `Unexpected type for 'turtle'. Expected string, got: ${util.typeString(turtle)}`,
  )

  const body = { commit_info: { author, message }, turtle }

  const request = agent.put(path).send(body)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.triples.insertSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.triples.failure(error)))
    },
    unverified () {
      return request
    },
  }
}

function replace (agent, path, turtle, params) {
  params = new Params(params)
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')

  assert(
    util.isString(turtle),
    `Unexpected type for 'turtle'. Expected string, got: ${util.typeString(turtle)}`,
  )

  const body = { commit_info: { author, message }, turtle }

  const request = agent.post(path).send(body)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.triples.updateSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.triples.failure(error)))
    },
    unverified () {
      return request
    },
  }
}

function insertIntoBranch (agent, turtle, params) {
  return insert(agent, api.path.triplesBranch(agent, params), turtle, params)
}

function replaceIntoBranch (agent, turtle, params) {
  return replace(agent, api.path.triplesBranch(agent, params), turtle, params)
}

module.exports = {
  get,
  getFromBranch,
  getFromSystem,
  insert,
  replaceIntoBranch,
  insertIntoBranch,
}
