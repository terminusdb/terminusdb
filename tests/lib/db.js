const api = require('./api')
const { Params } = require('./params.js')
const util = require('./util.js')

function create (agent, params) {
  params = new Params(params)
  const bodyString = params.string('bodyString')
  // Clients use an empty comment string by default, so we do that here.
  const comment = params.string('comment', '')
  const label = params.string('label', 'default label')
  const path = params.string('path', api.path.db(agent, params))
  const prefixes = params.object('prefixes')
  const schema = params.boolean('schema')
  params.assertEmpty()

  const request = agent.post(path)

  if (bodyString) {
    request.type('json').send(bodyString)
  } else {
    const body = { comment, label }
    if (util.isDefined(prefixes)) {
      body.prefixes = prefixes
    }
    if (util.isDefined(schema)) {
      body.schema = schema
    }
    request.send(body)
  }

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.db.createSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.db.createFailure(error)))
    },
    notFound (error) {
      return request.then(api.response.verify(api.response.db.createNotFound(error)))
    },
    set (header, value) {
      request.set(header, value)
      return this
    },
    unverified () {
      return request
    },
  }
}

function exists (agent, params) {
  params = new Params(params)
  const path = params.string('path', api.path.db(agent, params))
  const query = params.object('query', { exists: true })
  params.assertEmpty()

  const request = agent.head(path).query(query)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.db.existsSuccess)))
    },
    fails () {
      return request.then(api.response.verify(api.response.db.existsFailure))
    },
    notFound () {
      return request.then(api.response.verify(api.response.db.existsNotFound))
    },
    unverified () {
      return request
    },
  }
}

function delete_ (agent, params) {
  params = new Params(params)
  const path = params.string('path', api.path.db(agent, params))
  params.assertEmpty()

  const request = agent.delete(path)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.db.deleteSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.db.deleteFailure(error)))
    },
    notFound (error) {
      return request.then(api.response.verify(api.response.db.deleteNotFound(error)))
    },
    set (header, value) {
      request.set(header, value)
      return this
    },
    unverified () {
      return request
    },
  }
}

module.exports = {
  create,
  exists,
  delete: delete_,
}
