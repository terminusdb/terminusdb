const api = require('./api')
const { Params } = require('./params.js')

function get (agent, params) {
  params = new Params(params)
  const path = params.string('path', api.path.remote(agent, params))
  params.assertEmpty()

  const request = agent.get(path)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.remote.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.remote.failure(error)))
    },
    notFound (error) {
      return request.then(api.response.verify(api.response.remote.notFound(error)))
    },
    query (q) {
      request.query(q)
      return this
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

function add (agent, query, params) {
  params = new Params(params)
  const path = params.string('path', api.path.remote(agent, params))
  params.assertEmpty()

  const request = agent.post(path).send(query)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.remote.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.remote.failure(error)))
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

function update (agent, query, params) {
  params = new Params(params)
  const path = params.string('path', api.path.remote(agent, params))
  params.assertEmpty()

  const request = agent.put(path).send(query)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.remote.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.remote.failure(error)))
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

function delete_ (agent, query, params) {
  params = new Params(params)
  const path = params.string('path', api.path.remote(agent, params))
  params.assertEmpty()

  const request = agent.delete(path).query(query).send()

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.remote.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.remote.failure(error)))
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
  get,
  add,
  update,
  delete: delete_,
}
