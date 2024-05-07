const api = require('./api')
const { Params } = require('./params.js')
const util = require('./util.js')

function post (agent, query, params) {
  params = new Params(params)
  const path = params.string('path', api.path.woqlResource(agent, params))
  const bodyString = params.string('bodyString')
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const commitInfo = params.object('commitInfo', { author, message })
  params.assertEmpty()

  const request = agent.post(path)

  if (util.isDefined(bodyString)) {
    request.type('json').send(bodyString)
  } else {
    util.assertObject('query', query)
    request.send({ commit_info: commitInfo, query, optimize: true })
  }

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.woql.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.woql.failure(error)))
    },
    notFound (error) {
      return request.then(api.response.verify(api.response.woql.notFound(error)))
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

function multipart (agent, query, params) {
  params = new Params(params)
  const path = params.string('path', api.path.woqlResource(agent, params))
  let bodyString = params.string('bodyString')
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const commitInfo = params.object('commitInfo', { author, message })
  params.assertEmpty()

  if (!util.isDefined(bodyString)) {
    util.assertObject('query', query)
    bodyString = JSON.stringify({ commit_info: commitInfo, query })
  }

  const request = agent
    .post(path)
    .attach(
      'payload',
      Buffer.from(bodyString),
      { filename: 'body.json', contentType: 'application/json' },
    )

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.woql.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.woql.failure(error)))
    },
    notFound (error) {
      return request.then(api.response.verify(api.response.woql.notFound(error)))
    },
    set (header, value) {
      request.set(header, value)
      return this
    },
    attach (name, file, options) {
      request.attach(name, file, options)
      return this
    },
    unverified () {
      return request
    },
  }
}

module.exports = {
  post,
  multipart,
}
