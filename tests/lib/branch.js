const api = require('./api')
const { Params } = require('./params.js')
const util = require('./util.js')

function create (agent, branchName, params) {
  params = new Params(params)
  const path = params.string('path', api.path.branchTarget({ ...agent, branchName }, params))
  const origin = params.string('origin')
  const prefixes = params.object('prefixes')
  params.assertEmpty()

  const body = { }
  if (util.isDefined(origin)) {
    body.origin = origin
  }
  if (util.isDefined(prefixes)) {
    body.prefixes = prefixes
  }

  const request = agent.post(path).send(body)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.branch.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.branch.failure(error)))
    },
    unverified () {
      return request
    },
  }
}

function delete_ (agent, branchName, params) {
  params = new Params(params)
  const path = params.string('path', api.path.branchTarget({ ...agent, branchName }, params))
  params.assertEmpty()

  const request = agent.delete(path)

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.branch.success)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.branch.failure(error)))
    },
    unverified () {
      return request
    },
  }
}

module.exports = {
  create,
  delete: delete_,
}
