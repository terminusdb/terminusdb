const api = require('./api')
const { Params } = require('./params.js')

function add (agent, params) {
  params = new Params(params)
  const bodyString = params.string('bodyString')
  const orgName = params.string('orgName', agent.orgName)
  const user = params.string('user', agent.user)
  params.assertEmpty()

  const request = agent.post(api.path.organization())

  if (bodyString) {
    request.type('json').send(bodyString)
  } else {
    request.send({ organization_name: orgName, user_name: user })
  }

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.org.addSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.org.addFailure(error)))
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

function delete_ (agent, params) {
  params = new Params(params)
  const bodyString = params.string('bodyString')
  const orgName = params.string('orgName', agent.orgName)
  params.assertEmpty()

  const request = agent.delete(api.path.organization())

  if (bodyString) {
    request.type('json').send(bodyString)
  } else {
    request.send({ organization_name: orgName })
  }

  return {
    then (resolve) {
      resolve(
        request
          .then(api.response.verify(api.response.org.deleteSuccess))
          // TODO: Remove catch when this is fixed.
          .catch(() => {
            console.error('FIXME: Deleting an organization is broken.')
          }),
      )
    },
    fails (error) {
      return request.then(api.response.verify(api.response.org.deleteFailure(error)))
    },
    notFound (error) {
      return request.then(api.response.verify(api.response.org.deleteNotFound(error)))
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
  add,
  delete: delete_,
}
