const assert = require('assert')
const superagent = require('superagent')

const { Params } = require('./params.js')
const util = require('./util.js')

// This class encompasses the request agent from SuperAgent as well as some
// agent-specific values that get used a lot in tests.
class Agent {
  constructor (params) {
    params = new Params(params)
    this.baseUrl = params.string('baseUrl', process.env.TERMINUSDB_BASE_URL || 'http://localhost:6363')
    this.orgName = params.string('orgName', process.env.TERMINUSDB_ORG || 'admin')
    this.dbName = params.string('dbName', 'db-' + util.randomString())
    params.assertEmpty()

    // Create a superagent with the base URL as a prefix for requests.
    this.agent = superagent
      .agent()
      // Consider status codes other than server errors as successful.
      // See <https://visionmedia.github.io/superagent/#error-handling>.
      .ok((response) => response.status < 500)
      .use((request) => {
        request.url = this.baseUrl + request.url
      })
      .use(verboseError)
  }

  // Add authentication
  auth () {
    this.userName = process.env.TERMINUSDB_USER
    assert(this.userName, 'Missing environment variable: TERMINUSDB_USER')

    const token = process.env.TERMINUSDB_ACCESS_TOKEN
    if (token) {
      this.agent.use((request) => {
        request.auth(token, { type: 'bearer' })
      })
    } else {
      const pass = process.env.TERMINUSDB_PASS
      assert(pass, 'Missing environment variable: TERMINUSDB_ACCESS_TOKEN or TERMINUSDB_PASS')
      this.agent.use((request) => {
        request.auth(this.userName, pass)
      })
    }
    return this
  }

  defaults () {
    return {
      orgName: this.orgName,
      dbName: this.dbName,
      userName: this.userName,
    }
  }

  head (path) {
    return this.agent.head(path)
  }

  get (path) {
    return this.agent.get(path)
  }

  post (path) {
    return this.agent.post(path)
  }

  put (path) {
    return this.agent.put(path)
  }

  delete (path) {
    return this.agent.delete(path)
  }
}

// This is an agent plugin that prints the request and response to stderr when
// there is an error.
function verboseError (request) {
  request.on('error', (err) => {
    const response = err.response
    if (!response) {
      return
    }

    const contentType = response.header['content-type']
    if (!contentType) {
      return
    }

    err.message += '\n' + response.error.method + ' ' + response.error.path

    const request = response.request

    if (request._header) {
      err.message += '\nRequest headers: ' + JSON.stringify(request._header, null, 2)
    }
    if (request._data) {
      err.message += '\nRequest body: ' + JSON.stringify(request._data, null, 2)
    }

    err.message += '\nResponse status: ' + response.error.status
    err.message += '\nResponse headers: ' + JSON.stringify(response.headers, null, 2)
    if (contentType.startsWith('application/json') && response.body) {
      err.message += '\nResponse body: ' + JSON.stringify(response.body, null, 2)
    }
    if (contentType.startsWith('text') && response.text) {
      err.message += '\nResponse body: ' + response.text
    }
  })
}

module.exports = { Agent }
