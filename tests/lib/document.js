const assert = require('assert')

const api = require('./api')
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

function get (agent, params) {
  params = new Params(params)
  const path = params.string('path', api.path.document(agent, params))
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

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.doc.getSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.doc.getFailure(error)))
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

function getFromCommit (agent, commitId, params) {
  return get(agent, { path: api.path.documentCommit({ ...agent, commitId }, params), ...params })
}

function getFromCommits (agent, params) {
  return get(agent, { path: api.path.documentCommits(agent, params), ...params })
}

function getFromMeta (agent, params) {
  return get(agent, { path: api.path.documentMeta(agent, params), ...params })
}

function getFromSystem (agent, params) {
  return get(agent, { path: api.path.documentSystem(agent, params), ...params })
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

function insert (agent, params) {
  params = new Params(params)
  const path = params.string('path', api.path.document(agent, params))
  const queryString = params.string('queryString')
  const bodyString = params.string('bodyString')
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const fullReplace = params.boolean('fullReplace')
  const schema = params.object('schema')
  const instance = params.object('instance')
  const rawJSON = params.boolean('raw_json')
  const requireMigration = params.boolean('require_migration')
  const allowDestructiveMigration = params.boolean('allow_destructive_migration')
  params.assertEmpty()

  const request = agent.post(path)

  if (util.isDefined(queryString)) {
    request.query(queryString)
  } else {
    request.query({
      graph_type: util.isDefined(schema) ? 'schema' : 'instance',
      author,
      message,
      raw_json: rawJSON,
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

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.doc.insertSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.doc.insertFailure(error)))
    },
    set (header, value) {
      request.set(header, value)
      return this
    },
    serialize (fun) {
      request.serialize(fun)
      return this
    },
    unverified () {
      return request
    },
  }
}

function insertIntoSystem (agent, params) {
  return insert(agent, { path: api.path.documentSystem(agent, params), ...params })
}

function replace (agent, params) {
  params = new Params(params)
  const path = params.string('path', api.path.document(agent, params))
  const queryString = params.string('queryString')
  const bodyString = params.string('bodyString')
  const author = params.string('author', 'default_author')
  const message = params.string('message', 'default_message')
  const schema = params.object('schema')
  const instance = params.object('instance')
  const create = params.boolean('create')
  const requireMigration = params.boolean('require_migration')
  const allowDestructiveMigration = params.boolean('allow_destructive_migration')
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

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.doc.replaceSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.doc.replaceFailure(error)))
    },
    notFound (doc) {
      return request.then(api.response.verify(api.response.doc.replaceNotFound(doc)))
    },
    set (header, value) {
      request.set(header, value)
      return this
    },
    serialize (fun) {
      request.serialize(fun)
      return this
    },
    unverified () {
      return request
    },
  }
}

function delete_ (agent, params) {
  params = new Params(params)
  const path = params.string('path', api.path.document(agent, params))
  const queryString = params.string('queryString')
  const queryParams = new Params(params.object('query'))
  const query = {}
  query.author = queryParams.string('author', 'default_author')
  query.message = queryParams.string('message', 'default_message')
  query.graph_type = queryParams.string('graph_type')
  query.id = queryParams.string('id')
  query.require_migration = queryParams.boolean('require_migration')
  query.allow_destructive_migration = queryParams.boolean('allow_destructive_migration')
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

  return {
    then (resolve) {
      resolve(request.then(api.response.verify(api.response.doc.deleteSuccess)))
    },
    fails (error) {
      return request.then(api.response.verify(api.response.doc.deleteFailure(error)))
    },
    notFound (doc) {
      return request.then(api.response.verify(api.response.doc.deleteNotFound(doc)))
    },
    set (header, value) {
      request.set(header, value)
      return this
    },
    serialize (fun) {
      request.serialize(fun)
      return this
    },
    unverified () {
      return request
    },
  }
}

function deleteFromSystem (agent, params) {
  return delete_(agent, { path: api.path.documentSystem(agent, params), ...params })
}

module.exports = {
  get,
  getFromCommit,
  getFromCommits,
  getFromMeta,
  getFromSystem,
  insert,
  insertIntoSystem,
  replace,
  delete: delete_,
  deleteFromSystem,
}
