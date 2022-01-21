// Each of these functions contructs a path for an endpoint.

const { Params } = require('./params.js')

function branch (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/branch/${orgName}/${dbName}`,
    orgName: orgName,
    dbName: dbName,
  }
}

function db (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/db/${orgName}/${dbName}`,
    orgName: orgName,
    dbName: dbName,
  }
}

function document (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/document/${orgName}/${dbName}`,
    orgName: orgName,
    dbName: dbName,
  }
}

function documentCommits (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  const remoteName = params.string('remoteName', 'local')
  return {
    path: `/api/document/${orgName}/${dbName}/${remoteName}/_commits`,
    orgName: orgName,
    dbName: dbName,
    remoteName: remoteName,
  }
}

function documentMeta (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/document/${orgName}/${dbName}/_meta`,
    orgName: orgName,
    dbName: dbName,
  }
}

function documentSystem () {
  return {
    path: '/api/document/_system',
  }
}

function remote (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/remote/${orgName}/${dbName}`,
    orgName: orgName,
    dbName: dbName,
  }
}

function diff (params) {
  params = new Params(params)
  return {
    path: '/api/diff',
  }
}

function patch (params) {
  params = new Params(params)
  return {
    path: '/api/patch',
  }
}

function woqlResource (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/woql/${orgName}/${dbName}`,
    orgName: orgName,
    dbName: dbName,
  }
}

module.exports = {
  branch,
  db,
  diff,
  document,
  documentCommits,
  documentMeta,
  documentSystem,
  patch,
  remote,
  woqlResource,
}
