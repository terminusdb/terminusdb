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

function branchNew (params, newBranchName) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  const remoteName = params.string('remoteName', 'local')
  const branchName = params.string('branchName', 'main')
  const newOrgName = params.string('newOrgName', orgName)
  const newDbName = params.string('newDbName', dbName)
  const newRemoteName = params.string('newRemoteName', remoteName)
  return {
    path: `/api/branch/${newOrgName}/${newDbName}/${newRemoteName}/branch/${newBranchName}`,
    origin: `/${orgName}/${dbName}/${remoteName}/branch/${branchName}`,
    orgName,
    dbName,
    remoteName,
    newOrgName,
    newDbName,
    newRemoteName,
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

function documentCommit (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  const commitId = params.stringRequired('commitId')
  const remoteName = params.string('remoteName', 'local')
  return {
    path: `/api/document/${orgName}/${dbName}/${remoteName}/commit/${commitId}`,
    orgName: orgName,
    dbName: dbName,
    remoteName: remoteName,
    commitId: commitId,
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

function frameSystem (params) {
  return {
    path: '/api/schema/_system',
  }
}

function triples (resource) {
  return {
    path: `/api/triples/${resource}`,
  }
}

function triplesBranch (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  const remoteName = params.string('remoteName', 'local')
  const branchName = params.string('branchName', 'main')
  const graph = params.string('graph', 'instance')
  return {
    path: `/api/triples/${orgName}/${dbName}/${remoteName}/branch/${branchName}/${graph}`,
    orgName,
    dbName,
    remoteName,
    branchName,
    graph,
  }
}

function triplesSystem () {
  return {
    path: '/api/triples/_system/schema',
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

function versionDiff (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/diff/${orgName}/${dbName}`,
    orgName: orgName,
    dbName: dbName,
  }
}

function patch (params) {
  params = new Params(params)
  return {
    path: '/api/patch',
  }
}

function prefixes (params) {
  params = new Params(params)
  const orgName = params.stringRequired('orgName')
  const dbName = params.stringRequired('dbName')
  return {
    path: `/api/prefixes/${orgName}/${dbName}`,
    orgName: orgName,
    dbName: dbName,
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
  branchNew,
  db,
  diff,
  versionDiff,
  document,
  documentCommit,
  documentCommits,
  documentMeta,
  documentSystem,
  frameSystem,
  patch,
  prefixes,
  remote,
  triples,
  triplesBranch,
  triplesSystem,
  woqlResource,
}
