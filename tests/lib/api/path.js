// Each of these functions contructs a path for an endpoint.

const { Params } = require('../params.js')

module.exports = {
  branch (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/branch/${orgName}/${dbName}`
  },
  branchTarget (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    const remoteName = params.string('remoteName', 'local')
    const branchName = params.string('branchName', 'main')
    return `/api/branch/${orgName}/${dbName}/${remoteName}/branch/${branchName}`
  },
  branchOrigin (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    const remoteName = params.string('remoteName', 'local')
    const branchName = params.string('branchName', 'main')
    return `/${orgName}/${dbName}/${remoteName}/branch/${branchName}`
  },
  db (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/db/${orgName}/${dbName}`
  },
  document (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/document/${orgName}/${dbName}`
  },
  documentCommit (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    const commitId = params.stringRequired('commitId')
    const remoteName = params.string('remoteName', 'local')
    return `/api/document/${orgName}/${dbName}/${remoteName}/commit/${commitId}`
  },
  documentCommits (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    const remoteName = params.string('remoteName', 'local')
    return `/api/document/${orgName}/${dbName}/${remoteName}/_commits`
  },
  documentMeta (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/document/${orgName}/${dbName}/_meta`
  },
  documentSystem () {
    return '/api/document/_system'
  },
  frameSystem () {
    return '/api/schema/_system'
  },
  triples (resource) {
    return `/api/triples/${resource}`
  },
  triplesBranch (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    const remoteName = params.string('remoteName', 'local')
    const branchName = params.string('branchName', 'main')
    const graph = params.string('graph', 'instance')
    return `/api/triples/${orgName}/${dbName}/${remoteName}/branch/${branchName}/${graph}`
  },
  triplesSystem () {
    return '/api/triples/_system/schema'
  },
  remote (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/remote/${orgName}/${dbName}`
  },
  diff () {
    return '/api/diff'
  },
  versionDiff (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/diff/${orgName}/${dbName}`
  },
  organization () {
    return '/api/organization'
  },
  organizationName (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    return `/api/organization/${orgName}`
  },
  patch () {
    return '/api/patch'
  },
  graphQL (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/graphql/${orgName}/${dbName}`
  },
  prefixes (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/prefixes/${orgName}/${dbName}`
  },
  woqlResource (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    return `/api/woql/${orgName}/${dbName}`
  },
  apply (params, extra) {
    params = new Params({ ...params, ...extra })
    const orgName = params.stringRequired('orgName')
    const dbName = params.stringRequired('dbName')
    const branchName = params.string('branchName', 'main')
    const remoteName = params.string('remoteName', 'local')
    return `/api/apply/${orgName}/${dbName}/${remoteName}/branch/${branchName}`
  },
}
