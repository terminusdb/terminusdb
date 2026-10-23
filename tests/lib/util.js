// General utility functions

const assert = require('assert')
const crypto = require('crypto')
const fs = require('fs')
const path = require('path')

const defaultContext = {
  '@base': 'terminusdb:///data/',
  '@schema': 'terminusdb:///schema#',
  '@type': '@context',
}

// True when the suite runs against an enterprise server (MOCHA_ENTERPRISE=true,
// exported by the terminusdb-enterprise Makefile). Enterprise routes may
// deliberately differ — e.g. anonymous /api/info returns only the caller's
// authority — so tests branch on this rather than asserting community-only
// behaviour.
const enterprise = process.env.MOCHA_ENTERPRISE === 'true'

function deepClone (object) {
  return JSON.parse(JSON.stringify(object))
}

function isBoolean (val) {
  return typeof val === 'boolean'
}

function isDefined (val) {
  return val !== undefined
}

function isInteger (val) {
  return Number.isInteger(val)
}

function isNonNegativeInteger (val) {
  return Number.isInteger(val) && val >= 0
}

function isString (val) {
  return typeof val === 'string' || val instanceof String
}

function isFunction (val) {
  return typeof val === 'function'
}

function isObject (val) {
  return val instanceof Object
}

function isObjectType (val, type) {
  try {
    return val.constructor.name === type
  } catch (_) {
    return false
  }
}

function isNonEmptyObject (val) {
  return isUndefinedOrNull(val) || Object.keys(val).length === 0
}

function isUndefinedOrNull (val) {
  return val === undefined || val === null
}

function randomString () {
  return crypto.randomBytes(16).toString('hex')
}

function authorizationHeader (agent) {
  const credentials = `${agent.user}:${agent.password}`
  return `Basic ${Buffer.from(credentials).toString('base64')}`
}

function typeString (val) {
  try {
    return val.constructor.name
  } catch (_) {
    return typeof val
  }
}

function firstCapture (val, re) {
  const result = re.exec(val)
  if (result && result[1]) {
    return result[1]
  } else {
    throw new Error(`Failure in firstCapture('${val}', ${re})`)
  }
}

function assertDefined (name, val) {
  assert(isDefined(val), `Missing '${name}': undefined`)
}

function assertBoolean (name, val) {
  assert(
    isBoolean(val),
    `Unexpected type for '${name}'. Expected boolean, got: ${typeString(val)}`,
  )
}

function assertInteger (name, val) {
  assert(
    isInteger(val),
    `Unexpected type for '${name}'. Expected integer, got: ${typeString(val)}`,
  )
}

function assertString (name, val) {
  assert(
    isString(val),
    `Unexpected type for '${name}'. Expected string, got: ${typeString(val)}`,
  )
}

function assertStringOrArray (name, val) {
  assert(
    isString(val) || Array.isArray(val),
    `Unexpected type for '${name}'. Expected string or array, got: ${typeString(val)}`,
  )
}

function assertFunction (name, val) {
  assert(
    isFunction(val),
    `Unexpected type for '${name}'. Expected function, got: ${typeString(val)}`,
  )
}

function assertObject (name, val) {
  assert(
    isObject(val),
    `Unexpected type for '${name}'. Expected object, got: ${typeString(val)}`,
  )
}

function assertArrayOrObject (name, val) {
  assert(
    Array.isArray(val) || isObject(val),
    `Unexpected type for '${name}'. Expected array or object, got: ${typeString(val)}`,
  )
}

/**
 * Returns the correct path to terminusdb.sh based on current working directory.
 * Supports running tests from both repo root and tests directory.
 * @returns {string} Path to terminusdb.sh
 */
function terminusdbScript () {
  // Check if running from tests directory
  if (fs.existsSync('./terminusdb.sh')) {
    return './terminusdb.sh'
  }
  // Check if running from repo root
  if (fs.existsSync('./tests/terminusdb.sh')) {
    return './tests/terminusdb.sh'
  }
  throw new Error('Could not find terminusdb.sh. Run tests from repo root or tests/ directory.')
}

/**
 * Returns the absolute path to test served files.
 * Supports running tests from repo root, tests directory, and containerized environments.
 * @param {string} filename - The file name in the served directory
 * @returns {string} Absolute path to the served file
 */
function servedPath (filename) {
  // Check if running from tests directory
  if (fs.existsSync('./served')) {
    return path.resolve('./served', filename)
  }
  // Check if running from repo root
  if (fs.existsSync('./tests/served')) {
    return path.resolve('./tests/served', filename)
  }
  // Fallback: try to find based on __dirname (works in all environments)
  const utilDir = __dirname // tests/lib/
  const servedDir = path.join(utilDir, '..', 'served')
  if (fs.existsSync(servedDir)) {
    return path.resolve(servedDir, filename)
  }
  throw new Error(`Could not find served directory. Tried: ./served, ./tests/served, and ${servedDir}`)
}

/**
 * Creates a test database storage path that works in both Docker and snap environments.
 * Docker requires relative paths (resolved inside container), snap requires absolute paths
 * to avoid working directory ambiguity.
 * @param {string} [testDir] - Optional test directory path. Defaults to __dirname/..
 * @returns {string} Database path - relative for Docker, absolute for snap/native
 */
function testDbPath () {
  const isDocker = !!process.env.TERMINUSDB_DOCKER_CONTAINER

  if (isDocker) {
    // Docker: use relative path (resolves inside container's /app/terminusdb/tests workdir)
    return `./storage/${randomString()}`
  }
  // Snap/Native/in-container: absolute path on tmpfs where available, so
  // per-command fsync goes to RAM. /dev/shm is tmpfs on Linux; /tmp is not,
  // but still avoids repo checkout I/O. TERMINUSDB_TEST_TMPDIR overrides.
  // Under strict snap confinement /dev/shm exists but mkdir is AppArmor-denied
  // and /tmp is a private namespace, so use the snap's shared common dir.
  let base = process.env.TERMINUSDB_TEST_TMPDIR
  if (!base && (process.env.TERMINUSDB_EXEC_PATH || '').startsWith('/snap/')) {
    base = path.join(process.env.HOME, 'snap', 'terminusdb', 'common')
  }
  if (!base) base = fs.existsSync('/dev/shm') ? '/dev/shm' : '/tmp'
  return path.join(base, 'terminusdb-test', randomString())
}

module.exports = {
  assertArrayOrObject,
  assertBoolean,
  assertDefined,
  assertFunction,
  assertInteger,
  assertObject,
  assertString,
  assertStringOrArray,
  authorizationHeader,
  deepClone,
  defaultContext,
  enterprise,
  firstCapture,
  isBoolean,
  isDefined,
  isFunction,
  isInteger,
  isNonEmptyObject,
  isNonNegativeInteger,
  isObject,
  isObjectType,
  isString,
  isUndefinedOrNull,
  randomString,
  servedPath,
  terminusdbScript,
  testDbPath,
  typeString,
}
