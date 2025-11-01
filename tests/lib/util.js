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
function testDbPath (testDir) {
  const isDocker = !!process.env.TERMINUSDB_DOCKER_CONTAINER
  const dir = testDir || path.join(__dirname, '..')

  if (isDocker) {
    // Docker: use relative path (resolves inside container's /app/terminusdb/tests workdir)
    return `./storage/${randomString()}`
  } else {
    // Snap/Native: use absolute path to avoid working directory ambiguity
    return path.resolve(dir, 'storage', randomString())
  }
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
