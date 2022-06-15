// General utility functions

const assert = require('assert')
const crypto = require('crypto')

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

module.exports = {
  assertArrayOrObject,
  assertBoolean,
  assertDefined,
  assertFunction,
  assertInteger,
  assertObject,
  assertString,
  assertStringOrArray,
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
  typeString,
}
