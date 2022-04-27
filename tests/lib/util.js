// General utility functions

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

function isObject (val) {
  return val instanceof Object
}

function isNonEmptyObject (val) {
  return isUndefinedOrNull(val) || Object.keys(val).length === 0
}

function isUndefinedOrNull (val) {
  return val === undefined || val === null
}

function randomString () {
  return crypto.randomBytes(3).toString('hex')
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

module.exports = {
  deepClone,
  defaultContext,
  firstCapture,
  isBoolean,
  isDefined,
  isInteger,
  isNonEmptyObject,
  isNonNegativeInteger,
  isObject,
  isString,
  isUndefinedOrNull,
  randomString,
  typeString,
}
