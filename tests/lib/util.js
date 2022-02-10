// General utility functions

const crypto = require('crypto')

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

module.exports = {
  deepClone,
  isBoolean,
  isDefined,
  isInteger,
  isNonNegativeInteger,
  isString,
  isObject,
  randomString,
  isUndefinedOrNull,
  typeString,
}
