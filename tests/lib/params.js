const assert = require('assert')

const util = require('./util.js')

class Params {
  constructor (params) {
    this.params = params || {}
  }

  // Assert that there are no more parameters.
  assertEmpty () {
    const keys = Object.keys(this.params)
    if (keys.length) {
      throw new Error(`Unexpected parameter keys: ${keys}`)
    }
  }

  // Extract a boolean value.
  boolean (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (val === undefined || val === null) {
      return def
    }

    if (!util.isBoolean(val)) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected boolean, got: ${val.constructor.name || typeof val}`)
    }

    return val
  }

  // Extract a string value.
  string (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (val === undefined || val === null) {
      return def
    }

    if (!util.isString(val)) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected string, got: ${val.constructor.name || typeof val}`)
    }

    return val
  }

  // Extract a string value. Assert if not there.
  stringRequired (key) {
    const val = this.string(key)
    assert(val, `Missing required parameter: '${key}'`)
    return val
  }

  // Extract an object value.
  object (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (val === undefined || val === null) {
      return def
    }

    if (!util.isObject(val)) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected object, got: ${val.constructor.name || typeof val}`)
    }

    return val
  }

  // Extract an object value.
  arrayOrObject (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (val === undefined || val === null) {
      return def
    }

    if (!(Array.isArray(val) || util.isObject(val))) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected array or object, got: ${val.constructor.name || typeof val}`)
    }

    return val
  }
}

module.exports = { Params }
