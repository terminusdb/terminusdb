const assert = require('assert')

const util = require('./util.js')

// This class provides an abstraction for extracting parameters from an object.
// Each parameter that is found using one of the type-named methods is then
// removed from the object. If you want to check that all parameters have been
// extracted, use `assertEmpty()` to assert that the object has no more keys.
class Params {
  // Wrap an object with `Params`.
  constructor (params) {
    // Clone with Object.assign() so that we can later use assertEmpty() without
    // actually changing the params passed in.
    this.params = Object.assign({}, params || {})
  }

  // Assert that there are no more parameters.
  assertEmpty () {
    const keys = Object.keys(this.params)
    if (keys.length) {
      throw new Error(`Unexpected parameters: ${keys}`)
    }
  }

  // Extract a boolean value.
  boolean (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    if (!util.isBoolean(val)) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected boolean, got: ${val.constructor.name || typeof val}`)
    }

    return val
  }

  // Extract an integer value.
  integer (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    if (!util.isInteger(val)) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected integer, got: ${val.constructor.name || typeof val}`)
    }

    return val
  }

  // Extract a string value.
  string (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    if (!util.isString(val)) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected string, got: ${util.typeString(val)}`)
    }

    return val
  }

  // Extract a string value. Assert if not there.
  stringRequired (key) {
    const val = this.string(key)
    assert(util.isDefined(val), `Missing required parameter: '${key}'`)
    return val
  }

  // Extract a string or array value as an array value.
  stringOrArray (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    if (!(util.isString(val) || Array.isArray(val))) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected string or array, got: ${util.typeString(val)}`)
    }
    if (Array.isArray(val)) {
      return val
    } else {
      return [val]
    }
  }

  // Extract an object value.
  object (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    if (!util.isObject(val)) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected object, got: ${util.typeString(val)}`)
    }

    return val
  }

  // Extract an object value.
  arrayOrObject (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    if (!(Array.isArray(val) || util.isObject(val))) {
      throw new Error(`Unexpected type for parameter '${key}'. Expected array or object, got: ${util.typeString(val)}`)
    }

    return val
  }
}

module.exports = { Params }
