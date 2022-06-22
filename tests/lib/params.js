const assert = require('assert')

const util = require('./util.js')

// This class provides an abstraction for extracting parameters from an object.
// Each parameter that is found using one of the type-named methods is then
// removed from the object. If you want to check that all parameters have been
// extracted, use `assertEmpty()` to assert that the object has no more keys.
class Params {
  // Wrap an object with `Params`.
  constructor (params) {
    // Clone so that we can assertEmpty() without affecting the original.
    this.params = { ...params }
  }

  // Assert that there are no more parameters.
  assertEmpty () {
    const keys = Object.keys(this.params)
    assert(keys.length === 0, `Unexpected parameters: ${keys}`)
  }

  // Extract a boolean value.
  boolean (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    util.assertBoolean(key, val)

    return val
  }

  // Extract an integer value.
  integer (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    util.assertInteger(key, val)

    return val
  }

  // Extract an integer value. Assert if not there.
  integerRequired (key) {
    const val = this.integer(key)
    util.assertDefined(key, val)
    return val
  }

  // Extract a string value.
  string (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    util.assertString(key, val)

    return val
  }

  // Extract a string value. Assert if not there.
  stringRequired (key) {
    const val = this.string(key)
    util.assertDefined(key, val)
    return val
  }

  // Extract a string or array value as an array value.
  stringOrArray (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    util.assertStringOrArray(key, val)

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

    util.assertObject(key, val)

    return val
  }

  // Extract an object value.
  arrayOrObject (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    util.assertArrayOrObject(key, val)

    return val
  }

  // Extract a function value.
  fun (key, def) {
    const val = this.params[key]
    delete this.params[key]

    if (util.isUndefinedOrNull(val)) {
      return def
    }

    util.assertFunction(key, val)

    return val
  }
}

module.exports = { Params }
