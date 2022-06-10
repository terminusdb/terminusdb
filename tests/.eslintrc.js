'use strict'

module.exports = {
  env: {
    es2021: true,
  },
  extends: [
    'plugin:mocha/recommended',
    'standard',
  ],
  globals: {
    open: 'readonly',
    __ENV: 'readonly',
    __ITER: 'readonly',
  },
  parserOptions: {
    ecmaVersion: 12,
    sourceType: 'module',
  },
  plugins: [
    'mocha',
    'json-format',
  ],
  rules: {
    camelcase: ['error', { properties: 'never', ignoreDestructuring: true }],
    'comma-dangle': ['error', 'always-multiline'],
    'prefer-object-spread': 'error',
    // This is for chai. See <https://stackoverflow.com/q/37558795>.
    'no-unused-expressions': 'off',
    'mocha/no-hooks-for-single-case': 'off',
  },
}
