import { globalIgnores } from 'eslint/config'
import neostandard from 'neostandard'
import mochaPlugin from 'eslint-plugin-mocha'
import json from '@eslint/json'

// Rule was renamed to no-pending-tests; keep the old name resolvable so
// existing eslint-disable directives in test files keep working.
const mocha = {
  ...mochaPlugin,
  rules: {
    ...mochaPlugin.rules,
    'no-skipped-tests': mochaPlugin.rules['no-pending-tests'],
  },
}

export default [
  globalIgnores(['**/.*', '!**/.mocharc.js', '**/storage/']),
  ...neostandard({
    env: ['mocha'],
    globals: {
      open: 'readonly',
      __ENV: 'readonly',
      __ITER: 'readonly',
    },
  }),
  {
    files: ['**/*.js', '**/*.cjs', '**/*.mjs'],
    plugins: { mocha },
    rules: mochaPlugin.configs.recommended.rules,
  },
  {
    files: ['**/*.js', '**/*.cjs', '**/*.mjs'],
    rules: {
      camelcase: ['error', { properties: 'never', ignoreDestructuring: true }],
      '@stylistic/comma-dangle': ['error', 'always-multiline'],
      '@stylistic/quote-props': ['error', 'as-needed', { numbers: true }],
      'prefer-object-spread': 'error',
      // This is for chai. See <https://stackoverflow.com/q/37558795>.
      'no-unused-expressions': 'off',
      'mocha/no-hooks-for-single-case': 'off',
    },
  },
  {
    // Suite is intentionally gated on the SKIP_MEMORY_MODE_TESTS env var.
    files: ['test/memory-mode.js'],
    rules: {
      'mocha/no-conditional-tests': 'off',
    },
  },
  {
    files: ['**/*.json'],
    ignores: ['package-lock.json'],
    language: 'json/json',
    ...json.configs.recommended,
  },
]
