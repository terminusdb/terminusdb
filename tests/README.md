TerminusDB Integration Test Suite
=================================

This directory contains a Node.js package for running tests written in
JavaScript against the HTTP API of TerminusDB.

These tests check that the API conforms to expectations of TerminusDB behavior
as far as HTTP requests and responses are concerned. These tests are not meant
for evaluating performance.

## Running the test suite

To run the test suite, first make sure you have [installed Node.js and
`npm`][install-nodejs]. Then, change to this directory and build the test suite:

```sh
npm install
```

Next, find the running instance of TerminusDB that you want to test.

1. If you are using a local instance, export these environment variables with
   appropriate values:

   ```sh
   export TERMINUSDB_BASE_URL=...
   export TERMINUSDB_ORG=...
   export TERMINUSDB_USER=...
   export TERMINUSDB_PASS=...
   ```

2. If you are using TerminusX, export these environment variables with
   appropriate values:

   ```sh
   export TERMINUSDB_BASE_URL=...
   export TERMINUSDB_ORG=...
   export TERMINUSDB_ACCESS_TOKEN=...
   ```

Once you have a running instance and the above exports in your shell, run the
test suite:

```sh
npm test
```

To run one or more test files, use:

```sh
npx mocha ./test/ok.js
npx mocha ./test/db*.js
```

## About the test suite

The test suite is built with 3 main support libraries:

* [Chai][chai] is a library for writing assertions.
* [SuperAgent][superagent] is library for constructing HTTP requests and
  inspecting responses.
* [Mocha][mocha] is a testing framework.

Each of these has good documentation to help you understand them and to find the
useful bits you need to work on this test suite.

### Directory structure

* `lib/`: library of JavaScript source files used by the tests but are not tests
* `test/`: Mocha source files with tests
* `package.json`: Node.js package information
* `package-lock.json`: specification of recursive dependency versions
* `.eslintrc.js`: ESLint configuration
* `.mocharc.js`: Mocha configuration

### Checking your JavaScript code

To lint and format JavaScript code, run [ESLint][eslint] with:

```
npm run lint
```

[chai]: https://www.chaijs.com/
[eslint]: https://eslint.org/
[install-nodejs]: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
[mocha]: https://mochajs.org/
[superagent]: https://visionmedia.github.io/superagent/
