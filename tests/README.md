TerminusDB Integration Tests and Benchmarks
===========================================

This directory contains a Node.js package for running integration tests and
benchmarks written in JavaScript against the HTTP API of TerminusDB.

The **integration tests** check that the API conforms to expectations of
TerminusDB behavior as far as HTTP requests and responses are concerned.

We use the **benchmarks** to measure the time for API requests to help with
optimization and preventing performance regressions.

## Getting started

1. [Install Node.js and `npm`][install-nodejs].

2. Change to this directory and download all dependencies:

   ```sh
   npm install
   ```

3. Export these environment variables:

   ```sh
   export TERMINUSDB_USER=admin
   export TERMINUSDB_PASS=root
   ```

4. Run a local instance of the TerminusDB server. If the URL is different from
   the default, export this environment variable:

   ```sh
   export TERMINUSDB_BASE_URL=...
   ```

## Integration tests

### Running

Run the integration tests with:

```sh
npm test
```

To run one or more test files, use:

```sh
npx mocha ./test/ok.js
npx mocha ./test/db*.js
```

### More information

The tests are built with 3 main support libraries:

* [Chai][chai] is a library for writing assertions.
* [SuperAgent][superagent] is library for constructing HTTP requests and
  inspecting responses.
* [Mocha][mocha] is a testing framework.

Each of these has good documentation to help you understand them and to find the
useful bits you need to work on the tests.

## Benchmarks

### Running

Run the benchmarks with:

```sh
npm run bench
```

To run one or more benchmark files, use:

```sh
npm run bench ./bench/db-create.js
npm run bench ./bench/document-instance-insert-first-*-simple.js
```

### More information

The benchmarks are built around one support library:

* [Buffalo-Bench][buffalo-bench]

The Buffalo-Bench `README.md` and the existing benchmarks should help you get
started working on the benchmarks.

Note that file names ending with `.template.js` are ignored by the benchmark
runner [`bench.js`](./bench.js). Use these files to share code common to
multiple benchmarks.

## Directory structure

* `lib/`: library of JavaScript source files used by the tests and benchmarks
* `test/`: test files run by Mocha
* `test-served/`: test files run by Mocha with `http-server`
* `served/`: files served by `http-server`
* `bench/`: benchmark files run by [`bench.js`](./bench.js)
* `bench.js`: benchmark file runner
* `package.json`: Node.js package information
* `package-lock.json`: specification of recursive dependency versions
* `.eslintrc.js`: ESLint configuration
* `.mocharc.js`: Mocha configuration

### Checking your JavaScript code

To lint and format JavaScript code, run [ESLint][eslint] with:

```
npm run lint
```

[buffalo-bench]: https://github.com/Masquerade-Circus/buffalo-bench
[chai]: https://www.chaijs.com/
[eslint]: https://eslint.org/
[install-nodejs]: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
[mocha]: https://mochajs.org/
[superagent]: https://visionmedia.github.io/superagent/
