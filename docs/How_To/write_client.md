# How To Get Started Writing a Client

Writing a client for TerminusDB is relatively easy compared to a lot of other databases. The TerminusDB
API is able to communicate in JSON-LD. Any language which is able to serialize data structures into
JSON communicate over HTTP can be used to write a client.

This guide will mainly list the prerequisites for building a client rather than building
a complete client yourself, as we can't possibly cover every language or framework on earth.

## Starting out: pick a language

Be sure that there is no existing client for the language or framework that you are
looking for. So far, there is a Python and JavaScript client.

Any language should do for making a client as long as it supports JSON serialization and HTTP.

## Looking at the JSON-LD documentation

Be sure to check out the JSON-LD [reference documentation](https://github.com/terminusdb/terminusdb/blob/master/docs/reference/WOQL_JSON.md). It contains
the JSON-LD specification that needs to be implemented. Another way to discover the used JSON-LD
of the queries is by inspecting the console requests when you fire off queries yourself.

## API Endpoints

A good TerminusDB client should be able to communicate with the various API endpoints that exist in TerminusDB.
The different API endpoints can be found in [the API reference docs](https://github.com/terminusdb/terminusdb/blob/master/docs/reference/API.md).


## Example clients

* https://github.com/terminusdb/terminusdb-client-js
* https://github.com/terminusdb/terminusdb-client-python/
