## How To Install TerminusDB

Desktop versions of TerminusDB can be found in our [Download Center](https://terminusdb.com/hub/download) 

Find the binary bundled with all the dependencies look there or have a look in the [GitHub Releases](https://github.com/terminusdb/terminusdb/releases)

For development and testing, we recommend our [TerminusDB Bootstrap](https://github.com/terminusdb/terminusdb-bootstrap) installer, which uses the Docker image

### Building from source

In order to build from the source, follow the [build from source](https://github.com/terminusdb/terminusdb/blob/master/docs/BUILD.md) directions.

### Open Console

Once you have installed terminusdb-server either from the docker image or built from the source you can enter the server location into your browser to view your database from the console "http://SERVER:PORT/". (the default is: https://127.0.0.1:6363/).

### Command Line

In order to start the terminusdb server or simply to explore a store
from the command line, you can use the [TerminusDB Command Line
Interface](https://github.com/terminusdb/terminusdb/blob/master/docs/CLI.md).

## Client

In order to begin manipulating and querying your database you can
start the TerminusDB server (using the CLI) and then simply point your
browser to `"http://SERVER:PORT/"` which should allow you to manage
and query your database.

You can also perform all configuration, querying, and management
functions via the RESTful API. We have a number of client libraries to
help you get started:

The [JavaScript client libraries](https://github.com/terminusdb/terminusdb-client) give you
assistance with programmatic access.

The [JavaScript Console](https://github.com/terminusdb/terminusdb-console) allows manipulation 
of the database using the javascript client fluent syntax.

The [Python client libraries](https://github.com/terminusdb/terminusdb-client-python) allows
you to use a pythonic syntax to query the database.

