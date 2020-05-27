
```
████████╗███████╗██████╗ ███╗   ███╗██╗███╗   ██╗██╗   ██╗███████╗
╚══██╔══╝██╔════╝██╔══██╗████╗ ████║██║████╗  ██║██║   ██║██╔════╝
   ██║   █████╗  ██████╔╝██╔████╔██║██║██╔██╗ ██║██║   ██║███████╗
   ██║   ██╔══╝  ██╔══██╗██║╚██╔╝██║██║██║╚██╗██║██║   ██║╚════██║
   ██║   ███████╗██║  ██║██║ ╚═╝ ██║██║██║ ╚████║╚██████╔╝███████║
   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚══════╝

```

# Terminus Server

[![Build Status](https://travis-ci.com/terminusdb/terminus-server.svg?branch=master)](https://travis-ci.com/terminusdb/terminus-server)

TerminusDB is an open source model driven graph database for knowledge
graph representation designed specifically for the web-age.

Terminus Server provides TerminusDB with a RESTful API for interacting
with knowledge graphs via the JSON-LD exchange format. This means you
can easily compose applications within your own toolchain which
utilise the powerful features of graph search and graph storage.

We use an advanced git-like model storing append only changes to
graphs represented in concise datastructures using
[terminus-store](https://github.com/terminusdb/terminus-store). You
can read a description of the architecture in our [Succinct Data Structures and Delta Encoding for Modern Databases](https://github.com/terminusdb/terminusdb-knowledge/blob/master/succinct-data-structures-whitepaper/terminusdb.pdf)
whitepaper.

## Latest Version

v1.1.6

## Getting Started

### Install from Docker image (*RECOMMENDED*)

For all users, we recommend our [Quickstart](https://github.com/terminusdb/terminus-quickstart) installer.

Follow the instructions there. If you choose to run the Docker image directly, please not that
the default username is "admin" and the password is "root" (without the double quotes) and that you should change it in a
production environment.

### Building from source

In order to build from source, follow the [build from source](https://github.com/terminusdb/terminus-server/blob/master/docs/BUILD.md) directions.

### Open Console

Once you have installed terminus-server either from the docker image or built from source you can go simply enter the server location into your browser to view your database from the dashboard "http://SERVER:PORT/console". (the default is: [http://localhost:6363/console](http://localhost:6363/console)).

## Why

TerminusDB will:

* Make complex data models easy, maintainable and enforced.
* Overcome the Object Impedance mismatch without turning your Database into an incomprehensible soup.
* Allow you to search for repeating patterns using recursion.
* Give you powerful temporal queries using finite domain constraint logic.
* Enable the sharing of data using linked open data formats RDF and JSON-LD making scientific or organisational information sharing easy.
* Help you automate the production of UI and data-entry by *knowing* what data *means*.

## Client

In order to begin manipulating and querying your database you can
simply point your browser to `TERMINUS_SERVER/console` which should
allow you to manage and query your database.

You can also perform all configuration, querying and management
functions via the RESTful API. We have a number of client libraries to
help you get started:

The [JavaScript client libraries](https://github.com/terminusdb/terminus-client) give you
assistance with programatic access.

The [JavaScript dashboard](https://github.com/terminusdb/terminus-dashboard) is an example
application allowing manipulation of the database using the javascript client fluent syntax.

The [Python client libraries](https://github.com/terminusdb/terminus-client-python) allow
you to use a pythonic syntax to query the database.

## Documentation

In order to use your TerminusDB through the console rather than
directly through clients you should look at the documentation on our
[TerminusDB documentation](https://terminusdb.com/docs/) website.

## Changes in this Version

* Now with our own [terminus-store](https://github.com/terminusdb/terminus-store) backend!
* Extended WOQL to greater functionality for string and arithmetic processing
* Better date handling in WOQL
* Improved type casting in WOQL
* Improvements to query processing speed
* We now have decoupled public URI from internal database URI

## TODO

* Improve error handling and logging
* Convert frames into valid JSON-LD

## Contact

You can write the authors, or connect to us on FreeNode IRC on the `#TerminusDB` channel.

## Authors

Gavin Mendel-Gleason <gavin@datachemist.com>

Matthijs van Otterdijk <matthijs@datachemist.com>

Robin de Rooij <robin@datachemist.com>

Anne Ogborn <annie@datachemist.com>

Dmytri Kleiner <dk@datachemist.com>

[Paulo Moura](https://github.com/pmoura)

Put your Name here by contributing!

## Copyright

This file is part of TerminusDB.

TerminusDB is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License version 3 (GPLv3) as published by
the Free Software Foundation.

TerminusDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.
