![](terminusdb.png)

# TerminusDB Server

[![Build Status](https://travis-ci.com/terminusdb/terminusdb-server.svg?branch=master)](https://travis-ci.com/terminusdb/terminusdb-server)

TerminusDB is an open source model driven RDF graph database for
knowledge graph representation designed specifically for the web-age.

TerminusDB Server provides TerminusDB with a RESTful API for interacting
with knowledge graphs via the JSON-LD exchange format. This means you
can easily compose applications within your own toolchain which
utilise the powerful features of graph search and graph storage.

We use an advanced git-like model, storing append only changes to
graphs represented in succinct datastructures using
[terminusdb-store](https://github.com/terminusdb/terminusdb-store). You
can read a description of the architecture in our [Succinct Data Structures and Delta Encoding for Modern Databases](docs/whitepaper/terminusdb.pdf)
whitepaper.

TerminusDB's *delta-encoding* approach makes possible branch, merge,
push, pull, clone, time-travel and other git-like operations on a fully
featured graph database.

## Latest Version

v2.0.2

## Getting Started

### Install from Docker image (*RECOMMENDED*)

For all users, we recommend our [Quickstart](https://github.com/terminusdb/terminusdb-quickstart) installer.

Follow the instructions there. If you choose to run the Docker image directly, please note that
the default username is "admin" and the password is "root" (without the double quotes) and that you should change it in a
production environment.

### Building from source

In order to build from source, follow the [build from source](docs/BUILD.md) directions.

### Open Console

Once you have installed terminusdb-server either from the docker image or built from source you can go simply enter the server location into your browser to view your database from the console "http://SERVER:PORT/console". (the default is: [http://localhost:6363/console](http://localhost:6363/console)).

## Why

TerminusDB will:

* Make complex data models easy, maintainable and enforced.
* Overcome the Object Impedance mismatch without turning your Database into an incomprehensible soup.
* Allow you to search for repeating patterns using recursion.
* Enable the sharing of data using linked open data formats RDF and JSON-LD making scientific or organisational information sharing easy.
* Branch, clone, merge, time-travel, push and pull all facilitated by our delta-encoding approach
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

[Release Notes](RELEASE_NOTES.md)

## Contact

Come visit us on our [discord server](https://discord.gg/yTJKAma)
or our [forum!](https://discuss.terminusdb.com)

## Authors

Gavin Mendel-Gleason <gavin@terminusdb.com>

Matthijs van Otterdijk <matthijs@terminusdb.com>

Robin de Rooij <robin@terminusdb.com>

Anne Ogborn <anne@swi-prolog.org>

Dmytri Kleiner <dk@terminusdb.com>

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
