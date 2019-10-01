# Terminus Server

[![Build Status](https://travis-ci.com/terminusdb/terminus-server.svg?branch=master)](https://travis-ci.com/terminusdb/terminus-server)

TerminusDB is an open source model driven graph database for knowledge
graph representation designed specifically for the web-age.

Terminus Server provides TerminusDB with a RESTful API for interacting
with knowledge graphs via the JSON-LD exchange format. This means you
can easily string together applications within your own toolchain
which utilise the powerful features of graph search and graph storage.

## Latest Version 

v1.0.0

## Getting Started

### Install from Docker image

To install a docker image with the latest server (*RECOMMENDED*) follow the instructions 
here: [running the terminus-server from docker](https://github.com/terminusdb/terminus-server/blob/master/DOCKER.md).

### Building from source

In order to build from source, follow the [build from source](https://github.com/terminusdb/terminus-server/blob/master/BUILD.md) directions. 

### Open Dashboard

One you have installed terminus-server either from the docker image or built from source you can go simply enter the server location into your browser to view your database from the dashboard "http://SERVER:PORT/dashboard". (for instance "http://localhost:6363/dashboard").

## Documentation 

### Microservices API

Terminus Server is organised to be _web native_ which means that all
actions and activities are easily automated via a RESTful JSON-LD API.

The Server API is documented [here](https://github.com/terminusdb/terminus-server/blob/master/API.md).

### WOQL 

WOQL, the Web Object Query Language, allows you to seemlessly traverse the graph extracting graph 
fragments, nodes, tables or JSON-LD documents. The syntax itself is in JSON-LD, making the syntax 
a native data-structure in JavaScript, Python, Prolog or any other language which has the 

The Syntax is documented [here](https://github.com/terminusdb/terminus-server/blob/master/SYNTAX.md).

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
simply point your browser to `TERMINUS_DIR/index.html` which should
allow you to manage and query your database.

You can also perform all configuration, querying and management
functions via the RESTful API. We have a number of client libraries to
help you get started:

The [JavaScript client libraries](https://github.com/terminusdb/terminus-client) give you 
assistance with programatic access.

The [JavaScript dashboard](https://github.com/terminusdb/terminus-dashboard) is an example 
application allowing manipulation of the database, queries and .

The [Python client libraries] Coming Soon...

## Changes in this Version 

* Added additional typing information and documentation
* Added database bootstrapping for access permissions and controls using the capabilities ontology
* Added database initialisation utility 
* Added code for instance and schema checking

## Future 

We are eagerly awaiting the release of our new highly scalable lock-free storage layer, 
[terminus-store](https://github.com/terminusdb/terminus-store), which will be integrated 
into Terminus-server in a future release.

## TODO

* Write tests for Document API
* Add optimisations to document expansion in JSON-LD avoiding duplicate expansions

## Authors

Gavin Mendel-Gleason <gavin@datachemist.com>

Matthijs van Otterdijk <matthijs@datachemist.com>

Put your Name here by contributing!

## Copyright

This file is part of TerminusDB.

TerminusDB is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TerminusDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with TerminusDB.  If not, see <https://www.gnu.org/licenses/>.
