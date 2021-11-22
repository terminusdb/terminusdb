<img src="https://assets.terminusdb.com/images/TerminusDB-Pop.gif">

## A toolkit for building collaborative applications

[![CI](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml/badge.svg?event=push)](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml)
[![Docker](https://img.shields.io/docker/pulls/terminusdb/terminusdb-server?logo=Docker&style=plastic)](https://hub.docker.com/r/terminusdb/terminusdb-server)
[![Discord](https://img.shields.io/discord/689805612053168129?label=Discord&logo=Discord&style=plastic)](https://discord.gg/yTJKAma)
[![Discourse](https://img.shields.io/discourse/topics?color=yellow&logo=Discourse&server=https%3A%2F%2Fdiscuss.terminusdb.com%2F&style=plastic)](https://discuss.terminusdb.com/)

TerminusDB is an open source graph database and document store. It is lightweight, temporal, & immutable. TerminusDB allows you to link JSON documents in a powerful knowledge graph all through a simple [document API](https://terminusdb.com/docs/v10.0/#/reference/reference-document-interface). 

Use TerminusDB to build powerful applications with collaboration at thier core. Allow your users to branch and merge their data and to collaborate in a controlled way.

TerminusX is a cloud data platform built on TerminusDB. TerminusX is in **public beta** and you can [sign up now](https://dashboard.terminusdb.com/).

<img src="https://assets.terminusdb.com/images/tdb-dashboard-tablet.png" width="100%" height="auto" />

Our delta-encoding approach makes branch, merge, push, pull, clone, time-travel, and other git-like operations possible. TerminusDB versions both data and schema allowing your team to deliver a consistent product to others while continuing to improve and innovate. 

If you want to build a data-intensive applications, TerminusDB and TerminusX are right for you.


## [Installation Guide](https://terminusdb.com/docs/v10.0/#/overviews/get-started)

| <p align="center"><a href="https://pypi.org/project/terminusdb-client/">🐍 Python | <p align="center"><a href="https://hub.docker.com/r/terminusdb/terminusdb-server">🐋 Docker (Universal) | <p align="center"><a href="https://github.com/terminusdb/terminusdb-client">🌐 JavaScript (macOS) | <p align="center"><a href="https://formulae.brew.sh/formula/ciphey">🍺 Homebrew (macOS/Linux) |
| --------------------------------------------------------------------- | --------------------------------------------------------------------------------- | --------------------------------------------------------------------------------- |--------------------------------------------------------------------------------- |
| <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/python.png" /></p>    | <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/docker.png" /></p> | <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/macports.png" /></p> | <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/homebrew.png" /></p> |
| `python3 -m pip install ciphey --upgrade` | `docker run -it --rm remnux/ciphey` | `sudo port install ciphey` | `brew install ciphey` |

| Linux                                                                                                                   | Mac OS                                                                                                                     | Windows                                                                                                                   |
| ----------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| ![GitHub Workflow Status](https://img.shields.io/github/workflow/status/ciphey/ciphey/Python%20application?label=Linux) | ![GitHub Workflow Status](https://img.shields.io/github/workflow/status/ciphey/ciphey/Python%20application?label=Mac%20OS) | ![GitHub Workflow Status](https://img.shields.io/github/workflow/status/ciphey/ciphey/Python%20application?label=Windows) |

<hr>


## Quick Links

| [TerminusDB and TerminusX Documentation](https://terminusdb.com/docs/) | [Bootstrap TerminusDB](https://github.com/terminusdb/terminusdb-bootstrap) | [Python Client](https://github.com/terminusdb/terminusdb-client-python) | [JavaScript Client](https://github.com/terminusdb/terminusdb-client-js) |
| :--- | :--- | :--- | :--- |

<br/><br/>

<img src="https://assets.terminusdb.com/images/terminusdb-github-intro.gif" width="100%" height="auto" />

<br/>

TerminusDB is a lightweight, high-speed immutable database. Our [delta-encoding](https://assets.terminusdb.com/research/succinct-data-structures-and-delta-encoding.pdf) approach makes branch, merge, push, pull, clone, time-travel, and other git-like operations possible.

**Data collaboration is facilitated through [TerminusX](https://terminusdb.com)**, which is a cloud self-service data platform enabling you to build, deploy, execute, monitor, and share versioned data products.


## Why TerminusDB/TerminusX

* Get started in minutes. Work using your preferred language, or use our low-code console to get your database running in just a few minutes.
* Add, replace and query documents using our document interface.
* Powerful query allows you to search for repeating patterns using recursion.
* Generate forms, get data validation, and a flexible & surfable document interface (in the console).
* Visual tool to build complex data models, which are easy, maintainable, and enforced.
* Data science/data engineering functionality directly in the database and via TerminusX.
* Versioning as a first class citizen. Commit graphs and data lineage out of the box.
* Clone the production database and associated schema to branch, merge, collaborate, and time travel.
* New paradigm in data-centric, domain focused, and decentralized data product development.


## Community

Come visit us [Discord](https://discord.gg/yTJKAma)
or our [forum](https://discuss.terminusdb.com). On Twitter, we're [@TerminusDB](https://twitter.com/TerminusDB).
<img align="right" src="https://assets.terminusdb.com/images/TerminusDB%20color%20mascot.png" width="256px"/>

## Changes in this Version

[Release Notes](docs/RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```

## Why TerminusDB


* Get started in minutes. Whatever language you're using or if you prefer our low-code console, TerminusDB is easy - you can have your database running in just a few minutes. And nothing will make you more productive
* Powerful query allows you to search for repeating patterns using recursion
* Generate forms, get data validation, and a flexible & surfable document interface (in the console)
* Visual tool to build complex data models, which are easy, maintainable, and enforced
* Data science/data engineering functionality directly in the database and via TerminusHub
* Clone the production database, branch, merge, collaboration, lineage, versioning, and time travel - Git-like revision control for large databases
* New paradigm in data-centric application development


## Getting Started

Desktop versions of TerminusDB can be found in our [Download Center](https://terminusdb.com/hub/download).

For development and testing, we recommend our [TerminusDB Bootstrap](https://github.com/terminusdb/terminusdb-bootstrap) installer, which uses the Docker image.

### Building from source

In order to build from the source, follow the [build from source](docs/BUILD.md) directions.

### Open Console

Once you have installed terminusdb-server either from the docker image or built from source you can enter the server location into your browser to view your database from the console "http://SERVER:PORT/". (the default is: https://127.0.0.1:6363/).

### Command Line

In order to start the terminusdb server or simply to explore a store
from the command line, you can use the [TerminusDB Command Line
Interface](docs/CLI.md).

<img src="https://assets.terminusdb.com/images/cli-github.gif" width="500px"/>

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

## Documentation

In order to use your TerminusDB through the console rather than
directly through clients, you should look at the documentation on our
[TerminusDB documentation](https://terminusdb.com/docs/terminusdb/) website.

## Community

Come visit us on our [discord server](https://discord.gg/yTJKAma)
or our [forum](https://discuss.terminusdb.com). We are also on [twitter](https://twitter.com/TerminusDB)
<img align="right" src="https://assets.terminusdb.com/images/TerminusDB%20color%20mascot.png" width="256px"/>

## Changes in this Version

[Release Notes](docs/RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```
