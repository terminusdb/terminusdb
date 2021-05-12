<img src="https://assets.terminusdb.com/images/main_lockup.png" width="700px"/>

# For Data-Intensive Applications

[![Build Status](https://github.com/terminusdb/terminusdb/workflows/Publish/badge.svg?branch=master)](https://github.com/terminusdb/terminusdb/actions) ![Discord](https://img.shields.io/discord/689805612053168129?label=Discord&logo=Discord&style=plastic) ![Docker Pulls](https://img.shields.io/docker/pulls/terminusdb/terminusdb-server?logo=Docker&style=plastic) ![Discourse topics](https://img.shields.io/discourse/topics?color=yellow&logo=Discourse&server=https%3A%2F%2Fdiscuss.terminusdb.com%2F&style=plastic) ![GitHub commit activity](https://img.shields.io/github/commit-activity/m/terminusdb/terminusdb?color=orange&logo=GitHub&style=plastic) ![GitHub](https://img.shields.io/github/license/terminusdb/terminusdb?color=pink&logo=apache&style=plastic) ![GitHub release (latest by date)](https://img.shields.io/github/v/release/terminusdb/terminusdb?color=purple&style=plastic)

<img align="left" src="https://assets.terminusdb.com/images/Laptop.png" width="256px"/>

TerminusDB is an open-source graph database and document store. It is designed for collaboratively building data-intensive applications and knowledge graphs. If you want to collaborate with colleagues or build data-intensive applications, nothing will make you more productive.

It is a native revision control database that is architecturally similar to Git and other distributed version control systems.

TerminusDB provides a RESTful API for interacting via the JSON-LD exchange format. You can easily compose applications within your own toolchain that utilise the powerful features of TerminusDB.

Join us [on Discord](https://discord.gg/ZnmQvap) to hang out and ask questions!


<br/><br/>

![](https://assets.terminusdb.com/images/Git%20for%20GitHub-480p-210108.gif)

<br/>

TerminusDB is a lightweight, high-speed immutable database. Our [delta-encoding](whitepaper/terminusdb.pdf) approach makes branch, merge, push, pull, clone, time-travel, and other git-like operations possible.

Data collaboration is facilitated through [TerminusHub](https://terminusdb.com/hub), which is a freemium service that allows users to manage access to databases and collaboratively work on shared resources.

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

In order to build from the source, follow the [build from source](BUILD.md) directions.

### Open Console

Once you have installed terminusdb-server either from the docker image or built from source you can enter the server location into your browser to view your database from the console "http://SERVER:PORT/". (the default is: https://127.0.0.1:6363/).

### Command Line

In order to start the terminusdb server or simply to explore a store
from the command line, you can use the [TerminusDB Command Line
Interface](CLI.md).

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

Documentation is on our [TerminusDB documentation](https://terminusdb.com/) site.

## Community

Come visit us on our [discord server](https://discord.gg/yTJKAma)
or our [forum](https://discuss.terminusdb.com). We are also on [twitter](https://twitter.com/TerminusDB)
<img align="right" src="https://assets.terminusdb.com/images/TerminusDB%20color%20mascot.png" width="256px"/>

## Changes in this Version

[Release Notes](RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```
