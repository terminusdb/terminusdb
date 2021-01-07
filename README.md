![](https://github.com/terminusdb/terminusdb-web-assets/blob/master/images/main_lockup.png)

# TerminusDB - Making Collaboration Easy

[![Build Status](https://github.com/terminusdb/terminusdb/workflows/Publish/badge.svg?branch=master)](https://travis-ci.com/terminusdb/terminusdb-server) ![Discord](https://img.shields.io/discord/689805612053168129?label=Discord&logo=Discord&style=plastic) ![Docker Pulls](https://img.shields.io/docker/pulls/terminusdb/terminusdb-server?logo=Docker&style=plastic) ![Discourse topics](https://img.shields.io/discourse/topics?color=yellow&logo=Discourse&server=https%3A%2F%2Fdiscuss.terminusdb.com%2F&style=plastic) ![GitHub commit activity](https://img.shields.io/github/commit-activity/m/terminusdb/terminusdb?color=orange&logo=GitHub&style=plastic) ![GitHub](https://img.shields.io/github/license/terminusdb/terminusdb?color=pink&logo=apache&style=plastic) ![GitHub release (latest by date)](https://img.shields.io/github/v/release/terminusdb/terminusdb?color=purple&style=plastic)


TerminusDB is a open source graph database and document store. It is designed for collaboratively building data-intensive applications and knowledge graphs. If you want to collaborate with colleagues or build data-intensive applications, nothing will make you more productive. 

It is a native revision control database that is architecturally similar to Git and other distributed version control systems. 

TerminusDB provides a RESTful API for interacting via the JSON-LD exchange format. You can easily compose applications within your own toolchain which utilise the powerful features of TerminusDB.


![](https://github.com/terminusdb/terminusdb-web-assets/blob/master/images/GitHub.gif)


TerminusDB is a lightweight, high-speed immutable database. Our [delta-encoding](docs/whitepaper/terminusdb.pdf) approach makes possible branch, merge, push, pull, clone, time-travel and other git-like operations on a fully featured database.

## Getting Started

Desktop versions of TerminusDB can be found in our [Download Center](https://terminusdb.com/hub/download)

For development and testing we recommend our [TerminusDB Bootstrap](https://github.com/terminusdb/terminusdb-bootstrap) installer, which uses the Docker image

### Building from source

In order to build from source, follow the [build from source](docs/BUILD.md) directions.

### Command Line

In order to start the terminusdb server or simply to explore a store
from the command line, you can use the [TerminusDB Command Line
Interface](docs/CLI.md).

![](https://github.com/terminusdb/terminusdb-web-assets/blob/master/images/CLI.gif)




Data collaboration is facilitated through [TerminusHub](https://terminusdb.com/hub), which is a freemium SaaS that allows users to manage access to databases and collaboratively work on shared resources. 









## Why

TerminusDB will:

* Give you collaboration features currently available in git, but designed to work well with large datasets
* Make complex data models easy, maintainable and enforced.
* Overcome the Object Impedance mismatch without turning your Database into an incomprehensible soup.
* Allow you to search for repeating patterns using recursion.
* Enable the sharing of data using linked open data formats RDF and JSON-LD making scientific or organisational information sharing easy.
* Branch, clone, merge, time-travel, push and pull all facilitated by our delta-encoding approach
* Help you automate the production of UI and data-entry by *knowing* what data *means*.

## Client

In order to begin manipulating and querying your database you can
start the TerminusDB server (using the CLI) and then simply point your
browser to `"http://SERVER:PORT/"` which should allow you to manage
and query your database.

You can also perform all configuration, querying and management
functions via the RESTful API. We have a number of client libraries to
help you get started:

The [JavaScript client libraries](https://github.com/terminusdb/terminusdb-client) give you
assistance with programatic access.

The [JavaScript dashboard](https://github.com/terminusdb/terminusdb-dashboard) is an example
application allowing manipulation of the database using the javascript client fluent syntax.

The [Python client libraries](https://github.com/terminusdb/terminusdb-client-python) allow
you to use a pythonic syntax to query the database.

## Documentation

In order to use your TerminusDB through the console rather than
directly through clients you should look at the documentation on our
[TerminusDB documentation](https://terminusdb.com/documentation/) website.

## Changes in this Version

[Release Notes](RELEASE_NOTES.md)

## Community

Come visit us on our [discord server](https://discord.gg/yTJKAma)
or our [forum!](https://discuss.terminusdb.com)





