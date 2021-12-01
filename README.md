<img src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/images/TerminusDB-Pop2.gif">

## A toolkit for building collaborative applications

[![CI](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml/badge.svg?event=push)](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml)
[![Docker](https://img.shields.io/docker/pulls/terminusdb/terminusdb-server?logo=Docker&style=plastic)](https://hub.docker.com/r/terminusdb/terminusdb-server)
[![Discord](https://img.shields.io/discord/689805612053168129?label=Discord&logo=Discord&style=plastic)](https://discord.gg/yTJKAma)
[![Discourse](https://img.shields.io/discourse/topics?color=yellow&logo=Discourse&server=https%3A%2F%2Fdiscuss.terminusdb.com%2F&style=plastic)](https://discuss.terminusdb.com/)

TerminusDB is an open source graph database and document store. TerminusDB allows you to link JSON documents in a powerful knowledge graph all through a simple [document API](https://terminusdb.com/docs/v10.0/#/reference/reference-document-interface). Use TerminusDB to enable Git-like collaborative workflows in your application.  

<img
  src="https://assets.terminusdb.com/images/tdb-dashboard-tablet.png"
  alt="TerminusDB Dashboard"
  width="50%"
  align="right"
/>
<br />

It is lightweight, flexible, temporal, & immutable:

- **Fast:** it's fast – _really_ fast - build an application MVP in a single sprint! 🚀
- **Customizable:** generate UIs & forms, get data validation for free
- **Schema Support:** tools to build complex data models - extend schema when you want
- **Like Git but for Data:**  versioning-first database you can fork, clone, branch, merge, just like Git
- **Document Oriented:** work with JSON documents.
- **Scaleable:** domain sharding provides serious scale.
- **Easy:** quick to install – start using it in minutes.
<br />


Use TerminusDB to build powerful applications with collaboration at their core. Allow your users to branch and merge their data and to collaborate in a controlled way.

Our [delta-encoding approach](https://github.com/terminusdb/terminusdb/blob/dev/docs/whitepaper/terminusdb.pdf) makes branch, merge, push, pull, clone, time-travel, and other git-like operations possible. TerminusDB versions both data and schema allowing you or your users to **undo** and test before release to deliver a consistent product while continuing to improve and innovate. 

TerminusX is a cloud data platform built on TerminusDB. TerminusX is in **public beta** and you can [sign up now](https://dashboard.terminusdb.com/). If you want to build a data-intensive applications, TerminusDB and TerminusX are right for you.


## [🚀 Installation Guide](https://terminusdb.com/docs/v10.0/#/overviews/get-started)

| <p align="center"><a href="https://hub.docker.com/r/terminusdb/terminusdb-server">🐋 Docker (Universal) | <p align="center"><a href="https://pypi.org/project/terminusdb-client/">🐍 Python  | <p align="center"><a href="https://github.com/terminusdb/terminusdb-client">🌐 JavaScript | <p align="center"><a href="https://terminusdb.com/docs/v10.0/#/install/install-from-source-code">⛵ TerminusX (managed service) |
| --------------------------------------------------------------------- | --------------------------------------------------------------------------------- | --------------------------------------------------------------------------------- |--------------------------------------------------------------------------------- |
| <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/docker.png" /></p>   | <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/python.png" /></p> | <p align="center"><img src="https://assets.terminusdb.com/images/50a6428d99f98e808074cceaf4c755e7.png" width="100" height="100"  /></p> | <p align="center"><img src="https://assets.terminusdb.com/images/favicon.png" width="100" height="100" /></p> |
| `./terminusdb-container run` | `python -m pip install terminusdb-client` | `$ npm install --save @terminusdb/terminusdb-client` | `managed service ftw` |


<hr>


## Quick Links

| [TerminusDB and TerminusX Documentation](https://terminusdb.com/docs/) | [Bootstrap TerminusDB](https://github.com/terminusdb/terminusdb-bootstrap) | [Python Client](https://github.com/terminusdb/terminusdb-client-python) | [JavaScript Client](https://github.com/terminusdb/terminusdb-client-js) | [Quick Start](https://terminusdb.com/docs/v10.0/#/landing/quick-start) |
| :--- | :--- | :--- | :--- | :--- |

<br/><br/>

<img src="https://assets.terminusdb.com/images/terminusdb-github-intro.gif" width="100%" height="auto" />

<br/>
 
Data-centric, domain focused, and decentralized application development.

TerminusDB is a powerful document-oreinted graph database that provides version control features. If Neo4j, MongoDB, and Git had a superpowered baby, that would be TerminusDB. We want to facilitate users in building data-intense applications with collaborative workflows. The JSON format is now ubiqutous in application development but it typically lacks the easy ability to link documents. A document graph opens up many new possibilities for building applications and working with data.  
  
Schema design
  
Data injestion 
  
  
UI Config Dashboard - UI SDK

Authentication and authorisation   Create an access token

Document API 

Rich query language for complexity  Datalog query interface that can be used to express complex joins and recursive graph traversals.

Python client so you can work with those objects and process (fluent python)
  
HTTP

  
Superpowers of application  
  
Power of Undo  
  
Commit graph
<img
  src="https://assets.terminusdb.com/images/terminusdb-commit-graph-diagram.png"
  alt="TerminusDB Dashboard"
  width="50%"
  align="right"
/>
<br />

## Community

Come visit us [Discord](https://discord.gg/yTJKAma)
or our [forum](https://discuss.terminusdb.com). On Twitter, we're [@TerminusDB](https://twitter.com/TerminusDB).
<img align="right" src="https://assets.terminusdb.com/images/TerminusDB%20color%20mascot.png" width="256px"/>

## Documentation

In order to use your TerminusDB through the console rather than
directly through clients, you should look at the documentation on our
[TerminusDB documentation](https://terminusdb.com/docs/terminusdb/) website.


## Changes in this Version

[Release Notes](docs/RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```
