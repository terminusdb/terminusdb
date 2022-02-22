<img src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/images/TerminusDB-Pop2.gif">

## A toolkit for building collaborative applications

[![CI](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml/badge.svg?event=push)](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml)
[![Docker](https://img.shields.io/docker/pulls/terminusdb/terminusdb-server?logo=Docker&style=plastic)](https://hub.docker.com/r/terminusdb/terminusdb-server)
[![Discord](https://img.shields.io/discord/689805612053168129?label=Discord&logo=Discord&style=plastic)](https://discord.gg/yTJKAma)
[![Discourse](https://img.shields.io/discourse/topics?color=yellow&logo=Discourse&server=https%3A%2F%2Fdiscuss.terminusdb.com%2F&style=plastic)](https://discuss.terminusdb.com/)

TerminusDB is an open-source graph database and document store. TerminusDB allows you to link JSON documents in a powerful knowledge graph all through a simple [document API](https://terminusdb.com/docs/v10.0/#/reference/reference-document-interface). Use TerminusDB to enable Git-like collaborative workflows in your application.  

<img
  src="https://assets.terminusdb.com/images/tdb-dashboard-tablet.png"
  alt="TerminusDB Dashboard"
  width="50%"
  align="right"
/>
<br />

It is lightweight, flexible, bitemporal, & immutable:

- **Fast:** it's fast – _really_ fast - build an application MVP in a single sprint! 🚀
- **Customizable:** generate UIs & forms, get data validation for free
- **Schema Support:** tools to build complex data models - extend schema when you want
- **Like Git but for Data:**  versioning-first database you can fork, clone, branch, merge, just like Git
- **Document Oriented:** work with JSON documents
- **Scalable:** [domain sharding](https://github.com/terminusdb/terminusdb/discussions/527) provides scale
- **Easy:** quick to install – start using it in minutes
<br />


Use TerminusDB to build powerful applications with collaboration at their core. Allow your users to branch and merge their data and to collaborate in a controlled way.

Our [delta-encoding approach](https://github.com/terminusdb/terminusdb/blob/dev/docs/whitepaper/terminusdb.pdf) makes branch, merge, push, pull, clone, time-travel, and other git-like operations possible. TerminusDB versions both data and schema allowing you or your users to **undo** and test before release to deliver a consistent product while continuing to improve and innovate. 

TerminusX is a cloud data platform built on TerminusDB. TerminusX is in **public beta** and you can [sign up now](https://dashboard.terminusdb.com/). If you want to build a data-intensive applications, TerminusDB and TerminusX are right for you.


## [🚀 Installation Guide](https://terminusdb.com/docs/v10.0/#/overviews/get-started)

| <p align="center"><a href="https://terminusdb.com/docs/index/terminusdb/install/install-as-docker-container">🐋 Docker (Universal) | <p align="center"><a href="https://pypi.org/project/terminusdb-client/">🐍 Python  | <p align="center"><a href="https://github.com/terminusdb/terminusdb-client">🌐 JavaScript | <p align="center"><a href="https://dashboard.terminusdb.com/">⛵ TerminusX (managed service) |
| --------------------------------------------------------------------- | --------------------------------------------------------------------------------- | --------------------------------------------------------------------------------- |--------------------------------------------------------------------------------- |
| <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/docker.png" /></p>   | <p align="center"><img src="https://github.com/Ciphey/Ciphey/raw/master/Pictures_for_README/python.png" /></p> | <p align="center"><img src="https://assets.terminusdb.com/images/50a6428d99f98e808074cceaf4c755e7.png" width="100" height="100"  /></p> | <p align="center"><img src="https://assets.terminusdb.com/images/favicon.png" width="100" height="100" /></p> |
| `docker run --name terminusdb --rm -it  -p 6363:6363 terminusdb.docker.scarf.sh/terminusdb/terminusdb-server:latest` | `python -m pip install terminusdb-client` | `$ npm install --save @terminusdb/terminusdb-client` | `managed service ftw` |


<hr>


## Quick Links

| [TerminusDB and TerminusX Documentation](https://terminusdb.com/docs/) | [Bootstrap TerminusDB](https://github.com/terminusdb/terminusdb-bootstrap) | [Python Client](https://github.com/terminusdb/terminusdb-client-python) | [JavaScript Client](https://github.com/terminusdb/terminusdb-client-js) | [Quick Start](https://terminusdb.com/docs/v10.0/#/landing/quick-start) |
| :--- | :--- | :--- | :--- | :--- |

## Data-Centric Applications  
  
<br/><br/>

  <p align="center">
  <img width="65%" height="auto" src="https://assets.terminusdb.com/images/terminusdb-github-intro.gif">
</p>

<br/>
TerminusDB is a powerful document-oriented graph database that provides version control features. If Neo4j, MongoDB, and Git had a superpowered baby, that would be TerminusDB. We want to facilitate users in building data-intense applications with collaborative workflows. The JSON format is now ubiquitous in application development but other systems - such as MongoDB - lack the easy ability to link documents. A schema-driven document graph opens new possibilities for building applications and working with data.  
<br />
  
Data-centric, domain focused, and decentralized application development.

<br /> 
<br />  
  
  
<img
src="https://assets.terminusdb.com/images/build-collaborative-apps-terminusx.png"
alt="TerminusDB Build App"
width="75%"
align="right"
/>
<br />

- **Schema Design:** build schemas using a simple JSON format in a client or with our visual schema tool  🚀
- **Schema Evolution:** continue to evolve your schema as your application grows 
- **Data Ingestion:** ingest data from anywhere 
- **UI Config:** use our [UI library](https://github.com/terminusdb/terminusdb-documents-ui) to generate a custom UI (under active development)
- **Authentication & Authorization:** simple set up using forward headers
- **HTTP Client:** easily allow your users to interact with your application
- **Rich Query Language:** [Datalog](https://en.wikipedia.org/wiki/Datalog) (WOQL) query interface that can be used to express complex joins and recursive graph traversals
- **Python Client:** process data in TerminusDB python client
- **Fast:** build an application MVP in a single sprint! 🚀
<br />
  

 ## Highlighted Features 
<br />  
<img
  src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/images/like-git-for-data-terminusx-V2.png"
  alt="TerminusDB DWorkflow"
  width="50%"
  align="right"
/>
  
**Workflow** for your users. Allow users to make changes to the data on a separate branch, have those changes approved, and then merge the branch into the production database and application. If there are conflicts, these can be resolved when merging (under active development). 
  
<br />
<br />  
<br />
<br />  
<br />  
  
<img
  src="https://assets.terminusdb.com/images/terminusdb-commit-graph-diagram.png"
  alt="TerminusDB Commit Graph"
  width="50%"
  align="left"
/>
  
Full audit log of changes with the TerminusDB **commit graph**. Track data flows and changes in your system. Power your application with the ability to see who mades specific changes. 
  
Expose this functionality to your users so they can audit their application data with ease. 
 
<br />
<br /> 
<br />
<br /> 

<img
  src="https://assets.terminusdb.com/images/godundo.JPG"
  alt="Power of Undo"
  width="40%"
  align="right"
/>
  
<br /> 
<br />
  
If corrupt or erroneous data is infecting your application, you can immediately roll back to a working version. 

You can allow your users much more freedom to interact with the application safe in the knowledge that you easily roll back to an earlier working version. Immutability provides freedom without trust.
  
<br />
  
<br /> 

  
<br />
  
<br />  

## Community

Come visit us [Discord](https://discord.gg/yTJKAma)
or our [forum](https://discuss.terminusdb.com). On Twitter, we're [@TerminusDB](https://twitter.com/TerminusDB).
<img align="right" src="https://assets.terminusdb.com/images/TerminusDB%20color%20mascot.png" width="256px"/>

## Documentation

Check out our documentation site for more information: [TerminusDB documentation](https://terminusdb.com/docs/terminusdb/) website. 
  
We are working hard to improve our docs - if you see an issue, please open an issue in the [documentation repo](https://github.com/terminusdb/terminusdb-docs). 

## Changes in this Version

[Release Notes](docs/RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```
