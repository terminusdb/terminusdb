<img
  src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/readmes/terminusdb/TerminusDB-Logo-Colour_3.png"
  alt="TerminusDB Logo"
  width="30%"
  align="center"
/>
---

[![Native Build (macos, ubuntu)](https://github.com/terminusdb/terminusdb/actions/workflows/native-build.yml/badge.svg)](https://github.com/terminusdb/terminusdb/actions/workflows/native-build.yml)
[![Docker Build](https://github.com/terminusdb/terminusdb/actions/workflows/docker-build.yml/badge.svg?branch=main)](https://github.com/terminusdb/terminusdb/actions/workflows/docker-build.yml)
[![Issues](https://img.shields.io/github/issues/terminusdb/terminusdb)](https://github.com/terminusdb/terminusdb/issues)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

<!--
[![CI](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml/badge.svg?event=push)](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml) 
-->

## TerminusDB is a distributed database with a collaboration model.

It is designed to be like git, but for data. If you find this project useful, please consider **starring the repo**.

The building blocks of the model are:

  - Revision Control: commits for every update
  - Diff: differences between commits can be interpreted as patches between states
  - Push/Pull/Clone: communicate diffs between nodes using push / pull / clone
  - Query: You can query any state of the database at any commit.

TerminusDB allows you to link JSON documents in a knowledge graph through a [document API](https://terminusdb.org/docs/document-insertion).

Note that documentation is now maintained at [https://terminusdb.org/docs/](https://terminusdb.org/docs/) and is no longer available at terminusdb.com, which is currently experiencing issues.

### TerminusDB Version 11

[TerminusDB 11](https://github.com/terminusdb/terminusdb/releases/tag/v11.0.0) features a new Rustified storage backend that reduces storage overhead and latency, improves search performance, and simplifies interchange. TerminusDB 11 also comes with some exciting features to make building easier and faster -

- [GraphQL](https://terminusdb.org/docs/graphql-basics) - Use GraphQL as a proper graph query language with deep link discover and path queries
- Added `@unfoldable` document flag to frames - Making data curation easier by unfolding subdocuments within a frame to add all relevant data in one place
- Add `@metadata` to frames - Include additional metadata to document frames including data formatted as Markdown.


## Installation Guide

[![Get it from the Snap Store](https://snapcraft.io/static/images/badges/en/snap-store-black.svg)](https://snapcraft.io/terminusdb)

The easiest way to install TerminusDB as a developer is by using the [Snap](https://snapcraft.io/terminusdb). It does not provide a daemon and is mainly intended for developers that want to try TerminusDB.

For deployments, or if you want to use the local dashboard:

1. Add the following to a .env file in the source directory:

```shell
OPENAI_KEY=YOUR_OPENAI_KEY_HERE
# And optionally specify number of pages for the vector database
# for instance
BUFFER_AMOUNT=120000
```

The OPENAI_KEY is not mandatory to use, but without it, the AI indexing will not work. Of course, all the document graph database functionality will still work as intended.

2. `docker compose up`

You should be able to view TerminusDB running by default at `localhost:6363`

> If you're installing TerminusDB on Windows with Docker, our friends at DFRNT wrote this [comprehensive guide](https://dfrnt.com/blog/2023-02-25-run-terminusdb-on-windows-with-docker/).

You can also install TerminusDB from the [Source Code](https://terminusdb.org/docs/install-terminusdb-from-source-code).


## TerminusDB CLI

A simple example creating a person with friends can be created as follows:

```shell
terminusdb db create admin/example1
terminusdb doc insert --graph_type=schema admin/example1 <<EOF
{ "@id" : "Person",
  "@type" : "Class",
  "name" : "xsd:string",
  "occupation" : "xsd:string",
  "friends" : { "@type" : "Set",
                "@class" : "Person" }}
EOF
terminusdb doc insert admin/example1 --message='adding Gavin' <<EOF
{ "@type" : "Person","name" : "Gavin", "occupation" : "Coder"}
EOF
```

## Community

Come visit us on [Discord](https://discord.gg/yTJKAma)

## Documentation

Check out our documentation site for more information: [TerminusDB documentation](https://terminusdb.org/docs/get-started-with-terminusdb/) website.

We are working hard to improve our docs - if you see an issue, please open an issue in the [documentation repo](https://github.com/terminusdb/terminusdb-docs).

White paper on our [delta-encoding approach](https://terminusdb.org/blog/succinct-data-structures-for-modern-databases/) to data management.

Check the [Python Client](https://pypi.org/project/terminusdb-client/) 🐍. Or the [JavaScript Client](https://github.com/terminusdb/terminusdb-client) 🌐.

WOQL is a powerful query language that allows you to express complex patterns over arbitrary data structures concisely. What makes it so expressive and easy to use is the [radical simplicity of the core underlying concepts](https://terminusdb.org/blog/the-power-of-web-object-query-language/).

## Changes in this Version

[Release Notes](docs/RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```
