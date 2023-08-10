<img
  src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/readmes/terminusdb/TerminusDB-Logo-Colour_3.png"
  alt="TerminusDB Logo"
  width="30%"
  align="center"
/>

[![CI](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml/badge.svg?event=push)](https://github.com/terminusdb/terminusdb/actions/workflows/ci.yml) [![Issues](https://img.shields.io/github/issues/terminusdb/terminusdb)](https://github.com/terminusdb/terminusdb/issues)

## TerminusDB is a distributed database with a collaboration model.

It is designed to be like git, but for data. If you are reading this, **give this repo a star**.

The building blocks of the model are -

  - Revision Control: commits for every update
  - Diff: differences between commits can be interpreted as patches between states
  - Push/Pull/Clone: communicate diffs between nodes using push / pull / clone
  - Query: You can query any state of the database at any commit.

TerminusDB allows you to link JSON documents in a knowledge graph through a [document API](https://terminusdb.com/docs/document-insertion). TerminusDB is available as a standalone server, or you can use our headless content and knowledge management system [TerminusCMS](https://dashboard.terminusdb.com/).

### TerminusDB Version 11

[TerminusDB 11](https://github.com/terminusdb/terminusdb/releases/tag/v11.0.0) features a new Rustified storage backend that reduces storage overhead and latency, improves search performance, and simplifies interchange. TerminusDB 11 also comes with some exciting features to make building easier and faster -

- [GraphQL](https://terminusdb.com/docs/graphql-basics) - Use GraphQL as a proper graph query language with deep link discover and path queries
- Added '@unfoldable' document flag to frames - Making data curation easier by unfolding subdocuments within a frame to add all relevant data in one place
- Add '@metadata' to frames - Include additional metadata to document frames including data formatted as Markdown.


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

2. `docker compose up`

You should be able to view TerminusDB running by default at `localhost:6363`

> If you're installing TerminusDB on Windows with Docker, our friends at DFRNT wrote this [comprehensive guide](https://dfrnt.com/blog/2023-02-25-run-terminusdb-on-windows-with-docker/).


### Get this repo, cd to it

```
git clone https://github.com/terminusdb/terminusdb-bootstrap
cd terminusdb-bootstrap
```

### Run the container by using the script (the first time)

```
./terminusdb-container run

Unable to find image 'terminusdb/terminusdb-server:latest' locally
latest: Pulling from terminusdb/terminusdb-server
8f91359f1fff: Pulling fs layer
939634dec138: Pulling fs layer
f30474226dd6: Pulling fs layer
32a63113e3ae: Pulling fs layer
ae35de9092ce: Pulling fs layer
023c02983955: Pulling fs layer
d9fa4a1acf93: Pulling fs layer
[ ... ]
```

### To stop, attach, etc, see usage
```
./terminusdb-container

USAGE:
  terminusdb-container [COMMAND]

  help        show usage
  run         run container
  cli         use the terminusdb cli
  stop        stop container
  attach      attach to prolog shell
  exec        execute a command inside the container
  rm          remove volumes
```

More information in the [docs](https://terminusdb.com/docs/install-terminusdb-as-a-docker-container), or in the [TerminusDB Bootstrap](https://github.com/terminusdb/terminusdb-bootstrap) repository.


You can also install TerminusDB from the [Source Code](https://terminusdb.com/docs/install-terminusdb-from-source-code).


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

You can attach this to a database on TerminusCMS in order to distribute it by setting your remote. First you will need to create
a resource from [TerminusCMS](https://dashboard.terminusdb.com/). This is analogous to creating a repository on GitHub.

Log into TerminusCMS, create a new data product, and make sure
you have an access token to the team in which we created that data
product. Then copy the URL to clone from the data product info page.

If you make a data product called `example` in the team
`Terminators`. We could then issue the following command using the
TerminusDB CLI. You should get an API token from your profile page and replace `'XYZ'` with this token.

```shell
terminusdb remote add admin/example1 origin 'https://cloud-dev.terminusdb.com/Terminators/example' --token='XYZ'
terminusdb fetch admin/example1 --token='XYZ'
terminusdb push admin/example1 --token='XYZ'
```

Once completed, you'll have a remote copy of this database.

More on the [CLI here](https://terminusdb.com/blog/distributed-database-with-collaboration-model/)

## Community

Come visit us on [Discord](https://discord.gg/yTJKAma)
or our [community Subreddit](https://www.reddit.com/r/TerminusDB/). On Twitter, we're [@TerminusDB](https://twitter.com/TerminusDB).

## Documentation

Check out our documentation site for more information: [TerminusDB documentation](https://terminusdb.com/docs/) website.

We are working hard to improve our docs - if you see an issue, please open an issue in the [documentation repo](https://github.com/terminusdb/terminusdb-docs).

White paper on our [delta-encoding approach](https://terminusdb.com/blog/succinct-data-structures-for-modern-databases/) to data management.

Check the [Python Client](https://pypi.org/project/terminusdb-client/) 🐍. Or the [JavaScript Client](https://github.com/terminusdb/terminusdb-client) 🌐.

WOQL is a powerful query language that allows you to express complex patterns over arbitrary data structures concisely. What makes it so expressive and easy to use is the [radical simplicity of the core underlying concepts](https://terminusdb.com/blog/the-power-of-web-object-query-language/).

## Changes in this Version

[Release Notes](docs/RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```
