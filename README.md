<img
  src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/readmes/terminusdb/TerminusDB-Logo-Colour_3.png"
  alt="TerminusDB Logo"
  width="50%"
  align="center"
/>

***

TerminusDB is designed to be a distributed database with a collaboration model built to rhyme with the ideas behind git. It is meant to be a git for data. If you are reading this, **give this repo a star**. 

The building blocks of the model are:

  - Revision Control: We have commits for every update
  - Diff: Differences between commits can be interpreted as patches between states
  - Push/Pull/Clone: We can communicate diffs between nodes using push / pull / clone


TerminusDB allows you to link JSON documents in a knowledge graph through a [document API](https://terminusdb.com/docs/v10.0/#/reference/reference-document-interface). TerminusX is a cloud data platform built on TerminusDB. You can [use now](https://dashboard.terminusdb.com/).


## Installation Guide

Install as a [Docker Container](https://terminusdb.com/docs/index/terminusdb/install/install-as-docker-container). This uses [TerminusDB Bootstrap](https://github.com/terminusdb/terminusdb-bootstrap).

Install from [Source Code](https://terminusdb.com/docs/index/terminusdb/install/install-from-source-code).

Install the [Python Client](https://pypi.org/project/terminusdb-client/) 🐍.

Install the [JavaScript Client](https://github.com/terminusdb/terminusdb-client) 🌐.

## TerminusDB CLI

The simplest way to see how the TerminusDB CLI can be used is to clone
a resource from TerminusX. This is analogous to creating a repository on GitHub.

First we log into TerminusX, create a new data product, and make sure
we have an access token to the team in which we created that data
product. Then copy the URL to clone from the data product info page.

Supposing I make a data product called `example` in the team
`Terminators`. We could then issue the following command using the
TerminusDB CLI.

```shell
./terminusdb clone 'https://cloud-dev.terminusdb.com/Terminators/example' --token='XYZ'
```

Once completed, you'll have a local copy of this database.

More on the [CLI here](https://github.com/GavinMendelGleason/blog/blob/main/entries/terminusdb_cli.md)

## Community

Come visit us on [Discord](https://discord.gg/yTJKAma)
or our [forum](https://discuss.terminusdb.com). On Twitter, we're [@TerminusDB](https://twitter.com/TerminusDB).

## Documentation

Check out our documentation site for more information: [TerminusDB documentation](https://terminusdb.com/docs/terminusdb/) website. 
  
We are working hard to improve our docs - if you see an issue, please open an issue in the [documentation repo](https://github.com/terminusdb/terminusdb-docs). 

White paper on our [delta-encoding approach](https://github.com/terminusdb/terminusdb/blob/dev/docs/whitepaper/terminusdb.pdf) to data mangement. 

## Changes in this Version

[Release Notes](docs/RELEASE_NOTES.md)

## Copyright

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
```
http://www.apache.org/licenses/LICENSE-2.0
```
