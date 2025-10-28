<img
  src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/readmes/terminusdb/TerminusDB-Logo-Colour_3.png"
  alt="TerminusDB Logo"
  width="30%"
  align="center"
/>

---

[![Native Build](https://github.com/terminusdb/terminusdb/actions/workflows/native-build.yml/badge.svg?branch=main&event=push)](https://github.com/terminusdb/terminusdb/actions/workflows/native-build.yml)
[![Docker Build](https://github.com/terminusdb/terminusdb/actions/workflows/docker-amd64-build.yml/badge.svg?branch=main)](https://github.com/terminusdb/terminusdb/actions/workflows/docker-images-publish.yml)
[![Issues](https://img.shields.io/github/issues/terminusdb/terminusdb)](https://github.com/terminusdb/terminusdb/issues)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Project Overview

**TerminusDB is a distributed database with a collaboration model — git for data.**

If you find this project useful, please consider **starring the repo** ⭐

TerminusDB allows you to link JSON documents in a semantic knowledge graph through a powerful [document API](https://terminusdb.org/docs/document-insertion). It's designed as a system of record, making data management collaborative, versioned, and queryable.

### Key Features

- **Revision Control**: Commits for every update — track changes over time
- **Diff**: Differences between commits can be interpreted as patches between states
- **Push/Pull/Clone**: Communicate diffs between nodes using familiar git-like operations
- **Time-Travel Queries**: Query any state of the database at any commit
- **Document + Knowledge Graph**: Link JSON documents in a knowledge graph
- **Multimodal**: Support for REST API, GraphQL, WOQL and with Closed World RDF
- **Goal seeking**: Built in unification, path queries, and a datalog logic engine

### What's New in Version 11

[TerminusDB 11](https://github.com/terminusdb/terminusdb/releases/tag/v11.0.0) features a fast Rust storage backend which reduces storage overhead and latency, improves search performance, and simplifies interchange. Version 11 includes:

- **[REST API](https://terminusdb.org/docs/rest-api)**: Use REST API as a proper graph query language with deep link discovery, path queries and linked data
- **[GraphQL Support](https://terminusdb.org/docs/graphql-basics)**: Use GraphQL as a proper graph query language with deep link discovery and path queries
- **[WOQL Datalog](https://terminusdb.org/docs/datalog-explanation)**: Use WOQL as a goal-seeking problem-solving toolbox, complete with triples across and within documents, path queries and variables unification
- **[Schema Constraints](https://terminusdb.org/docs/schema-reference-guide/)**: Use schema constraints to enforce data quality and consistency, including for advanced typing
- **`@unfoldable` Documents**: Unfold subdocuments within a frame to add all relevant data in one place
- **`@metadata` Support**: Include additional metadata in document frames, including Markdown-formatted data

## Getting Started

The easiest way to install TerminusDB as a developer is by using the [Docker TerminusDB Image](https://hub.docker.com/r/terminusdb/terminusdb-server). It can be joined by using [Snap](https://snapcraft.io/terminusdb) locally as a git-for-data client to perform push and pull. Docker brings the server component, and snap the ability to try TerminusDB on the command line, for example for ML/Ops.


For deployments, copy the docker-compose.yml file from the repository. For a complete modeller user interface, create an account for the [DFRNT Studio](https://studio.dfrnt.com) which can also be used to model TerminusDB on localhost! There is still the possibility to run the deprecated (buggy) dashboard, by following a [couple of instructions](https://terminusdb.org/docs/dashboard).

1. Add the following to a `.env` file in the source directory:

```shell
# Database administrator's password (required)
TERMINUSDB_ADMIN_PASS=

```

Notes:
 * TERMINUSDB_ADMIN_PASS is mandatory and must be set.


1. `docker compose up`

You should be able to view TerminusDB running by default at `localhost:6363`

> If you're installing TerminusDB on Windows with Docker, our friends at DFRNT wrote this [comprehensive guide](https://dfrnt.com/blog/2023-02-25-run-terminusdb-on-windows-with-docker/).

You can also install TerminusDB from [Source Code](https://terminusdb.org/docs/install-terminusdb-from-source-code).

## Usage

### Quick Start with CLI

Once installed, you can start using TerminusDB immediately. Here's a simple example creating a person with friends:

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

### Client Libraries

TerminusDB provides official client libraries for multiple languages:

- 🐍 **[Python Client](https://pypi.org/project/terminusdb-client/)**: Full-featured Python library for TerminusDB
- 🌐 **[JavaScript Client](https://www.npmjs.com/package/@terminusdb/terminusdb-client)**: Browser and Node.js support
- **[Rust Client](https://github.com/ParapluOU/terminusdb-rs)**: Full-featured Rust library for TerminusDB, developed by the community

## Documentation

Full documentation is available at **[terminusdb.org/docs](https://terminusdb.org/docs/get-started-with-terminusdb/)**.

### Key Resources

- **[Getting Started Guide](https://terminusdb.org/docs/get-started-with-terminusdb/)**: Complete onboarding tutorial
- **[Document API](https://terminusdb.org/docs/document-insertion)**: Working with JSON documents
- **[GraphQL Guide](https://terminusdb.org/docs/graphql-basics)**: Query your knowledge graph with GraphQL
- **[WOQL](https://terminusdb.org/docs/datalog-explanation/)**: Web Object Query Language fundamentals
- **[Release Notes](docs/RELEASE_NOTES.md)**: Latest changes and version history

Found an issue in the docs? Please [open an issue or pull request](https://github.com/dfrnt-labs/terminusdb-docs-static) in our documentation repo or here.

## Community

Come visit us on **[Discord](https://discord.gg/yTJKAma)** to:

- Ask questions and get help
- Share your projects and use cases
- Contribute to discussions
- Stay updated on the latest developments

## Contributing

We welcome contributions from the community! Whether you're fixing bugs, adding features, or improving documentation, your help is appreciated.

### How to Contribute

1. **Fork the repository** and create your feature branch
2. **Run tests** to ensure everything works: `make dev && ./terminusdb test`
3. **Write clear commit messages** with descriptive titles
4. **Submit a Pull Request** to the main branch

### Development Setup

Quick start for contributors:

```bash
# Clone your fork
git clone git@github.com:[your_username]/terminusdb.git
cd terminusdb

# Build the project
make dev

# Start test server
./tests/terminusdb-test-server.sh start

# Run tests
npx mocha tests/test/*.js
```

For detailed development instructions, coding conventions, and testing guidelines, see **[CONTRIBUTING.md](docs/CONTRIBUTING.md)**.

### Reporting Issues

Found a bug? Have a feature request? Please [open an issue](https://github.com/terminusdb/terminusdb/issues) using the bug template, with:

- Clear description of the problem or suggestion
- Steps to reproduce (for bugs)
- Expected vs. actual behavior
- Your environment details (OS, TerminusDB version)

## License

TerminusDB is licensed under the **Apache License 2.0**.

You may obtain a copy of the license at: http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
