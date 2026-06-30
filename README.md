<img
  src="https://github.com/terminusdb/terminusdb-web-assets/blob/master/readmes/terminusdb/TerminusDB-Logo-Colour_3.png"
  alt="TerminusDB Logo"
  width="30%"
  align="center"
/>

---

[![Native Build](https://github.com/terminusdb/terminusdb/actions/workflows/native-build.yml/badge.svg?branch=main&event=push)](https://github.com/terminusdb/terminusdb/actions/workflows/native-build.yml)
[![Docker Build](https://github.com/terminusdb/terminusdb/actions/workflows/docker-images-publish.yml/badge.svg?branch=main)](https://github.com/terminusdb/terminusdb/actions/workflows/docker-images-publish.yml)

[![Issues](https://img.shields.io/github/issues/terminusdb/terminusdb)](https://github.com/terminusdb/terminusdb/issues)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Project Overview (Updated May 2026)

**TerminusDB is a distributed database with a collaboration model — git for data.**

If you find this project useful, please consider **starring the repo** ⭐

TerminusDB allows you to link JSON and JSON-LD documents in a semantic knowledge graph through a powerful [document API](https://terminusdb.org/docs/document-insertion). It's designed as a system of record, making data management collaborative, versioned, and queryable.

Now with new maintainers and an enterprise version. 

### Key Features

- **Revision Control**: Commits for every update — track changes over time
- **Diff**: Differences between commits can be interpreted as patches between states
- **Push/Pull/Clone**: Communicate diffs between nodes using familiar git-like operations
- **Time-Travel Queries**: Query any state of the database at any commit
- **Document + Knowledge Graph**: Link JSON documents in a knowledge graph
- **Multimodal**: Support for REST API, GraphQL, WOQL and with Closed World RDF
- **Goal seeking**: Built in unification, path queries, and a datalog logic engine

### What's New in Version 12

[TerminusDB 12](https://github.com/terminusdb/terminusdb/releases/tag/v12.0.5) features performance, stability and precision provided by the new [DFRNT](https://dfrnt.com?utm_source=github_terminusdb) maintainers. Now engineered for industrial and financial use cases with high precision math, temporal reasoning, and stability under load. Version 12 includes:

- **[Arbitrary Precision xsd:decimal]()**: Arbitrary rational precision xsd:decimal mathematics, including high precision JSON decimals over the wire for JSON, GraphQL and WOQL, see [ISO/IEC 21778:2017](https://www.iso.org/standard/71616.html)
- **[Allen Integer Algebra and Temporal Reasoning](https://terminusdb.org/docs/woql-interval-algebra/)**: Reason about ISO8601 date, time and durations with high precision rational math
- **[Range Queries over Succinct Data](https://terminusdb.org/docs/woql-triple-slice/)**: Extremely fast range queries with constant time retrieval over arbitrary size ordered data, which is great for knowledge graph processing over timestamp, numeric, and interval ranges; with strong temporal reasoning and classification
- **[ISO8601 Allen Interval Algebra](https://terminusdb.org/docs/woql-interval-algebra/)**: Support for temporal reasoning and storage of XSD time datatypes including temporal interval classification, storage and life cycle management with datalog reasoning
- **[JSON Git-for-Data (Linked Data)](https://terminusdb.org/docs/git-for-data-reference/)**: Git for data for JSON, JSON-LD, XML and Turtle documents with schema control (and also for deduplicated schemaless JSON documents)
- **[REST API](https://terminusdb.org/docs/rest-api)**: Use REST API as a proper graph query language with deep link discovery, path queries and linked data
- **[GraphQL Support](https://terminusdb.org/docs/graphql-basics)**: Use GraphQL as a proper graph query language with deep link discovery and path queries
- **[WOQL Datalog](https://terminusdb.org/docs/what-is-datalog)**: Use WOQL as a goal-seeking problem-solving toolbox, complete with triples across and within documents, path queries and variables unification
- **[Schema Constraints](https://terminusdb.org/docs/schema-reference-guide/)**: Use schema constraints to enforce data quality and consistency, including for advanced typing
- **`@unfoldable` Documents**: Unfold subdocuments within a frame to add all relevant data in one place
- **`@metadata` Support**: Include additional metadata in document frames, including Markdown-formatted data

## Getting Started

The easiest way to install TerminusDB as a developer is by following the [10-minutes docker getting started manual](https://terminusdb.org/docs/get-started/) with the latest [Docker TerminusDB Image](https://hub.docker.com/r/terminusdb/terminusdb-server). It can be installed locally using [Snap](https://snapcraft.io/terminusdb) as both git-for-data client and server to perform push and pull. Docker brings the server component, and snap the ability to try TerminusDB on the command line, for example for ML/Ops and AI use cases requiring deterministic symbolic AI processing.

For deployments, copy the docker-compose.yml file from the repository. For a complete modeller user interface, create an account for the [DFRNT Studio](https://studio.dfrnt.com) which can also be used to model TerminusDB on localhost! There is still the possibility to run the now deprecated (buggy) dashboard, by following a [the instructions for the TerminusDB dashboard](https://terminusdb.org/docs/dashboard).

1. Add the following to a `.env` file in the source directory:

```shell
# Database administrator's password (required)
TERMINUSDB_ADMIN_PASS=

```

Notes:
 * TERMINUSDB_ADMIN_PASS is mandatory and must be set.


1. `docker compose up`

You should be able to view TerminusDB running by default at `localhost:6363`

> If you're installing TerminusDB on Windows with Docker, follow the comprehensive guide for [TerminusDB on Windows with Docker]([https://dfrnt.com/blog/2023-02-25-run-terminusdb-on-windows-with-docker/](https://terminusdb.org/docs/install-terminusdb-docker-windows/)).

You can also install TerminusDB from [Source Code](https://terminusdb.org/docs/install-terminusdb-from-source-code).

## Usage

### Quick Start with CLI

Once installed, you can start using TerminusDB immediately. Here's a simple example creating a person with friends (or use the [Manual and more in-depth 15-minutes Getting Started Guide](https://terminusdb.org/docs/first-15-minutes/)):

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

- 🐍 **[Python Client (v12)](https://pypi.org/project/terminusdb/)**: Full-featured Python library for TerminusDB
- 🌐 **[JavaScript Client (v12)](https://www.npmjs.com/package/terminusdb)**: Browser and Node.js support
- **[Rust Client](https://github.com/ParapluOU/terminusdb-rs)**: Full-featured Rust library for TerminusDB, community contribution
- 🔮 **[Elixir Client](https://hex.pm/packages/terminusdb_client)**: An idiomatic Elixir client


## Documentation

Full documentation is available at **[https://terminusdb.org/docs](https://terminusdb.org/docs/get-started-with-terminusdb/)**.

### Key Resources

- **[What is TerminusDB](https://terminusdb.org/docs/terminusdb-explanation/)**: Why TerminusDB is a unique Graph Database
- **[Getting Started Guide](https://terminusdb.org/docs/get-started/)**: Complete onboarding tutorial
- **[Document API](https://terminusdb.org/docs/document-insertion)**: Working with JSON documents
- **[GraphQL Guide](https://terminusdb.org/docs/graphql-basics)**: Query your knowledge graph with GraphQL
- **[WOQL](https://terminusdb.org/docs/what-is-datalog/)**: Web Object Query Language fundamentals
- **[Release Notes](docs/RELEASE_NOTES.md)**: Latest changes and version history
- **[TerminusDB Enterprise]()**: Full JSON-LD, Turtle, and RDF/XML documents, enterprise features, very fast commit history queries, higher write performance, clustering, API for data product backup and restore (beyond command line tools) 

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
