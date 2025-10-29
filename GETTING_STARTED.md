# Welcome to TerminusDB!

This document has three sections:

1. Getting started with the open source software
2. Use the cloud version (for less technical users)
3. Getting started as a TerminusDB open source developer

## Use Open Source Version

### What is TerminusDB?

TerminusDB is a distributed database with a collaboration model - designed to be like git, but for data, with a many worlds approach ideal for scenario testing and using a many-worlds approach to logic and knowledge graphs. It provides:

- **Revision Control**: commits for every update
- **Diff**: differences between commits can be interpreted as patches between states
- **Push/Pull/Clone**: communicate diffs between nodes
- **Query**: Query any state of the linked data world at any commit
- **Closed world**: Knowledge Graph with schema and closed world rules
- **Document API**: Link JSON documents in a knowledge graph
- **GraphQL**: Use GraphQL as a proper graph query language with deep link discover and path queries

### Recent changes

TerminusDB no longer includes the dashboard component, to reduce the size of the docker container. Additionally, all JSON interfaces are now consistent with the handling of numbers and strings:

* Arbitrary precision decimals capped at 20 decimal positions
* Arbitrary precision integers
* All numbers represented as JSON numbers (high resolution)
* All strings are UTF-8 encoded

The above is a breaking change with the 11.2 release and may require clients to align to proper processing of the high precision results and use big decimals and big integer libraries to process the results.

JSON itself has support for high resolution when used properly, however this requires accuracy in development. The quest for quality in TerminusDB made the previous inconsistent approach a liability that is now resolved. Check out the integration tests in the [tests](tests/test) folder for examples (see mainly graphql and decimals-precision for examples with apollo and other clients) 

### Installation

#### Option 1: Docker (Recommended for Production)

1. Create a `.env` file in your working directory:

```shell
# Database administrator's password (required)
TERMINUSDB_ADMIN_PASS=your_secure_password

# OpenAI API key (optional - for AI-based indexing)
OPENAI_KEY=

# Optional: Number of pages to buffer for the vector database
BUFFER_AMOUNT=120000
```

2. Start TerminusDB with Docker Compose:

```bash
docker compose up
```

TerminusDB will be available at `http://localhost:6363`

#### Option 2: Snap (Quick Installation for Developers)

[![Get it from the Snap Store](https://snapcraft.io/static/images/badges/en/snap-store-black.svg)](https://snapcraft.io/terminusdb)

```bash
snap install terminusdb
```

The Snap package is intended for developers wanting to try TerminusDB and does not provide a daemon.

#### Option 3: From Source

See [Install from Source Code](https://terminusdb.org/docs/install-terminusdb-from-source-code) in the documentation.

### Quick Start

Once installed, you can create your first database and add data using the CLI:

```shell
# Create a new database
terminusdb db create admin/example1

# Add a schema (define your data structure)
terminusdb doc insert --graph_type=schema admin/example1 <<EOF
{ "@id" : "Person",
  "@type" : "Class",
  "name" : "xsd:string",
  "occupation" : "xsd:string",
  "friends" : { "@type" : "Set",
                "@class" : "Person" }}
EOF

# Insert data
terminusdb doc insert admin/example1 --message='adding Gavin' <<EOF
{ "@type" : "Person","name" : "Gavin", "occupation" : "Coder"}
EOF
```

### Next Steps

- 📚 [Full Documentation](https://terminusdb.org/docs/)
- 🐍 [Python Client](https://pypi.org/project/terminusdb-client/)
- 🌐 [JavaScript Client](https://github.com/terminusdb/terminusdb-client)
- 💬 [Join our Discord Community](https://discord.gg/yTJKAma)

## Use Cloud Version

Get started using the **DFRNT Semantic Data Hub** with the [Studio modeller](https://studio.dfrnt.com).

### What is DFRNT Studio?

DFRNT Studio is a cloud-hosted version of TerminusDB that provides:

- Complete visual data modeling interface for TerminusDB
- Collaborative data management
- Built-in version control
- A Semantic Data Hub with clone, push and pull for data
- No infrastructure to manage
- Enterprise features

### Getting Started

Visit [studio.dfrnt.com](https://studio.dfrnt.com) to:
1. Create a free account
2. Use the cloud interface to work with your local TerminusDB instance!
3. Clone examples to your local TerminusDB instance
4. Start building applications with the semantic data hub

This is ideal for users who want to focus on their data and applications without managing infrastructure.

## Development

### Getting Started as a Contributor

Want to contribute to TerminusDB? Great! check below on how to get started!

#### Quick Start for Development

The fastest way to start developing is using the test server script after cloning [this repository](https://github.com/terminusdb/terminusdb).

```bash
# Start test server (builds if needed, reuses existing storage)
./tests/terminusdb-test-server.sh start

# Start with fresh storage
./tests/terminusdb-test-server.sh start --clean

# Quick restart after code changes
./tests/terminusdb-test-server.sh restart

# Check status
./tests/terminusdb-test-server.sh status

# Stop server
./tests/terminusdb-test-server.sh stop
```

**Server Details:**
- URL: `http://127.0.0.1:6363`
- User: `admin`
- Pass: `root`

#### Manual Build

```bash
# Build Rust library and create development binary
make dev

# Start server manually
./terminusdb serve --storage /tmp/my-test-db
```

#### Running Tests

```bash
# Run all Prolog tests
swipl -g "run_tests()" -t halt -f src/interactive.pl

# Run JavaScript/Node.js tests
cd tests && npm test

# Run specific test suites
npx mocha tests/test/graphql.js
```

### Contributing Guidelines

See [CONTRIBUTING.md](docs/CONTRIBUTING.md) for detailed information on:
- Development workflow
- Building on different platforms (macOS, Linux, Windows)
- Testing procedures
- Pull request guidelines
- Code style and conventions

See [CODE_OF_CONDUCT.md](docs/CODE_OF_CONDUCT.md) for community guidelines.

### Relevant Repositories

* [TerminusDB.org Documentation](https://terminusdb.com/docs/) ([Repo](https://github.com/dfrnt-labs/terminusdb-static-docs))
* [TerminusDB Core](https://github.com/terminusdb/terminusdb) - This repository
* [Python Client](https://github.com/terminusdb/terminusdb-client-python)
* [JavaScript Client](https://github.com/terminusdb/terminusdb-client-js)

### Community

- 💬 [Discord Server](https://discord.gg/yTJKAma) - Get help and chat with the community
- 🐛 [Issue Tracker](https://github.com/terminusdb/terminusdb/issues) - Report bugs or request features
- 📝 [Blog](https://terminusdb.org/blog/) - Technical articles and updates

### Architecture Overview

TerminusDB is built with:
- **Prolog**: Core query engine, logic layer and API interface
- **Rust**: High-performance storage backend with succinct data structures
- **SWI-Prolog**: Prolog implementation
- **Juniper**: GraphQL implementation for Rust

Key components:
- Document API for JSON document management (in Rust core)
- GraphQL query interface (in Rust core)
- WOQL (Web Object Query Language) (in Prolog core)
- Git-like revision control system (in Prolog core)
- Distributed replication (push/pull/clone) (in Prolog core)
- Diff and Patch engine (in Prolog core)
- Schema validation (in Prolog core)
- Closed world inference (in Prolog core)