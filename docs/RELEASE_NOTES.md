# TerminusDB Server v11.2.0 Release Notes

## Bug fixes
* GraphQL enum names were not mapped to their proper IRI format in some cases
* Server URI in container dashboard is no longer hardcoded

## Enhancement
* WOQL Query order of execution has been improved
* Sped up path traversal
* Schema migrations now support making classes abstract and/or unfoldable

## New
* New flag `merge_repeats` in document interface insert that merges documents with the same id


# TerminusDB Server v11.1.1-1 Release Notes

Very minor hotpatch release to include the new local dashboard.

## New

* New dashboard version (v6.0.9) which includes various fixes and enhancements, like the ability
  to properly close pull requests.

# TerminusDB Server v11.1.1 Release Notes

This release introduces a docker-compose deployment method which gets you all components of a fully-featured terminusdb setup:
* TerminusDB - the main database.
* VectorLink (experimental) - a database sidecar dedicated to storing, indexing and retrieving vectors by similarity, used for AI-enhanced search.
* terminusdb-change-request-api - an API for working with change requests.
* Terminusdb-dashboard - our dashboard for managing terminusdb, which now experimentally supports AI search and change requests.

Beyond that, this version brings various fixes and enhancements to our GraphQL support, our schema checking and migrations, and the document interface. Please read the detailed list below.

## Bug fixes
* multi-level graphql hierarchies were not always returning all objects.
* properties with prefixes didn't work properly in graphql.
* graphql reverse links did not work with sanitized names.
* graphql incorrectly filtered out 'id' from user-defined types.

## New

* docker-compose now comes bundled with vectorlink, local dashboard and pull request API.
* properties can now be inherited from multiple superclasses, and even redefined in a subclass, as long as they form a type-compatible hierarchy. The strictest form is actually applied. If the subclass defines a property that is not at least as strict as what was defined in the supers, this results in a schema error.
* graphql can now be made to not return subsumptions through the generated `include_children` argument.
* new environment variable `TERMINUSDB_TRUST_MIGRATIONS` will ensure that instance data is not re-checked when schemas change due to migrations, as migrations should in principle not result in any errors. As the migration feature is still relatively new, the default here is false, meaning that without this environment variable, all schema changes result in a recheck of all existing instance data. In the future, this default will likely change.

## Enhancements

* schema migration scripts now support changes to class hierarchies and key strategies.
* the graphql engine now keeps a small cache around of recent schemas to speed up repeated queries.

## Experimental

* graphql filters now support deep matches on IDs.
* document API has a new parameter `ids` on GET allowing you to submit a list of documents to retrieve at once.

# TerminusDB Server v11.1.0 Release Notes

## Bug fixes

* Fix ordering of graphql stringy numbers
* Adding type information to decimals
* Adding a correct diff using out of band value
* Prevent reserved words in GraphQL from being used
* Fix loading of triples

## New

* Add support for the TerminusDB semantic indexer for AI search capabilities
* Add ability to merge base layers with the `concat` command

## Enhancement

* Reduced memory usage
* Remove Google Analytics in Dashboard

# TerminusDB Server v11.0.6 Release Notes

## Bug fixes
* Unused optional properties would cause GraphQL to fail
* Enums were not working properly for collection types in GraphQL
* Incorrect version checking on system graphs caused superfluous graph updates on startup
* Modifications of classes with `@oneOf` would leave stale objects in the schema graph
* Diffing sets now works
* Fix error reporting when pushing nonexistent remotes
* GraphQL did not work properly with `@oneOf` properties
* Memory leak in document API
* Crash when processing several compressed requests in the same session

## New
* On schema modifications, a migration will be inferred when possible.
* New options added to document interface
** `allow_destructive_migration`: Allow for inference of migrations that will also modify instance data. If set, the schema modification will also automatically transform any instance objects already in the database.
** `require_migration`: This will cause the request to fail for schema changes for which no migration can be inferred.

## Enhancement
* diff endpoint now has `start` and `count` options for paged results
* Improve memory footprint when retrieving lots of documents through the document API
* Support JSON objects in GraphQL

# TerminusDB Server v11.0.5 Release Notes
## Bug fixes
* diff endpoint was mishandling subdocuments

# TerminusDB Server v11.0.4 Release Notes
## New
* New history API to find out when an object changed

## Enhancement
* Keep loaded layers in an LRU cache for faster repeated queries
* Optimize will now also optimize schema layers
* Improved memory use during commits and squashes

## Experimental
* New endpoint for schema migrations which also transforms existing data
* Support updating a schema from a target branch as long as there is a migration

# TerminusDB Server v11.0.3 Release Notes

## Bug fixes

* Prevent early failure on full schema replace where id is missing
* Support stringy filters on all stringy types, not just strings
* Return 204 on doc delete (and disable chunking for non-gets)
* Fix inversion naming issue. This causes issues when using non-standard GraphQL class names

## New

* Adding Patch Endpoint for Resources
* Implement pinned organization ENV variable to keep data products of whole organizations/teams in memory

# TerminusDB Server v11.0.2 Release Notes

## Bug fixes
* On replace, schema documentation entities were not cleaned up properly
* Fix support for ne (not equal) operator in graphql

# TerminusDB Server v11.0.1 Release Notes

## Backwards-incompatible changes
* Document replacement allowed insertion of nested documents, even
  without the create option specified. We decided that this was a bug,
  and made it so that this now errors, unless the create option is
  specified. This is backwards-incompatible, but we believe that this
  is the correct behavior. If your code relies on nested documents
  being inserted, make sure you specify the create option. Note that
  this is just about nested documents which are top-level
  types. Subdocuments are not affected.

## New

* Introduced support for a grpc label store as an alternative to file-based label lookup [terminusdb-labs/terminusdb-grpc-labelstore](https://github.com/terminusdb-labs/terminusdb-grpc-labelstore)
* Implemented local clone
* Implement user impersionation in the CLI tool
* Added a pre-commit hook to allow implementation of custom schema validations

## Enhancements

* Pinned system schema graph so it is only loaded once
* Reduced amount of hash roundtrips for newly generated passwords to speed up login
* Disabled authentication on the ok endpoint for faster roundtrip
* Removed default SWI-Prolog HTTP server welcome message
* Improved db list speed

## Bug fixes
* Fixed startup message when no store is present
* Ensured that all users automatically have all capabilities of anonymous
* Delete the stale database if a clone fails
* Fix SSL issues in fetch and push: newer version of openssl errors when a connection is closed unexpectedly
* Fix accidental insertion of duplicate nested documents
* Fixed GYear support

# TerminusDB Server v11.0.0 Release Notes

## Backwards-Incompatible Changes

* This release changes the storage format of TerminusDB. After
  installing this version, you will also need to upgrade your storage
  directory, or the server will not start. A conversion tool is
  provided at
  [terminusdb-10-to-11](https://github.com/terminusdb/terminusdb-10-to-11). This
  is also bundled with
  [terminusdb-bootstrap](https://github.com/terminusdb/terminusdb-bootstrap).
* TerminusDB 11 now responds to `xsd:integer`, `xsd:decimal`, and all
  of the unbounded sub-types of these two objects in the document
  interface with strings. This is because many (even most) JSON
  libraries can not handle arbitrary precision integers, and (few if
  any) can handle arbitrary precision floats. It should still be
  possible to submit documents using integers or floats, but when
  returned they will be strings.

## Enhancements

* New Storage backend
  - Added typed storage for a wide variety of XSD types, reducing
    storage overhead and improving search performance
  - Introduce a layer archive format reducing storage use and latency
    and simplifying interchange.
* GraphQL `_type` added to objects to return the exact rather than subsumed type
* Added `@unfoldable` document flag to frames
* Add `@metadata` to frames

## Bug fixes

* Fixed a bug in inverse fields in GraphQL
* Removed extraneous system objects from GraphQL schema
* Improved completeness of GraphQL schema handling
* Added x-method-override header to CORS
* Fixed GraphQL naming bug leading to GraphQL schema crashes

# TerminusDB Server v10.1.11 Release Notes

## Enhancements

* GraphQL now deals with more TerminusDB schemata
* Add JSON formatted logs for welcome message
* Consumer role now has expanded privileges enabling public access for clones
* Context insert/replace now gives a more understandable error
* We now check the store version before startup to prevent version mismatch between server and store during upgrades

## Bug fixes

* Fix JWT lib build
* Expanded error handling for fetch
* WOQL AST was incorrect for some WOQL words which specify the graph
* Fix subsumption in document retrieval
* Fix subsumption in GraphQL retrieval

# TerminusDB Server v10.1.10 Release Notes

## Enhancements
+ Add ability to filter by multiple IDs in GraphQL
* Collections over base types
* Add Graphql backlinks
* Prevent exhaustive list comparisons in diff
* Path queries added to GraphQL

## Bug fixes

* Fix dangling reference check, improves performance on deletion
* Remove TERMINUSDB_LOG_PATH ENV as it caused a crash
* Graphiql is not hardcoded to localhost:6363 anymore
* Avoid framing json metadata

# TerminusDB Server v10.1.9 Release Notes

## Enhancements
+ Add new ENV variable called `TERMINUSDB_DASHBOARD_PATH` to set
  the dashboard path at compile time.

## Bug fixes
+ Fix crash when using some special unicode characters because
  the length of those characters were wrongly calculated.
+ Fix set property inheritance in doc retrieval.

# TerminusDB Server v10.1.8 Release Notes

## Enhancements
+ GraphQL endpoint at localhost:6363/api/graphql/ORG/DB
+ GraphiQL endpoint for testing available at localhost:6363/graphiql/ORG/DB
+ GraphQL automatic loading of Schema in GraphQL schema
+ GraphQL search and retrieval
+ New HEAD call for document endpoint, which makes it easy to check for existence
+ Add ARM64 support
+ New dashboard version: 0.0.10

## Bug fixes
+ Anonymous users now results in an auth with an explicit URI in the systemDB
+ Triple load interface now has better error reporting and is more robust
+ `@metadata` bugs on ingest / exgest fixed for classes.
+ Diff no longer returns explicit IDs for subdocuments, and does not compare IDs
+ sys:Unit now interacts correctly with Option

# TerminusDB Server v10.1.7 Release Notes

## Bug fixes
+ Better serialization/deserialization of Prolog strings, prevents
  crashes on unexpected characters
+ Fix group\_by query in dashboard
+ Fix time travel in dashboard
+ Fix bad parsing of xsd:Name
+ Fix corrupted data products on wrong prefixes

## Enhancements
+ Add new Dashboard version
+ Add xsd:Name and xsd:anyUri property type support in dashboard
+ Use new terminusdb-client version in dashboard

# TerminusDB Server v10.1.6 Release Notes

## Bug fixes
+ Escaped characters in json documents were escaped twice

# TerminusDB Server v10.1.5 Release Notes

## Enhancements
+ Trig files can now be loaded directly
+ Improved speed of document retrieval through new rust backend
+ Json responses are now a single line
+ Triple endpoint will now return turtle files if the right Accept header is specified
+ Grant endpoint improved to take names rather than IRIs
+ Squash added to CLI
+ Improve performance of forward references during document insert
+ Document interface can now handle 'reverse links' where subdocuments specify how they link back to their document, rather than the other way around

## Bug fixes
+ Fixed inheritance of multi-language documentation
+ Fixed handling of literals with language tags
+ Db name length is now validated properly before database creation is attempted
+ Document interface errors from woql are now properly reported
+ Frame endpoint now reports oneof properties properly
+ Fix woql typeof
+ Prefixes are now more properly recognized, allowing use of colons in contracted IRIs
+ Proper error reporting on diff
+ Better error handling for log
+ Captures didn't work in full replace

# TerminusDB Server v10.1.4 Release Notes

## Enhancements

+ New version of TerminusDB dashboard
+ Add multilanguage support in schemas
+ Add free (untyped) JSON metadata to classes

## Bug fixes

+ Fix serious performance regression (https://github.com/terminusdb/terminusdb/pull/1401)
+ Fix JSONDocument type is not linkable from Option, Set, List, Array (issue #1278)

# TerminusDB Server v10.1.3 Release Notes

## Enhancements

+ Pretty print lists
+ Better error messages
+ Add count option to `terminusdb log`
+ Users can now cast strings to 'sys:Top' in WOQL allowing them to
  treat a string as a node in the graph.

## Bug fixes

+ Improved error handling
+ Fixed password change bug, where a user would lose its capabilities when password changed.
+ Fix bug in WOQL inserting typed lists. (#1349)
+ Fix IANA code recognition
+ Fix read lock errors
+ Check inheritance of non-existing classes

# TerminusDB Server v10.1.2 Release Notes

## Enhancements
+ Added data product and user management dashboard
+ Speed up for raw JSON processing, especially large arrays
+ User Management Endpoints added
+ OpenAPI Specification of Endpoints expanded and added to source
+ Allow configurable name in JWT
+ Classes can be marked unfoldable, to get default unfolding of
  trees. Classes which have circularities leading to infinite
  unfoldings, will give an error
+ Added api/log route, so that history of a data product can be obtained
+ Added api/list and `terminusdb db list` to CLI to list databases with enhanced
  metadata display
+ Branches can be diffed by name, rather than commit now.
+ Added `terminusdb db update` to CLI
+ Added `terminusdb reset` to CLI

## Bug fixes
+ Referential integrity checking over-conservatism fixed
+ Raw JSON can now be used in containers (Lists, Sets, Arrays)
+ Path queries with path{n,m} fixed to be more forgiving about n and m
  arguments (strings will be parsed as ints)
+ Various error handling improvements to CLI
+ Fixed bug in enum checking of containers (Lists, Sets, Arrays)
+ Fixed enum inference to allow full enum URI

## Other
+ DB_SPEC explained in man page

# TerminusDB Server v10.1.1 Release Notes

## Enhancements
+ Added snap distribution
+ CLI query and log commands can now output as json
+ CLI apply command added for applying patches
+ Document ID list returned in various CLI commands made to print in a uniform way

## Bug fixes
+ CLI branch command fixed
+ CLI doc command query switch fixed
+ CLI diff argument processing fix
+ Fix inference subsumption order
+ Resolved double dependency on tokio in store backend
+ Fix -c flag on CLI document replace

## Other
+ Integration test suite refactored

# TerminusDB Server v10.1.0 Release Notes

## Enhancements
+ Change default query thread count to number of hardware threads
+ Opening a layer no longer uses a read lock. Locks are now only used for
  writes.
+ Insert and query arbitrary JSON documents as either top-level documents or as
  subdocuments.

## Other
+ Improve the build process by reducing duplication between Makefile and
  Dockerfile. Use DOCKER_BUILDKIT=1 to support building the enterprise
  distribution and to speed up the build.

# TerminusDB Server v10.0.25 Release Notes

## Bug fixes
+ Fix cost calculations to make a more accurate diff
+ Add missing error-checking on database creation
+ Halt on closed user output in the CLI
+ Handle empty environment variables in the CLI
+ Fix WOQL using so resolution works on absolute descriptors
+ Improve robustness of CLI commands:
  - clone
  - push
  - pull
  - query

## Enhancements
+ Replace elaboration with type inference in the document interface
+ Add squash commit to apply the diff between two commits to a branch
+ Add explicit copy to diff
+ New CLI commands:
  - diff
  - log
  - doc delete
  - doc replace
+ New CLI command flags:
  - doc insert: --full-replace
  - doc get: many new flags

## Other
+ Integrate `terminus_store_prolog` directly into TerminusDB

# TerminusDB Server v10.0.24 Release Notes
## Bug fixes
+ full_replace on schemas ignored newly submitted prefixes
+ For push operations, TUS will now submit proper Content-Length header

## Enhancements
+ CLI tool now has a --version flag that reports the version

# TerminusDB Server v10.0.23 Release Notes
## Bug fixes
+ Fix full_replace flag handling of duplicate IDs
+ Push can now overwrite a remote branch if this remote branch has no commits
+ Ensure that push, pull, fetch and clone all work with TerminusX

## Enhancements
+ Refactor full_replace to reuse code path with normal insert
+ Support chunked document retrieval
+ Document interface no longer has a timeout
+ Update build instruction documentation
+ Improved integration tests to run CLI tests without deleting an existing store

# TerminusDB Server v10.0.22 Release Notes
## Enhancement
+ Speed improvement to document interface
+ Add WOQL document templates to simplify inserting and updating documents via
  WOQL
+ Add JSON diff between objects and commits

## Other
+ Make unit tests run concurrently properly

# TerminusDB Server v10.0.21 Release Notes
## Bug fixes
+ Fix to allow optional `@comment` in `@documentation`

## Enhancement
+ Add `@context` to class frames
+ Add preliminary support for Content-Encoding compression to some endpoints
+ Minor speedup in document insertion

# TerminusDB Server v10.0.20 Release Notes
## Bug fixes
+ Fix: Forward id capture was capturing unexpanded ID

## Enhancement
+ Introducing cardinality type family

## Other
+ Various CI fixes to run unit tests properly

# TerminusDB Server v10.0.19 Release Notes
## Other
+ Use default user and password for tests
+ Use environment variable for CLI tests

# TerminusDB Server v10.0.18 Release Notes
## Bug fixes
+ Fix QueryResource post and url
+ Fix operation id logging so nothing gets wrapped in quotes
+ Fix WOQL document updates with random key type
+ Type definitions with type families are now properly checked to ensure their contained type exists
+ More endpoints now properly report errors when the database does not exist
+ Unit type handling is now more robust

## Enhancement
+ upgrade SWI-prolog to 8.4.2 in the docker container
+ Info endpoint reports the hash of the commit terminusdb was built with
+ On startup, TerminusDB now reports its version number
+ Common data layers are now pinned in memory

## Other
+ Added benchmark tests
+ Added CLI tests

# TerminusDB Server v10.0.17 Release Notes
## Bug fixes
+ Fix valuehash generation for multidimensional arrays
+ Fix patch cost calculations
+ Cardinality failure on inherited tagged union in combination with optional

## Enhancement
+ Allow optional comment on db creation

## Other
+ Removed message api endpoint
+ Moved various unit tests into the integration tests

# TerminusDB Server v10.0.16 Release Notes
## Enhancements
+ Patch and diff api endpoints
+ Multi-dimensional arrays
+ Data version header is now returned and accepted
+ Various improvements to error messages

## Bug fixes
+ Id capture did not work properly with transaction retries

# TerminusDB Server v10.0.15 Release Notes
## Bug fixes
+ id capture did not work with document arrays
+ subdocuments could be submitted as root documents

# TerminusDB Server v10.0.14 Release Notes
## New
+ id capture: during document insert and replace, it is now possible
  to capture an id and then refer to it in another document, allowing
  for easy document linking

# TerminusDB Server v10.0.13 Release Notes
## Bug fixes
+ utf-8 handling is now the default for json input
+ enums could not be documented
+ schema endpoint did not work properly with enums and `@oneOf`
+ floats could not be queried

## Enhancement
+ New flag compress_ids replaces prefixed. prefixed deprecated but
  retained for backwards compatibility.
+ schema endpoint will now return all types if none is specified
+ new environment variables `TERMINUSDB_INSECURE_USER_HEADER_ENABLED`
  and `TERMINUSDB_INSECURE_USER_HEADER`. if the enabled header is set
  to true, the content of the given header will be interpreted as
  containing the authenticated user, with no further authentication
  checks taking place. This can be used to plug in an external
  authentication mechanism.
+ Upgraded to the latest version of SWI-Prolog

# TerminusDB Server v10.0.12 Release Notes
## Bug fixes
+ Query parameter is ignored.
+ Type parameter is ignored for queries.
+ Subsumption check between specified type and match document type is reversed.

# TerminusDB Server v10.0.11 Release Notes
## New
+ `@oneOf` property in type definitions for disjoint properties as a
  generalization of tagged unions.

## Bug fixes
+ Cardinality bug fixes.
+ More robust request parameter checking.

## Enhancements
+ Frame generation can now generate frames for all classes at once.
+ Speed improvements in schema checking.
+ Tagged unions can now be inherited.

# TerminusDB Server v10.0.10 Release Notes

## Bug fixes
+ Improved handling of '@id' and '@type' for strange, non string
  values.

## Enhancements
+ Optimizations to improvement performance of the document interface.
+ Document paths now include an "any-forward" and "any-backward" by
  omiting the predicate from the 'PathPredicate' or 'InversePathPredicate'
+ Insert or Update functionality added to the document interface.
  which allows updating or inserting.

# TerminusDB Server v10.0.9 Release Notes
## Bug fixes
+ References to documents were dropped on document replacement
+ booleans handled incorrectly in document interface

## Enhancements
+ Improvements to error reporting
+ Speed improvements to schema checking

# TerminusDB Server v10.0.8 Release Notes
## New
+ Performance enhancements to document interface
+ Better transaction retry back-off parameters
+ New integration tests to improve correctness checking in CI

## Bug fixes
+ Schema document deletion bug fixed
+ Better error handling
+ More complete handling of JSON-LD format
+ Fixed WOQL IsA handling
+ Fixed behaviour of some WOQL update words

# TerminusDB Server v10.0.7 Release Notes
## Bug fixes
+ Enum triples were not properly type-checked
+ Authorization issues with graph filters
+ Document queries could not search for enums and uris

## Other
+ Remove obsolete code concerning unmaintained console

# TerminusDB Server v10.0.6 Beta Release Notes
## Bug fixes
+ Fix appimage build
+ Better error handling

# TerminusDB Server v10.0.5 Release Notes
## Bug fixes
+ Update version number in info

# TerminusDB Server v10.0.4 Release Notes
## New
+ New header `X-Operation-ID` for submitting an operation id which is included in json log records made during a request
+ All json log records made during a request now contain a requestId field
+ All authentication attempts are now logged

## Bug fixes
+ Wrong version reported in the info endpoint
+ Key values not normalized properly in id generation
+ New empty branches have no context object
+ Banner message prints weekday locale-dependent in otherwise english message
+ Banner message prints server name incorrectly

# TerminusDB Server v10.0.3 Release Notes
## New
+ Logging backend has been replaced with structured logging

## Bug fixes
+ Floats not marshalled correctly
+ Fallback route handler for unknown paths
+ Improved error reporting

# TerminusDB Server v10.0.2 Release Notes

## New
+ Enable keys with collections or optional fields
+ Include release management document

## Bug fixes
+ Better managment of keys and key error reporting

# TerminusDB Server v10.0.1 Release Notes

## Bug fixes

+ Numerous bug fixes
+ Error reporting improvements

# TerminusDB Server v10.0.0 Release Notes

## New
+ JSON schema interface - build schemas using a simple JSON format.
+ Radically simplified document interface - you can insert, update and query JSON documents.
+ JSON documents can refer to other documents in the graph.

## Bug fixes

+ Improvements to datatype coverage and correctness.

## Backwards-Incompatible Changes

+ TerminusDB no longer supports OWL schema validation.
+ The WOQL JSON interchange format has changed to enable WOQL documents to be savable and retrievable from within TerminusDB.

# TerminusDB Server v4.2.3 Release Notes

## New

+ Use new Rust-Prolog bridge for storage backend

# TerminusDB Server v4.2.2 Release Notes

## New

+ Add new TerminusDB console with performance improvements

# TerminusDB Server v4.2.1 Release Notes

## New

+ Coverage of all xsd types in casting, JSON-LD and storage
+ TypeOf now allows you to get the type of languages or nodes (as 'owl:Thing')
+ Expose internal RandomIdgen for generating unique URIs
+ New startup flag (--memory) to start TerminusDB in memory mode, without persistent storage
+ Optimized database listing, added JSON output in command line
+ Added memory only mode for non-persistent TerminusDB instances
+ Speed improvement for `terminusdb list`
+ Clearer errors

## Bug fixes

+ Improved robustness of casting.
+ Fix round-trip of dates and integers in Turtle
+ Throw error if no csv of the given name exists when using get_csv
+ Correct super-user permission scoping for various actions
+ Correct race condition in optimize for system graph and meta graph
+ Add imprecise rollups to avoid crash on poorly specified boundaries
+ Fixed organization filter being too strict

# TerminusDB Server v4.2.0 Release Notes

## New

+ Large data transfers over TUS protocol
+ Delta rollups are now used for more graph types
+ Optimize utilizes an exponential rollup strategy
+ Document interface with CRUD actions
+ Support for adding JSON documents
+ New frame for adding class choices
+ New branch management actions: squash, reset, delete, optimize
+ Refresh button to reload data in table

## Bug fixes

+ Casting fixes
+ Improve error reporting in user creation
+ Improve robustness of transaction retry
+ Fix cardinality calculation for subproperties
+ Fixed problem with prefixes parameter in query panel

## Changes

+ Switch to google cdn
+ Various speedups

## Improvements

+ Improved speed of database storage size calculation


# TerminusDB Server v4.1.1 Release Notes

## New

+ Add rpm build
+ Optimize uses delta rollups

## Bug fixes

+ JWT support was not working in the compiled binary, it is working now


# TerminusDB Server v4.1.0 Release Notes

## New

+ Add deb repo
+ Multiple witness flag, it allows to return all witnesses of failure during a WOQL query

## Bug fixes

+ Bug in push fixed related to push histories
+ Fix frame logic
+ Fix certificate bug on compiled versions
+ Error response on rebase is fixed


# TerminusDB Server v4.0.0 Release Notes

## New

+ Added "typeof" to WOQL allowing extraction or filtering of types from arbitrary nodes or datapoints
+ Added some CURL connection examples
+ CSV loading and updating
+ Automatic CSV schema generation
+ Allow embedded "usings" with appriopriate expansion of prefixes according to which database is referred to in the using
+ New CLI interface
+ Mac OS version released
+ Model Building Tool

## Bug fixes

+ Fix CORS headers on get request for CSV
+ Fixed handling of dates in typecasting
+ Schema validation error handling improved

# TerminusDB Server v3.0.7 Release Notes

## Bug fixes

+ Fix Windows tray icon error bug

# TerminusDB Server v3.0.6 Release Notes

## Bug fixes

+ Fix subject-object iterator

# TerminusDB Server v3.0.5 Release Notes

This is a feature and bugfix release

## New

+ Add tabling and compilation of schema definitions for faster document access
+ Speed improvements to literal marshalling

## Bug fixes

+ Fix long standing reporting issue on missing classes
+ Increased default stack-size because large packs are stack allocated and can cause stack-overflow on small stacks.
+ Improved determinism in frame code.

# TerminusDB Server v3.0.3 Release Notes

This is a feature and bugfix release

## New

+ Added reverse path queries allowing backwards follow in graph regular expressions for path
+ New word "immediately" which does non-backtracking side-effects
+ Force flag now allows database deletion to work even when database is not finalised
+ Expose additions and removals (deltas) to the WOQL query API

## Bug fixes

+ Add proper handling for rdf:langString

# TerminusDB Server v3.0.0 Release Notes

This is our TerminusDB Server v3 liberation release. We have removed
the masters from our default branching.

## New

+ Reset API allows reseting branch to arbitrary commit
+ Squash API operation now available
+ Default branch is now called main and not master
+ Added much more extensive coverage of API in the api.owl.ttl ontology
+ Fixed some schema errors in woql.owl.ttl
+ Added boolean flag (`all_witnesses`) for returning all or only the first witness from schema checks.
+ Improvements to schema checking. Most large inserts with schema will be 40% faster

## Backwards-Incompatible Changes

+ Default branch will be set to main and not master, so that some
  calls which relied on master being default will fail. This can be
  fixed in all cases by doing a branch operation from master to main.
+ By default only one witness is now returned in WOQL queries in which
  the resulting database violates schema constraints.

## Bug fixes

+ Improved the API for organisation management
+ Improved CORS handling on some calls

# TerminusDB Server v2.0.6 Release Notes

This is largely a bug fix and cleanup release. We focused on improving error handling and code maintanability.

## New

+ Better overall error handling.
+ Extended create db API to allow users to specify whether they want a schema or not.
+ Improved JWT handling for authentication.
+ Added API for role and organisation creation.
+ Extensive work on making capability checking more robust.
+ First draft of auto-generating `.md` files from loaded ontologies.
+ Turtle files can now be read with WOQL.get using "by order of"
  parameter list with three elements.
+ Posted files can now be processed by WOQL.get

## Changes

+ Fixed API endpoint for WOQL.put which allows us to dump to CSV

## Backwards-Incompatible Changes

+ Errors are now largely structured as JSON-LD carrying with them a type which
  is defined in `terminus-schema/api.owl.ttl`. This should make it much easier
  for clients to determine the exact meaning of an error, and allows Error reporting
  to be documented in an ontology.

## Bug fixes

+ Handling of floating point numbers has been improved to stop some spurious type errors.
+ Fixed a utf-8 encoding issue encountered when loading remote csvs.


# TerminusDB Server v2.0.5 Release Notes

## Changes

+ New TerminusDB Console
+ Bug fixes

# TerminusDB Server v2.0.4 Release Notes

## Changes

+ New version of terminusdb-console

# TerminusDB Server v2.0.3 Release Notes

## New

+ Implemented Clone, Push and Pull

## Changes

+ The `TERMINUSDB_SERVER_PUBLIC_URL` environment variable is now ignored. TerminusDB is now fully location-agnostic.
+ Added `TERMINUSDB_SERVER_PACK_DIR` environment variable, determining where swi-prolog packages will be installed.
  This was added to make it easier to offer alternative distributions of TerminusDB in the future.

## Bug fixes

+ CORS handling has been changed to reflect Origin on authenticated requests.

# TerminusDB Server v2.0.2 Release Notes

## New

+ Provide HTTPS support and the ability to set your own certs

## Bug fixes

+ The built-in web console now works on different ports
+ The built-in web console now works on different hosts

# TerminusDB Server v2.0.1 Release Notes

## Bug Fixes

+ Resolution of resource descriptors now attempts to resolve relative
  to the current context before trying to resolve as an absolute path.
+ woql:Size was using a database path predicate which was not imported
  causing it to fail.

# TerminusDB Server v2.0.0 Release Notes

## New

TerminusDB Server now implements databases as a tiered structure which
tracks deltas on collections of graphs. This tiered structure
includes:

+ Meta data graph: Holds information about the local and all remote
  repositories assocated with a given database.
+ Commit Graph: A graph containing information about all commits,
  their authors, a comment, and associated branches.
+ Instance and Schema Graphs: These graphs containing the actual data.

These new structural changes to the underlying store allow users to
perform a number of git-like operations. This includes:

+ Time-travel on databases: You can run queries, browse documents or
  view the schema at any previous commit.
+ Branching: You can branch from any current branch or reference (a
  previous commit).

Several new querying capabilities have been added:

+ Regular path queries allowing the user to query recursively using
  combinations of intermediate predicates resulting in both end-point
  nodes and the particular path as a list of edge objects.
+ Database size and triple count can be queried
+ You can choose a specific resource to query including: the meta-data
  graph, the commit graph, a collection of instance or schema graphs,
  or a particular commit.

## Backwards-Incompatible Changes

+ The storage approach has changed dramatically and so previous
  databases can not be read directly by the new TerminusDB. Upgrades
  require re-ingesting data.

+ The previous version of TerminusDB used JSON as an interchange for
  WOQL ASTs. The current version uses JSON-LD, which is a
  serialisation of RDF. This enables us to store WOQL queries in a
  database and provides a schema documentation of the WOQL query language.

## Bug Fixes

+ Transactional logic in TerminusDB 1.0 had surprising outcomes when
  backtracking over inserts. We currently treat inserts as
  non-backtracking destructive updates.
+ WOQL.when is not required in order to perform updates.
