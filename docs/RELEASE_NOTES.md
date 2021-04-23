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
