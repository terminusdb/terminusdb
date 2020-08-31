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
+ Improvements to schema checking. Most large inserts will be 40% faster

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


# TerminusDB Server v2.0.6 Release Notes

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





