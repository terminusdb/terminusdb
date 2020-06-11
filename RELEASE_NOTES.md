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





