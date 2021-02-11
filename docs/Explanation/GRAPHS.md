# What with all these graphs? Why have so many?

Each Database in TerminusDB is actually a collection of graph which
are used to keep track of not only your data but important information
*about* your data.

The structure of a database is hierarchical. Each level is a
combination of instance graphs which contain the data for that level,
and schema graphs which contain information about what data is allowed
and how to interpret it and inference graphs which allow us to infer
various facts about the data.

A database is addressed by giving its organisation and its name. For
purposes of illustration, we will assume we are using the `admin`
organisation and the database will be called `foo`. The database can
then be resolved with the descriptor path `admin/foo`.

The hierarchy of graphs is as follows:

- The Repository graphs
- The Commit graphs
- The Branch graphs
- Layers

## The Repository graphs

The repository graphs are necessary for *collaboration*. They help us
in understanding how to communicate with other TerminusDB instances.

The repository graphs keep track of which repositories we know
about. The repository graph is referenced using the designation
`_meta` which contains links with the various repositories.

These repositories are named either with the privileged name `local`
which might be addressed for our example database as `admin/foo/local`
and which refers to the local state of the database, or a remote which
could be given any name, but which might be, for instance,
`admin/foo/origin`.

A local repository in the repository instance data has only the
information about the *layer identifiers* which point to the
associated commit graphs.

A remote repository in the repository instance data has in addition to
the *layer identifiers* the URL of the remote with which we
communicate. This allows us to push and pull from various remotes and
then update our local repository to keep things in sync.

## The Commit graphs

The commit graph gives us *revision* control. It contains the
inforation we need to travel in time, branch, squash, reset and
perform rebase operations.

In order to do this, the commit graphs hold information about the
various branches, and all *commits* which have taken place on these
branches.

A commit graph is referenced using the name `_commits` with the
repository we are interested, for instance:
`admin/foo/local/_commits`. The fact that we track commits for local
and any number of remotes will enable us to sync with different
TerminusDB instances.

Branch objects in the commit graph will point to a commit object.
Each commit object is associated with layers which represent the
branch of interest, as well as potentially containing parent commits,
if the commit has a history.

## The Branch graphs

The Branch graphs are responsible for doing the heavy lifting of
storing our data in a querable way and ensuring correctness of the
data by maintaining a schema.

The branch graph is referred to by the word `branch` followed by an
arbitrary name which we use to refer to the branch of interest, and
which defaults to `main`. For instance, we might have a branch whose
full path is: `admin/foo/local/branch/main`.

## Layers

The lowest level of the hieraharchy is (conceptually) a single graph
which is composed of a sequence of layers. The layers specify each
change which has taken place to the data - including additions and
deletions.

## Transactions

A transaction in the graph is also a hierarchical operation, which
ensures we keep ACID properties. The stages are as follows:

- We first perform an update on the layers of the branch graphs. If
after we have added our new layers, the schema check succeeds then we
obtain take *layer names* of the succesful update (it could have
impacted several graphs at once, so it is plural).
- We write these new layers in as a commit object in the commit instance
graph and move the head of the branch. This results in a new instance
graph for the commit graph.
- We take the layer name for this and write it into the repository
graph so we know the current most recent state of the repository.
- Finally we take the layer name of the repository graph and write the
  newest version into a *label* which is kept in our layer store as a
  named pointer to a layer.

## Conclusion

While somewhat intricate the collection of graphs enables us to have
all of the machinery that one needs for a multi-master *collaboration*
and *revision control* graph database system.
