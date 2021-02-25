# What’s a delta roll up? And why would I need one?

TerminusDB is an append-only database. This means that every time you
do a commit, instead of directly changing your graphs, TerminusDB will
instead create new graphs. These new graphs point to the old graph,
and contain all changes that happened in the commit. This way we
retain the full history of changes, allowing one to roll back to an
arbitrary commit.

The big disadvantage of this though is that over time, this will build
up a pretty long chain of graphs. If you do a hundred commits, that
means that if you wish to query your database, those queries will have
to search through a hundred graphs.

Each commit slows down queries a little bit. This is not a big problem
for a handful of commits, but it does start getting quite noticable
over time. In order to have fast queries, we need some way to ensure
that the chain of graphs that needs to be searched through remains
small.

One way of doing this is through a squash operation. Squash will merge
all those changes into one big layer. The disadvantage though is that
while doing so, it also throws away all the history. This is great
when you don't care for this history, but it is not so great if you
intend to share your database with others, in which case you need to
retain a common history with everyone you share it with in order to be
able to collaborate.

Delta rollups solve this issue by squashing all those layers of
changes into one big layer, while still retaining history. Instead of
replacing your graphs with squashed versions, a delta rollup instead
creates a new layer, and adds a little bit of metadata to the old
layer so that when a load of that layer is requested, the delta rollup
layer is loaded instead.

Furthermore, the rollup layer maintains a few extra data structures to
ensure it remembers values that are unused. What that means is that
for example, if you add a triple with a never-before used value,
delete it in your next commit, then rollup, the rollup layer will not
have a triple for it, but the dictionaries will still know about this
value, ensuring that if a later commit uses this value, it gets
assigned the same ID. This sort of ID stability is very important in
ensuring that the behavior of a rollup layer remains identical to that
of the original layer.

In short, you need a delta rollup when you have a lot of commits, you
wish to retain the history of these commits, and you need queries to
be faster.
