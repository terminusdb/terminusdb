# So Why is TerminusDB immutable?

TerminusDB is an immutable datastore. This means that when we write
data into the store, we do not mutate the information that is already
there. Instead TerminusDB will mask what was deleted, and create any
new data which is added in a transaction on top of the mask.

Immutability has a number of advantages, including:

- Safety
- Concurrency
- Time Travel
- Auditability
- Collaboration

## Safety

One of the big advantages is safety. If you have something go wrong
during a transaction, this is much less of a problem in an immutable
store. In many cases, even perhaps a hard operating-system crash,
TerminusDB will simply wake back up as though the running transactions
simply had not happened.

## Concurrency

TerminusDB is largely lock free as a result of the immutable
datastructures used to implement it.  The query engine uses optimistic
concurrency which allows transactions to retry if their state was
changed while executing. The lack of locking simplifies the engine and
makes deadlocks very unlikely while giving ACID guarantees.

## Time Travel

The history of the database is all there. That means we can travel
back in time to any commit, or even branch and create a new database
starting at any commit with little difficulty. All of the information
is immediately present, not requiring a "rebuild" of the state of some
past commit.

## Auditabilty

This time travel comes along with information about *who* committed
*what* at what time. This means that we can reliably track
data-provenance which may be of particular interest in regulated
environments.

## Collaboration

The historical commit information is also a necessary pre-requisite
for TerminusDB's collaboration features. The state of two database
which share a common lineage can be compared, such that commits made
by different authors can simply be "re-run" on the current database
using a rebase operation, allowing you to sync state between different
databases.
