# What does ACID mean for TerminusDB?

ACID (atomicity, consistency, isolation, durability) is a set of
properties of database transactions that are generally considered
desirable for many applications.

# Atomicity

Atomicity means that we don't do things by half. If you start your
transaction and don't finish it, then you get no change at all. And if
you finish your transaction you're absolutely guaranteed not to have
only part of it available next time you look. TerminusDB uses
(immutability)[IMMUTABLE.md] combined with a single atomic operation
which "moves the head" of the Database to ensure this is the case.

# Consistency

Consistency is often the most confusing part of ACID since it doesn't
actually just mean one thing. Different kinds of consistency can be
maintained using different approaches. As it turns out, TerminusDB
actually has two notions of consistency, one weaker, and one stronger.

In all cases, if there is a schema, the database will not complete a
transaction unless the schema conditions are satified. That is,
consistency with respect to the schema is always maintained under all
conditions.

When running a query on the database, TerminusDB will always present a
consistent view of the database to the query which is a snapshot of
the database at the point the query started. If anything should change
prior to the transaction completing, then the transaction will re-run,
ensuring that no deviation from the read-state exists at the point of
commit.

When rebasing it is possible for transactions which had formerly
completed under certain read-conditions to be "replayed" by simply
reordering their commits. In this case the schema consistency is still
maintained but we will not have the consistency conditions which
obtain on queries running on the same database.

# Isolation

Isolation is the property in which the Database appears to its user
*semantically* as though they the only ones using it. That is, when a
transaction runs, when it completes the user will have the sense that
it occurred as though there was no concurrency. Nothing in the state
is interleved and any other users will also obtain the same impression.

TerminusDB uses the immutable nature of the database to ensure that
every read query exists at a given layer, and everyone gets a very
literal "snapshot" isolation.

Similarly completing write transactions ensure isolation with
optimistic concurrency, simply restarting transactions which might
have changed mid-run.

# Durability

TerminusDB is very durable. Even transactions being stopped mid-swing
are very unlikely to lead to any sort of corruption, and data
corruption that does not involve damage to the actual information
storage or that caused by operating system bugs, is very unlikely. In
the event of some partial commit, the previous layer remains
completely unchanged and thereby recoverable. This also significantly
simplifies backups - which need only copy storage in order to ensure a
safely recoverable state.
