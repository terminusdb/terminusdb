# Getting Started with the Command Line Interface

The TerminusDB command line tool allows you to access much of the
functionality of the API directly from a shell. The complete man page
for the CLI tool is [here](../CLI.md).

The TerminusDB CLI executable `terminusdb` is structured into 'commands' and 'sub
commands'. Assuming that the executable is in your path you can
execute the following:

```shell
$ terminusdb
terminusdb [command]
where command is one of: [branch,bundle,clone,csv,db,fetch,help,list,optimize,pull,push,query,rebase,remote,rollup,serve,store,test,triples,unbundle]
type: terminusdb [command] --help for more details
```

When we use `terminusdb` we will be using the store *IN THE CURRENT
DIRECTORY*. This means we should first move to the directory that we
want to keep our repositories.

```shell
$ mkdir my_database
$ cd my_database
```
When we call `terminusdb` with a command we can see the available
subcommands. For instance:

```shell
$ terminusdb branch
terminusdb branch [subcommand]
where subcommand is one of: [create,delete]
type: terminusdb branch [subcommand] --help for more details
```

# Creating a database

To create a database we use the `db` command with the `create`
subcommand. To get the full list of options you can use `--help`.

```shell
$ ./terminusdb db create --help

terminusdb db create DATABASE_SPEC OPTIONS

Create a database.

--help           -h  boolean=false               print help for the `db create` sub
                                                   command
--organization   -o  term=admin                  organizational owner of the database
--label          -l  atom=                       label to use for this database
--comment        -c  atom=                       long description of this database
--public         -p  boolean=false               whether this database is to be public
--schema         -k  boolean=true                whether to use a schema
--data-prefix    -d  atom=terminusdb:///data/    uri prefix to use for data
--schema-prefix  -s  atom=terminusdb:///schema#  uri prefix to use for schema
--prefixes       -x  atom={}
```

The simplest call with using only defaults would be something like:

```shell
$ terminusdb db create admin/foo
Database admin/foo created
```

We create a database with the current *organisation* as the first
argument. From the command line you have access to `admin` rights, so
you can construct databases in the `admin` organisation.

We can see our databases by using the `list` command:

```shell
$ terminusdb list
TerminusDB
│
└── admin/foo
    └── main
```

This shows us that there is one database named `admin/foo` with the
branch `main`.

We can delete the database with the delete command:

```shell
$ terminusdb db delete admin/foo
Database admin/foo deleted
```

# Creating a branch

We can create a branch in a database with the `branch` command.

```shell
$ terminusdb db create admin/foo
Database admin/foo created
$ ./terminusdb branch create admin/foo/local/branch/new -o admin/foo/local/branch/main
```

When we create a new branch, we need to specify the full path
`<organisation>/<dbid>/<repository>/branch/<branchid>`. (For more on
resource paths see: [Graphs](../Explanation/GRAPHS.md)).

The `local` repository is the one we're interested in, so we use that
in the path. We then specify the name of the new branch we are
creating `new`. Finally, we can either create a new empty branch, or
we can create a branch starting from any commit or branch that
currently exists.

The `-o admin/foo/local/branch/main` part of the command starts are
new branch where `main` leaves off. We will have all of the content
that is in `main` as it currently exists.

# Loading a CSV

We can load a CSV directly into a branch with the `csv` command.

```shell
$ terminusdb csv load admin/foo src/test/test.csv
Successfully loaded CSVs: ['src/test/test.csv']
```

This loads a csv into the `admin/foo` database (which is shorthand for
`admin/foo/local/branch/main`).

We can update the csv by editing the values, and using the `update`
command.

```shell
$ terminusdb csv update admin/foo src/test/test.csv
Successfully loaded CSVs: ['src/test/test.csv']
```

# Querying

It is also possible to query the database from the command line
client. Let's create a new schemaless database and add a few things.

```shell
$ terminusdb db create admin/bar --schema=false
Database admin/bar created
```
Now we can add some data (and we are unconstrained as we have no schema).

```shell
$ terminusdb query admin/bar 'insert(a,b,c)'
```

And finally we can perform a query to obtain the data...

```shell
$ terminusdb query admin/bar 't(X,Y,Z)'
X Y Z
a b c
```
We can combine queries with the `,` operator, for instance:

```shell
% terminusdb query admin/bar 'insert(a,b,c),insert(c,d,e),insert(e,f,g),insert(g,h,i)'
No results
Inserts: 4
```
And then we can query it with:

```shell
$ terminusdb query admin/bar 't(X,Y,Z),t(Z,A,B)'
X Y Z A B
a b c d e
c d e f g
e f g h i
```

This gives us bindings for all two hop paths in our database.
