terminusdb -- command line interface to the terminusdb Graph DBMS
=============================================

## SYNOPSIS

`terminusdb [command subcommand positional_arguments] flags`

## DESCRIPTION

The terminusdb(1) command line tool allows users to interact with a database,
allowing common operations such as querying (and updating), database creation,
data ingestion and maintainence.

## COMMANDS

terminusdb accepts a command, sometimes followed by a subcommand. The
accepted commands are given here, along with their arguments.

### help

`terminusdb help`

Display help regarding terminusdb.

  * `-m`, `--markdown`=[value]:
  generate help as markdown

### test

`terminusdb test OPTIONS`

Run internal TerminusDB tests.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for `test` command

  * `-t`, `--test`=[value]:
  Run a specific test

### serve

`terminusdb serve OPTIONS`

Run the TerminusDB server.

  * `-h`, `--help`=[value]:
  Print help for `serve` command

  * `-i`, `--interactive`=[value]:
  Run server in interactive mode

  * `-m`, `--memory`=[value]:
  Run server in-memory, without a persistent store. Takes a password as an optional argument. The in-memory store will be initialized with an admin account with the given password. If absent, the admin account will have 'root' as a password.

### list

`terminusdb list OPTIONS`

List available databases. [DEPRECATED]

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `list` command

  * `-b`, `--branches`=[value]:
  also describe the available branches

  * `-j`, `--json`=[value]:
  Return a JSON as the result of the `list` command

### optimize

`terminusdb optimize DB_SPEC OPTIONS`

Optimize a database (including _system and _meta).

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `optimize` command

### query

`terminusdb query DB_SPEC QUERY OPTIONS`

Query a database.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `query` command

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-o`, `--optimize`=[value]:
  allow query reordering

  * `-j`, `--json`=[value]:
  return results as a json object

### push

`terminusdb push DB_SPEC`

Push a branch.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `push` command

  * `-b`, `--branch`=[value]:
  set the origin branch for push

  * `-e`, `--remote-branch`=[value]:
  set the branch on the remote for push

  * `-r`, `--remote`=[value]:
  the name of the remote to use

  * `-x`, `--prefixes`=[value]:
  send prefixes for database

  * `-t`, `--token`=[value]:
  machine access token

  * `-u`, `--user`=[value]:
  the user on the remote

  * `-p`, `--password`=[value]:
  the password on the remote

### clone

`terminusdb clone URI <DB_SPEC>`

Clone a database (into DB_SPEC).

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `clone` command

  * `-t`, `--token`=[value]:
  machine access token

  * `-u`, `--user`=[value]:
  the user on the remote

  * `-p`, `--password`=[value]:
  the password on the remote

  * `-o`, `--organization`=[value]:
  organizational owner of the cloned database

  * `-l`, `--label`=[value]:
  label to use for this database

  * `-r`, `--remote`=[value]:
  remote to use for this database

  * `-c`, `--comment`=[value]:
  long description of the cloned database

  * `-b`, `--public`=[value]:
  whether the cloned database is to be public

### pull

`terminusdb pull BRANCH_SPEC`

Pull a branch from a database.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `pull` command

  * `-e`, `--remote-branch`=[value]:
  set the branch on the remote for pull

  * `-r`, `--remote`=[value]:
  the name of the remote to use

  * `-t`, `--token`=[value]:
  machine access token

  * `-u`, `--user`=[value]:
  the user on the remote

  * `-p`, `--password`=[value]:
  the password on the remote

### fetch

`terminusdb fetch DB_SPEC`

fetch data from a remote.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `fetch` command

  * `-r`, `--remote`=[value]:
  the name of the remote to use

  * `-t`, `--token`=[value]:
  machine access token

  * `-u`, `--user`=[value]:
  the user on the remote

  * `-p`, `--password`=[value]:
  the password on the remote

### rebase

`terminusdb rebase TO_DATABASE_SPEC FROM_DATABASE_SPEC OPTIONS`

Rebase a database with commits from FROM_DATABASE_SPEC into TO_DATABASE_SPEC.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `rebase` command

  * `-a`, `--author`=[value]:
  The author of the rebase

### squash

`terminusdb squash DATABASE_SPEC OPTIONS`

Squash a commit.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `squash` command

  * `-j`, `--json`=[value]:
  output result status as JSON

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

### rollup

`terminusdb rollup DATABASE_SPEC OPTIONS`

Creates an optimisation layer for queries on the given commit.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `rollup` command

### bundle

`terminusdb bundle DATABASE_SPEC OPTIONS`

Create a pack for a given DATABASE_SPEC that can then be reconsistuted with `terminusdb unbundle`.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `bundle` command

  * `-o`, `--output`=[value]:
  file name to use for pack output file (defaults to descriptor based name).

### unbundle

`terminusdb unbundle DATABASE_SPEC FILE OPTIONS`

Unbundle a bundle file.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `unbundle` command

### diff

`terminusdb diff [Path] OPTIONS`

Create a diff between two JSONs, a JSON and a commit (path required),
or two commits (path required).

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `diff` command

  * `-b`, `--before`=[value]:
  JSON document which is the *before*

  * `-a`, `--after`=[value]:
  JSON document which is the *after*

  * `-k`, `--keep`=[value]:
  Skeleton of the document to retain as context

  * `-c`, `--copy-value`, `--copy_value`=[value]:
  Maintain explit copies of diffs in lists

  * `-d`, `--docid`=[value]:
  document id to use for comparisons

  * `-p`, `--before_commit`, `--before-commit`=[value]:
  Commit or branch of the *before* document(s)

  * `-s`, `--after_commit`, `--after-commit`=[value]:
  Commit or branch of the *after* document(s)

  * `-n`, `--start`=[value]:
  How many diff results to skip before returning (ignored if not comparing resources)

  * `-l`, `--count`=[value]:
  Number of results to return (ignored if not comparing resources)

### apply

`terminusdb apply [Path] OPTIONS`

Apply a diff to path which is obtained from the differences between two commits

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `apply` command

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-k`, `--keep`=[value]:
  Skeleton of the document to retain as context

  * `-t`, `--type`=[value]:
  Variety of commit to create on apply (currently only squash)

  * `-f`, `--match-final-state`, `--match_final_state`=[value]:
  Allow conflicting patch to apply if patch would yield the same final state

  * `-p`, `--before_commit`, `--before-commit`=[value]:
  Commit of the *before* document(s)

  * `-s`, `--after_commit`, `--after-commit`=[value]:
  Commit of the *after* document(s)

### log

`terminusdb log DB_SPEC`

Get the log for a branch given by DB_SPEC.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `log` command

  * `-j`, `--json`=[value]:
  return log as JSON

  * `-s`, `--start`=[value]:
  How far back in commit log to start giving results

  * `-c`, `--count`=[value]:
  Number of results to return

  * `-v`, `--verbose`=[value]:
  Give back additional information on commits

### history

`terminusdb history DB_SPEC`

Get the history for a given document by id in DB_SPEC.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `history` command

  * `-i`, `--id`=[value]:
  The id of the document to provide history for

  * `-j`, `--json`=[value]:
  return history as JSON

  * `-s`, `--start`=[value]:
  How far back in commit history to start giving results

  * `-k`, `--created`=[value]:
  return time of creation (does not report all history)

  * `-u`, `--updated`=[value]:
  return time of last update (does not report all history)

  * `-c`, `--count`=[value]:
  Number of results to return

  * `-v`, `--verbose`=[value]:
  give back schema update information

### reset

`terminusdb reset BRANCH_SPEC COMMIT_OR_COMMIT_SPEC`

Reset the branch at BRANCH_SPEC to the COMMIT_OR_COMMIT_SPEC

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `reset` command

### migration

`terminusdb migration BRANCH_SPEC`

Reset the branch at BRANCH_SPEC to the COMMIT_OR_COMMIT_SPEC

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `migration` command

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-o`, `--operations`=[value]:
  operations to perform on the schema

  * `-t`, `--target`=[value]:
  resource with a schema as migration target

  * `-v`, `--verbose`=[value]:
  give back schema update information

  * `-d`, `--dry_run`=[value]:
  provide information about what would occur if the operations were performed

### concat

`terminusdb concat DB_SPEC`

Concatenate any number of space-separated COMMIT_SPEC or BRANCH_SPEC (provided they are base layers only) passed on standard-input into a commit on DB_SPEC

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `concat` command

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-j`, `--json`=[value]:
  Return a JSON readable commit identifier

### branch create

`terminusdb branch create BRANCH_SPEC OPTIONS`

Create a branch.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `branch create` sub command

  * `-o`, `--origin`=[value]:
  the origin branch to use (false for none)

### branch delete

`terminusdb branch delete BRANCH_SPEC OPTIONS`

Delete a branch.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `branch delete` sub command

### db list

`terminusdb list DB_SPEC [.. DB_SPECN] OPTIONS`

List available databases.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `list` command

  * `-b`, `--branches`=[value]:
  also describe the available branches

  * `-v`, `--verbose`=[value]:
  return lots of metadata

  * `-j`, `--json`=[value]:
  Return a JSON as the result of the `list` command

### db create

`terminusdb db create DATABASE_SPEC OPTIONS`

Create a database.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `db create` sub command

  * `-o`, `--organization`=[value]:
  organizational owner of the database

  * `-l`, `--label`=[value]:
  label to use for this database

  * `-c`, `--comment`=[value]:
  long description of this database

  * `-p`, `--public`=[value]:
  whether this database is to be public

  * `-k`, `--schema`=[value]:
  whether to use a schema

  * `-d`, `--data_prefix`, `--data-prefix`=[value]:
  uri prefix to use for data

  * `-s`, `--schema_prefix`, `--schema-prefix`=[value]:
  uri prefix to use for schema

  * `-x`, `--prefixes`=[value]:
  additional defined prefixes in JSON

### db delete

`terminusdb db delete DATABASE_SPEC OPTIONS`

Delete a database.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `db delete` sub command

  * `-o`, `--organization`=[value]:
  organizational owner of the database

  * `-f`, `--force`=[value]:
  force the deletion of the database (unsafe)

### db update

`terminusdb db update DATABASE_SPEC OPTIONS`

Update a database setting the OPTIONS in an existing database.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `db update` sub command

  * `-l`, `--label`=[value]:
  label to use for this database

  * `-c`, `--comment`=[value]:
  long description of this database

  * `-p`, `--public`=[value]:
  whether this database is to be public

  * `-k`, `--schema`=[value]:
  whether to use a schema

  * `-x`, `--prefixes`=[value]:
  Explicitly defined prefix set (in JSON)

### doc insert

`terminusdb doc insert DATABASE_SPEC OPTIONS`

Insert documents.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `doc insert` sub command

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-g`, `--graph_type`, `--graph-type`=[value]:
  graph type (instance or schema)

  * `-r`, `--require-migration`=[value]:
  require an inferred migration (assuming this is a schema change)

  * `-x`, `--allow-destructive-migration`=[value]:
  allow inferred migration to be destructive (assuming this is a schema change)

  * `-d`, `--data`=[value]:
  document data

  * `-j`, `--raw_json`, `--raw-json`=[value]:
  inserts as raw json

  * `-s`, `--merge_repeats`, `--merge-repeats`=[value]:
  merge repeated documents into a single document record

  * `-f`, `--full_replace`, `--full-replace`=[value]:
  delete all previous documents and substitute these

### doc delete

`terminusdb doc delete DATABASE_SPEC OPTIONS`

Delete documents.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `doc delete` sub command

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-g`, `--graph_type`, `--graph-type`=[value]:
  graph type (instance or schema)

  * `-r`, `--require-migration`=[value]:
  require an inferred migration (assuming this is a schema change)

  * `-x`, `--allow-destructive-migration`=[value]:
  allow inferred migration to be destructive (assuming this is a schema change)

  * `-i`, `--id`=[value]:
  document id to delete

  * `-t`, `--type`=[value]:
  document type to delete

  * `-d`, `--data`=[value]:
  document data

  * `-n`, `--nuke`=[value]:
  nuke all documents

### doc replace

`terminusdb doc replace DATABASE_SPEC OPTIONS`

Replace documents.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `doc replace` sub command

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-g`, `--graph_type`, `--graph-type`=[value]:
  graph type (instance or schema)

  * `-r`, `--require-migration`=[value]:
  require an inferred migration (assuming this is a schema change)

  * `-x`, `--allow-destructive-migration`=[value]:
  allow inferred migration to be destructive (assuming this is a schema change)

  * `-d`, `--data`=[value]:
  document data

  * `-j`, `--raw_json`, `--raw-json`=[value]:
  replace as raw json

  * `-c`, `--create`=[value]:
  create document if it does not exist

### doc get

`terminusdb doc get DATABASE_SPEC OPTIONS`

Query documents.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `doc get` sub command

  * `-g`, `--graph_type`, `--graph-type`=[value]:
  graph type (instance or schema)

  * `-s`, `--skip`=[value]:
  number of documents to skip

  * `-c`, `--count`=[value]:
  number of documents to return

  * `-m`, `--minimized`=[value]:
  return minimized prefixes

  * `-l`, `--as_list`, `--as-list`=[value]:
  return results as a JSON list (as opposed to JSON-lines)

  * `-u`, `--unfold`=[value]:
  include subdocuments, or only subdocument ids

  * `-i`, `--id`=[value]:
  id of document to retrieve

  * `--ids`=[value]:
  list of document ids to retrieve

  * `-t`, `--type`=[value]:
  type of document to retrieve

  * `-z`, `--compress_ids`, `--compress-ids`=[value]:
  return compressed / minimized ids using default prefixes

  * `-q`, `--query`=[value]:
  document query search template

### role create

`terminusdb role create ROLE_NAME ACTION_1 .. ACTION_N OPTIONS`

Create a new role with the listed actions. Actions may be any of:
 "create_database", "delete_database", "class_frame",
 "clone", "fetch", "push",
 "branch", "rebase", "instance_read_access", "instance_write_access",
 "schema_read_access", "schema_write_access", "meta_read_access",
 "meta_write_access", "commit_read_access", "commit_write_access",
 "manage_capabilities"

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `role create` sub command

### role delete

`terminusdb role create ROLE_ID_OR_ROLE_NAME`

Delete a role from the system database

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `role delete` sub command

  * `-i`, `--id`=[value]:
  Interpret argument as a role Id rather than a name.

### role update

`terminusdb role update ROLE_ID_OR_ROLE_NAME ACTIONS OPTIONS`

Update a role from the system database

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `role update` sub command

  * `-i`, `--id`=[value]:
  Interpret argument as a role Id rather than a name.

### role get

`terminusdb role get <ROLE_ID_OR_ROLE_NAME>`

Get a role description from name or id, or all roles if unspecified.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `role get` sub command

  * `-i`, `--id`=[value]:
  Interpret argument as a role id rather than a name.

  * `-j`, `--json`=[value]:
  Return answer as a JSON document

### organization create

`terminusdb organization create ORGANIZATION_NAME`

Create an organization with a given name.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `organization create` sub command

### organization delete

`terminusdb organization delete ORGANIZATION_NAME_OR_ID`

Create an organization with a given name or id.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `organization delete` sub command

  * `-i`, `--id`=[value]:
  Interpret argument as an organization id rather than a name.

### organization get

`terminusdb organization get <ORGANIZATION_NAME_OR_ID>`

Get an organization from its name or id, or list all if unspecified.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `organization get` sub command

  * `-i`, `--id`=[value]:
  Interpret argument as an organization id rather than a name.

  * `-j`, `--json`=[value]:
  Return answer as a JSON document

### user create

`terminusdb user create USER`

Create a user with a given name USER

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `user create` sub command

  * `-p`, `--password`=[value]:
  Specify the password to use for the user

### user delete

`terminusdb organization delete USER`

Delete a user with a given name or ID.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `user delete` sub command

  * `-i`, `--id`=[value]:
  Interpret argument as an organization id rather than a name.

### user get

`terminusdb user get <USER_NAME_OR_ID>`

Get a user from its name or id, or list all if unspecified.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `organization get` sub command

  * `-i`, `--id`=[value]:
  Interpret argument as an organization id rather than a name.

  * `-c`, `--capability`=[value]:
  Report on all capabilities of this user.

  * `-j`, `--json`=[value]:
  Return answer as a JSON document

### user password

`terminusdb user password USER`

Change password for user USER

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `user create` sub command

  * `-p`, `--password`=[value]:
  Specify the password to use for the user

### capability grant

`terminusdb capability grant USER SCOPE ROLE1 <...ROLEN>`

Grant ROLE1 ... ROLEN over SCOPE to USER

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `store init` sub command

  * `-s`, `--scope_type`, `--scope-type`=[value]:
  Should the scope be interpreted as a `database` (default) or an `organization`. If `ids` is specified then the parameters are assumed to be ids rather than names.

### capability revoke

`terminusdb capability revoke USER SCOPE ROLE1 <...ROLEN>`

Revoke ROLE1 ... ROLEN over SCOPE from USER

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `store init` sub command

  * `-s`, `--scope_type`, `--scope-type`=[value]:
  Should the scope be interpreted as a `database` (default) or an `organization`. If `ids` is specified then the parameters are assumed to be ids rather than names.

### store init

`terminusdb store init OPTIONS`

Initialize a store for TerminusDB.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `store init` sub command

  * `-k`, `--key`=[value]:
  key to use for admin login

  * `-f`, `--force`=[value]:
  force the creation of a new store even when one already exists

### triples dump

`terminusdb triples dump GRAPH_SPEC`

Dump an RDF string.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `triples dump` sub command

  * `-f`, `--format`=[value]:
  format of RDF (can be one of: [turtle])

### triples update

`terminusdb triples update GRAPH_SPEC FILE`

Update from an RDF file (replaces current content).

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `triples update` sub command

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-f`, `--format`=[value]:
  format of RDF (can be one of: [turtle])

### triples load

`terminusdb triples load GRAPH_SPEC FILE`

Load triples from RDF file (Appending new).

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `triples load` sub command

  * `-m`, `--message`=[value]:
  message to associate with the commit

  * `-a`, `--author`=[value]:
  author to place on the commit

  * `-f`, `--format`=[value]:
  format of RDF (can be one of: [turtle])

### remote add

`terminusdb remote add DATABASE_SPEC REMOTE_NAME REMOTE_LOCATION OPTIONS`

Add a remote.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `remote add` sub command

### remote remove

`terminusdb remote remove DATABASE_SPEC REMOTE_NAME OPTIONS`

Remove a remote.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `remote remove` sub command

### remote set-url

`terminusdb remote set-url DATABASE_SPEC REMOTE_NAME REMOTE_LOCATION OPTIONS`

Set the URL of a remote.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `remote set-url` sub command

### remote get-url

`terminusdb remote get-url DATABASE_SPEC REMOTE_NAME OPTIONS`

Get the URL of a remote.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `remote get-url` sub command

  * `-r`, `--remote`=[value]:
  the name of the remote to use

### remote list

`terminusdb remote list DATABASE_SPEC OPTIONS`

List remotes.

  * `--impersonate`=[value]:
  impersonate a particular user

  * `-h`, `--help`=[value]:
  print help for the `remote list` sub command

## SYNTAX

The designation of databases, repositories, the associated commit
graph of a database, and various graphs as used in the above command
requires the use of an appropriate descriptor path which is referred to
as the DB_SPEC.

  * `_system`:
  This is the system meta-data, which contains the user information,
  organization information and database records.

  * `<organization>/<database>`:
  This is the most basic descriptor path and actually refers to
  the more specific default path `<organization>/<database>/local/branch/main`.

  * `<organization>/<database>/_meta`:
  This is the repository graph associated with `database`. It contains
  information about the local repository and all known remotes.

  * `<organization>/<database>/<repository>`:
  This is a longer form database designator which specifies which
  respository we would like to address. It refers implicitly to
  `<organization>/<database>/<repository>/branch/main`.

  * `<organization>/<database>/<repository>/_commits`:
  The commit graph associated with a given database. This graph
  contains metadata about branch histories with their commit objects
  including the authorship and time.

  * `<organization>/<database>/<repository>/branch/<branch>`:
  The most specific branch descriptor, allows you to address a
  branch other than main.

  * `<organization>/<database>/<repository>/commit/<commit>`:
  The descriptor which allows an individual commit to be addressed
  directly.

For commands that refer to a GRAPH_SPEC, it should be a DB_SPEC
(specifying the precise branch if a database) followed by one of:

 * `DB_SPEC/instance`

 * `DB_SPEC/schema`

For example:

`terminusdb triples dump admin/people/local/branch/main/schema`

## ENVIRONMENT

  * `TERMINUSDB_SERVER_NAME`:
  Set the servername to use for` terminusdb serve`. Default is
  `127.0.0.1`.

  * `TERMINUSDB_SERVER_PORT`:
  Set the port to use for` terminusdb serve`. Default is `6363`.

  * `TERMINUSDB_SERVER_WORKERS`:
  Set the number of worker threads to use for `terminusdb serve`.
  Default is `8`.

  * `TERMINUSDB_SERVER_MAX_TRANSACTION_RETRIES`:
  Set the transaction retry count. Default is `3`.

  * `TERMINUSDB_SERVER_DB_PATH`:
  Set the location of the storage volume to be used by `terminusdb`
  operations. Can be addressed relative to current path using
  `./`<path>. Default is `./storage/db`.

  * `TERMINUSDB_SERVER_JWT_PUBLIC_KEY_PATH`:
  Set the public key path for JWT. Default is `''`.

  * `TERMINUSDB_SERVER_JWT_PUBLIC_KEY_ID`:
  Set the public key identifier for JWT. Default is `''`.

  * `TERMINUSDB_CONSOLE_BASE_URL`:
  Set the console javascript load URL. Default is
  `https://cdn.terminusdb.com/js_libs/terminusdb_console/dev`.

  * `TERMINUSDB_AUTOLOGIN_ENABLED`:
  If `true` then attempt to login automatically with default
  password. Default is `true`.

  * `TERMINUSDB_SERVER_PACK_DIR`:
  Location of the prolog pack directory if loading third party
  modules.

  * `TERMINUSDB_SERVER_TMP_PATH`:
  Path to use for temporary files.

  * `TERMINUSDB_IGNORE_REF_AND_REPO_SCHEMA`:
  Assume that ref and repo operations are inherently correct.

## EXIT STATUS

  * 0:
  Successfully program execution.  Associated with `api:success`.

  * 1:
  A generic failure of the API to carry out the operation.  Associated
  with `api:failure`.

  * 2:
  The resource which was being requested was not found.  Associated
  with  `api:not_found`.

  * 13:
  The user attempted to carry out an operation without appropriately
  authorizing or without permission.  Associated with
  `api:unauthorized` or `api:forbidden`.

  * 13:
  Unauthorized access attempted.  Associated with `api:unauthorized`.

  * 126:
  An api command was attempted which is not possible.  Associated with
   `api:method_not_allowed`.

  * 131:
  Internal server error. Associated with `api:server_error`.

## EXAMPLES

  * `terminusdb db create admin/foo`:
  Create a database in the organization `admin` with the identifier `foo`.

  * `terminusdb db create admin/foo --label="Foo"`:
  Create a database in the organization `admin` with the identifier
  `foo` and name `Foo`.

## COPYRIGHT

Copyright 2021 TerminusDB

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License. You may
obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied. See the License for the specific language governing
permissions and limitations under the License.

## AUTHORS

Gavin Mendel-Gleason <gavin@terminusdb.com>

Matthijs van Otterdijk <matthijs@terminusdb.com>

Robin de Rooij <robin@terminusdb.com>

Anne Ogborn <anne@swi-prolog.org>

Dmytri Kleiner <dk@terminusdb.com>

[Paulo Moura](https://github.com/pmoura)

Sean Leather <sean@terminusdb.com>
