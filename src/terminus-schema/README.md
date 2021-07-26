# TerminusDB Schema

The Terminus schemas are a self-documenting, formal description of all
of the internal datatypes and classes used by the TerminusDB
engine. They cover such aspects as internal configuration; documents;
relationships; time, space and geo-temporal scoping of data and
values; annotations and provenance; and a range of basic building
blocks such as some useful datatypes and classes.

On top of these core schemas, the TerminusDB schemas define the
governance structure of the database itself - they form the schema of
the capability database that governs the system. It's schemas all the
way down - every part of the server's configuration and saved state
has a schema managed by the system, even the capability database. We
eat our own dogfood. This makes a lot of sense because we get
user-interfaces and forms for free if we use our own system. Using
schemas to describe the internal datatypes and structures allows us to
much more easily extend and improve our system as we learn more.

The specific schema files and how they are used by TerminusDB are
detailed below.

# System (`system_schema.json`)

This file contains definitions regarding databases, organisations and
capabilities. It is used to store metadata about basic system
functionality.

# Repo (`repo.json`)

The repo schema gives information about repositories, whether they are
local or remote and if remote, what their origin is.

# Ref (`ref.json`)

The ref schema contains information about commits, commit metadata,
branches and refs and which commit a branch or ref currently points
to.

# WOQL (`woql.json`)

The WOQL query language syntax is documented in woql.json. This also
makes it possible to save and retrieve queries as structured
documents.
