# TerminusDB Schema

TerminusDB has a schema language which allows you to define
specifications for documents and their relationships using a simple
JSON-based syntax.

This syntax is designed to make it as easy as possible to specify a JSON
object which can be converted automatically to a graph. This approach
lets you view your data either as collections of documents or as
knowledge graphs of interconnected objects.

## Schema Objects

A JSON object in the TerminusDB schema is composed of key-value pairs in
which a key is one of the following:

* **keyword** – starts with `@`, has a value with a special meaning
* **property** – does not start with `@`, has a value with a **range**
  type

The full schema definition is a stream or list of these JSON objects.

## Class Definition

The basic unit of specification is the **class**. A class definition is a schema
object with the keyword `@type` whose value is `Class` plus the keyword `@id`
and the name of the class. Here is an example:

```javascript
{ "@type" : "Class",
  "@id" : "Person",
  "name" : "xsd:string" }
```

We define a class named `Person` with one property, `name`, whose type is
`xsd:string`. (See the XSD definitions for more information about types.).

## Context Object

There is one special schema object that affects the entire schema. It is
designated by the special `@type` value `@context`. Here is an example:

```javascript
{ "@type" : "@context",
  "@documentation" : {
      "@title" : "WOQL schema",
      "@description" : "This is the WOQL schema. It gives a complete specification of the 
syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic 
correctness, helps to prevent errors and detect conflicts in merge of queries, and allows 
the storage and retrieval of queries so that queries can be associated with data products.",
      "@authors" : ["Gavin Mendel-Gleason"]
  },
  "@schema" : "http://terminusdb.com/schema/woql#",
  "@base" : "terminusdb://woql/data/",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }
```

We're doing several things here:

1. We document the schema in the `@documentation` value. This includes the
   title, a description, and the list of schema authors.
2. We define default prefixes to be used for the schema and data.
3. We define the prefix `xsd`. This allows us to base our vocabulary on
   different URI prefixes. For example, we can write `xsd:string` to denote
   `http://www.w3.org/2001/XMLSchema#string`.

### Context Prefixes

All properties in the context object that do not start with `@` are considered
to be URI definitions. They must be of the form:

```
Prefix := ":alpha::alphaNum:*"
URI := ":alpha:alphaNum:*://:uriChar:*"

{ ...
 Prefix : URI
 ...}
```

Prefix and URI are defined by the respective regular expressions. That is, a
prefix should be an identifier starting with an alphabetic character followed by
alphanumeric characters. The URI should have a protocol followed by valid URI
characters. Each prefix should be paired with a URI.

### Context Keywords

#### `@schema`

The `@schema` keyword gives the default URI expansion that will be
used for all elements of the schema. For instance, given the following definition:

```javascript
{ "@type" : "@context",
  "@schema" : "http://terminusdb.com/schema/woql#",
  "@base" : "terminusdb://woql/data/" }

{ "@id" : "NamedQuery",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name" ] },
  "name" : "xsd:string",
  "query" : "Query" }
```

The class name `NamedQuery` expands to
`http://terminusdb.com/schema/woql#NamedQuery`.

#### `@base`

The `@base` keyword gives the default URI expansion that will be used
for all elements of instance data. For instance, given the following
schema definition:

```javascript
{ "@type" : "@context",
  "@schema" : "http://terminusdb.com/schema/woql#",
  "@base" : "terminusdb://woql/data/" }

{ "@id" : "NamedQuery",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name" ] },
  "name" : "xsd:string",
  "query" : "Query" }
```

And the following document in the instance graph:

```javascript
{ "@type" : "NamedQuery",
  "@id" : "NamedQuery_my_query",
  "name" : "my_query",
  "query" : { "@type" : "True" }}
```

The id `NamedQuery_my_query` expands to
`terminusdb://woql/data/NamedQuery_my_query`.

#### `@documentation`

The documentation keyword allows you to give documentation which is
global to the entire schema. The object is comprised entirely of
keywords which are described below.

Example:
```javasript
{ "@type" : "@context",
  "@documentation" : {
      "@title" : "WOQL schema",
      "@description" : "This is the WOQL schema. It gives a complete specification of the 
syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic 
correctness, helps to prevent errors and detect conflicts in merge of queries, and allows 
the storage and retrieval of queries so that queries can be associated with data products.",
      "@authors" : ["Gavin Mendel-Gleason"]
  },
  "@schema" : "http://terminusdb.com/schema/woql#",
  "@base" : "terminusdb://woql/data/",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }
```

##### `@title`

The title of the schema which should be displayed.

##### `@description`

The long form description which reveals the purpose of the schema, the
type of documents which should be contained within and any keywords
which are useful to those searching for the type of content that the
schema intends to encode.

##### `@authors`

A list of strings which give the names of all authors involve in
writing the schema.

## Class Keywords

A class definition includes a number of properties as well as the
keywords (beginning with `@`) which describe the class behaviour.

### `@type`

The `@type` keyword specifies the type of the object. At the schema
level this is one of: `Enum`, `Class`, `TaggedUnion` and `Unit`.

#### `Class`

The class designation is a standard class document. It will have some
number of properties defined as well as some number of keywords
describing various class attributes. An example of a class would be as
follows:

```
{ "@id" : "Dog",
  "@type" : "Class",
  "@base" : "Dog_",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name" ] },
  "name" : "xsd:string",
  "hair_colour" : "Colour" }
```

An example instance of this class might be:

```javascript
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s#" }

{ "@type" : "Dog",
  "@id" : "Dog_Cerberus",
  "name" : "Cerberus",
  "hair_colour" : "Grey" }
```

#### `Enum`

An enum is a non-standard class in which each instance is a simple URI
with no additional structure. To be a member of the class you must be
one of the referent URIs.

An `Enum` example class might be:

```javascript
{ "@type" : "Enum",
  "@id" : "PrimaryColour",
  "@values" : ["Red", "Blue", "Yellow"] }
```

An extension might be:

```javascript
"Blue"
```

In the database the actual URI for an Enum is expanded with the type
name preceeding and so the above `Blue` extension can be found as:

```javascript
"http://s#PrimaryColour_Blue"
```

#### `TaggedUnion`

A `TaggedUnion` gives us the ability to provide mutually exclusive
properties. This can be very useful when there is a disjoint choice
between options.

An example of a schema including a `TaggedUnion` is as follows:

```javascript
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s#" }

{ "@id" : "BinaryTree",
  "@type" : "TaggedUnion",
  "@base" : "binary_tree_",
  "@key" : { "@type" : "ValueHash" },
  "leaf" : "sys:Unit",
  "node" : "Node" }

{ "@id" : "Node",
  "@type" : "Class",
  "@key" : { "@type" : "ValueHash" },
  "value" : "xsd:integer",
  "left" : "BinaryTree",
  "right" : "BinaryTree" }
```

Here the `BinaryTree` class gives us a tagged union that allows us to
choose between either a leaf (with no value), or a node class carying
a value and branches.

A concrete extension of this class might be as follows:

```javascript
{ "@type" : "Node",
  "value" : 0,
  "left" : { "@type" : "BinaryTree",
             "leaf" : []},
  "right": { "@type" : "BinaryTree",
             "leaf" : []}}
```

#### `Unit`

The `Unit` type has a single extension, `[]`. This is used when only
the presence of the property is interesting, but it carries no
interesting value.

An example is given for the `BinaryTree` above.

### `@id`

The `@id` key of a class defines the class name and identifier. This is the name which will uniquely define the class and allow the class to be updated, retrieved, and deleted. For instance:

```javascript
{ "@id" : "NamedQuery",
  "@type" : "Class",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "name" ] },
  "name" : "xsd:string",
  "query" : "Query" }
```

Here the class is named `NamedQuery` and because it does not have a fully qualified URL or prefix, it is implicitly based on whatever URI is given for `@schema`.

### `@key`

The `@key` keyword specifies the mechanism which will be used to define the `@id` of *documents* in the database.  This is analogous to a primary key for a database.

The following key types are valid: `Lexical`,`Hash`,`ValueHash`,`Random`.

If the key `@base` is specified in the class, then this will be
pre-pended to the key. If this is a fully qualified URI then it will
be complete. If it is not then it will be combined with the value of
`@base` from the context.

#### `Lexical`

A Lexical key specifies a URI name which is formed from a URI encoded
combination of all `@fields` arguments provided in the order
provided. For instance:

```javascript
{ "@type" : "@context",
  "@schema" : "http://example.com/people#",
  "@base" : "http://example.com/people/" }

{ "@id" : "Person",
  "@type" : "Class",
  "@base" : "Person_",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "first_name", "last_name" ] },
  "first_name" : "xsd:string",
  "last_name" : "xsd:string",
  "year_of_birth" : "xsd:gYear" }
```

With this key strategy a URI will be formed from the combination of
`first_name` and `last_name`.

If `@base` is specified in the class, this will be prepended.

Given the following document:

```javascript
{ "@type" : "Person",
  "first_name" : "Hasdrupal",
  "last_name" : "Barca",
  "year_of_birth" "-245" }
```

This will either generate (if `@id` is not supplied) or check that the URI
`http://example.com/people/Person_Hasdrupal_Barca` is the "@id"
element.

#### `Hash`

Hash is generated exactly as with `Lexical` except that we first hash
the values using the SHA-256 hash algorithm. This can be used if there
are a large number of items that form the key which would make the URI
unwieldly, where there is no need for the URI to inform the user of
the content of the object, or when it is a requirement that data about
the object not be divulged by the key.

It likewise has a `@fields` argument.

```javascript
{ "@type" : "@context",
  "@schema" : "http://example.com/people#",
  "@base" : "http://example.com/people/" }

{ "@id" : "Person",
  "@type" : "Class",
  "@base" : "Person_",
  "@key" : { "@type" : "Hash",
             "@fields" : [ "first_name", "last_name" ] },
  "first_name" : "xsd:string",
  "last_name" : "xsd:string",
  "year_of_birth" : "xsd:gYear" }
```

The document:

```javascript
{ "@type" : "Person",
  "first_name" : "Hasdrupal",
  "last_name" : "Barca",
  "year_of_birth" "-245" }
```

Will generate the id: `Person_5dd7004081e437b3e684075fa3132542f5cd06c1`

#### `ValueHash`

The `ValueHash` key will generate a key which is defined as the
downward transitive closure of the directed acyclic graph from the
root of the document.  In practice this means you can produce a key
which is entirely based on the entire data object.

`ValueHash` takes no additional keywords.

`ValueHash` objects *can not be cyclic* but must instead be
directed acyclic graphs.

The value hash is formed using the SHA-256 hashing algorithm.

```javascript
{ "@id" : "layer:Layer",
  "@type" : "Class",
  "@documentation" : {
      "@comment" : "A layer object which has the identifier used in storage.",
      "@properties" : { "layer:identifier" : "The layer identifier." }
  },
  "@base" : "layer_data:Layer_",
  "@key" : { "@type" : "ValueHash" },
  "layer:identifier" : "xsd:string" }
```

Here the valuehash is formed only from the value of `layer:identifier`.

#### `Random`

If an object has no important characteristics which inform a key, and
it does not need to be constructed such that it is reproducible, then
`Random` can be a convenient key type.

```javascript
{ "@id" : "UserDatabase",
  "@type" : "Class",
  "@documentation" : {
      "@comment" : "A normal user database.",
      "@properties" : { "label" : "The label name of the database.",
                        "comment" : "A comment associated with the database.",
                        "creation_date" : "The time of creation of the database.",
                        "state" : "The system transaction state of the database." }
  },
  "@inherits" : "Database",
  "@key" : { "@type" : "Random" },
  "label" : "xsd:string",
  "comment" : "xsd:string",
  "creation_date" : "xsd:dateTime",
  "state" : "DatabaseState" }
```

Here the key is defined as random meaning that each new database which
is added will be unique regardless of label.

### `@documentation`

The `@documentation` keyword allows the user to add documentation to
the class and to property fields of the class.

The keywords of the documentation object are: `@comment` and `@properties`

An example documentation for a class might be as follows:

```javascript
{ "@id" : "UserDatabase",
  "@type" : "Class",
  "@documentation" : {
      "@comment" : "A normal user database.",
      "@properties" : { "label" : "The label name of the database.",
                        "comment" : "A comment associated with the database.",
                        "creation_date" : "The time of creation of the database.",
                        "state" : "The system transaction state of the database." }
  },
  "@inherits" : "Database",
  "@key" : { "@type" : "Random" },
  "label" : "xsd:string",
  "comment" : "xsd:string",
  "creation_date" : "xsd:dateTime",
  "state" : "DatabaseState" }
```

#### `@comment`

The `@comment` is the class description.

#### `@properties`

The `@properties` keyword is a JSON object with pairs of the form:

```javascript
 { "property_1" : "description_1",
   ...
   "property_n" : "description_n" }
```

### `@base`

The `@base` keyword specifies a prefix to be prepended to the
`@key`. This prefix will be absolute if `@base` is a fully qualified
URI, otherwise it will in turn be prefixed by the system wide `@base`
definition.

```javascript
{ "@type" : "@context",
  "@documentation" : {
      "@title" : "The Ref schema",
      "@description" : "This is the Ref schema. It gives a specification for storage of references, branches and commits in our commit graph.",
      "@authors" : ["Gavin Mendel-Gleason", "Matthijs van Otterdijk"]
  },
  "@base" : "terminusdb://ref/data/",
  "@schema" : "http://terminusdb.com/schema/ref#",
  "layer" : "http://terminusdb.com/schema/layer#",
  "layer_data" : "terminusdb://layer/data/",
  "xsd" : "http://www.w3.org/2001/XMLSchema#" }

{ "@id" : "layer:Layer",
  "@type" : "Class",
  "@documentation" : {
      "@comment" : "A layer object which has the identifier used in storage.",
      "@properties" : { "layer:identifier" : "The layer identifier." }
  },
  "@base" : "layer_data:Layer_",
  "@key" : { "@type" : "ValueHash" },
  "layer:identifier" : "xsd:string" }
```

Here `@base` for the class is fully qualified after the `layer_data`
prefix is expanded. This means that layer URIs will have the form:
`terminusdb://layer/data/Layer_` followed by a random string.

### `@subdocument`

The `@subdocument` key is either present with the value `[]` or it is
not present. If a class is designated as a sub-document then it is
considered to be completely owned by its containing document. Likewise
it is not possible to directly update or delete a subdocument, but it
must be done through the containing document.

Currently subdocuments *must* have a key which is one of either:
`Random` or `ValueHash`. This restriction may be relaxed in the
future.

An example subdocument declaration in a schema is as follows:

```javascript
{ "@type" : "@context",
  "@base" : "terminusdb://i/",
  "@schema" : "terminusdb://s#" }

{ "@type": "Class",
  "@id": "Person",
  "age": "xsd:integer",
  "name": "xsd:string",
  "address" : "Address" }

{ "@type": "Class",
  "@id": "Address",
  "@key": {"@type": "Random"},
  "@subdocument": [],
  "country": "xsd:string",
  "postal_code": "xsd:string",
  "street": "xsd:string"}
```

The corresponding document might look like the following:

```javascript
{ "@type" : "Person",
  "@id" : "doug",
  "name" : "Doug A. Trench",
  "address" : { "@type" : Address",
                "country" : "Neverlandistan",
                "postal_code" : "3",
                "street" : "Cool Harbour lane"}}
```

### `@abstract`

The `@abstract` key is either present with the value `[]` or it is not
present.

When a class is abstract it will have no concrete referents. Instead
it is designed to provide a common super-class and potentially some
number of properties which will be shared by all of its decendents.

Useful concrete members can be created by using the `@inherits`
keyword.

An example of the abstract keyword is given in the following schema:

```javascript
{ "@type" : "@context",
  "@base" : "terminusdb://i/",
  "@schema" : "terminusdb://s#" }

{ "@type": "Class",
  "@abstract": [],
  "@id": "NamedEntity",
  "name": "xsd:string" }

{ "@type" : "Person",
  "@id" : "Person",
  "@inherits" : ["NamedEntity"] }
```

A concrete instance of the `Person` class can be created (but not of
the `NamedEntity` class) and might look as follows:

```javascript
{ "@type" : "Person",
  "@id" : "doug,
  "name" : "Doug A. Trench" }
```

### `@inherits`

The `@inherits` keyword allows classes to inherit properties (and the
`@subdocument` designation) from parent classes. This inheritance tree
is also available as a `subsumption` relation in the WOQL query
language, and provides the semantics for *frames* in the *schema API*.

The range of `@inherits` may be either a class, or a list of
classes. For instance:

```javascript
{ ...,
  "@inherits" : "MyClass",
  ... }
```
Or

```javascript
{ ...,
  "@inherits" : ["MyFirstClass","MySecondClass"]
  ... }
```

TerminusDB allows multiple-inheritence as long as all inherited
properties of the same name have the same range class. If the range
classes conflict then the schema checker will not pass.

An example of inheritence of properties is as follows:

```javascript
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "RightHanded",
  "@type" : "Class",
  "right_hand" : "xsd:string" }

{ "@id" : "LeftHanded",
  "@type" : "Class",
  "left_hand" : "xsd:string" }

{ "@id" : "TwoHanded",
  "@type" : "Class",
  "@inherits" : [ "RightHanded", "LeftHanded"] }
```

An object which meets this specification might look like:

```javascript
{ "@type" : "TwoHanded",
  "@id" : "a two-hander",
  "left_hand" : "Pretty sinister",
  "right_hand" : "But this one is dexterous" }
```

## Class Properties

All non key-words are treated as properties of the class. They have the form:

```javascript
 <property> : <Class>
```
Or

```javascript
 <property> : { "@type" : <TypeFamily>,  "@class" : <Class> }
```

Range classes can be either a concrete base type defined as any of the
of the xsd types (see XSD), or they can be a class defined in the
current schema, including the current class.

An example class might be of the form:

```javascript
{ "@type" : "@context",
  "@schema" : "http://example.com/people#",
  "@base" : "http://example.com/people/" }

{ "@id" : "Person",
  "@type" : "Class",
  "@base" : "Person/",
  "@key" : { "@type" : "Lexical",
             "@fields" : [ "first_name", "last_name" ] },
  "first_name" : "xsd:string",
  "last_name" : "xsd:string",
  "knows" : { "@type" : "Set", "@class" : "Person"},
  "year_of_birth" : "xsd:gYear" }
```

Here `first_name` and `last_name` are strings, `year_of_birth` is a
year, and `friend` is any number of `Person` objects, in no particular
order and without duplication.

A concrete example of a set of documents with this form might be:

```javascript
{ "@type" : "Person",
  "@id" : "Person/Hasdrubal_Barca",
  "first_name" : "Hasdrubal",
  "last_name" : "Barca",
  "knows" :  ["Person/Imilce_Barca","Person/Hannibal_Barca"],
  "year_of_birth" "-245" }

{ "@type" : "Person",
  "@id" : "Person/Imilce_Barca",
  "first_name" : "Imilce",
  "last_name" : "Barca",
  "knows" :  ["Person/Hasdrupal_Barca","Person/Hannibal_Barca"],
  "year_of_birth" "-255" }

{ "@type" : "Person",
  "@id" : "Person/Hannibal_Barca"
  "first_name" : "Hannibal",
  "last_name" : "Barca",
  "knows" : ["Person/Imilce_Barca","Person/Hannibal_Barca"],
  "year_of_birth" "-247" }
```

## Type Families

The following type families are defined: `List`, `Set`, `Array` and
`Optional`. These type families allow us to construct optionality or
collections of values.

### `Optional`

In the case where a property is not required, you can supply
`Optional` as a type family.

```javascript
{ "@type" : "@context",
  "@schema" : "http://example.com/people#",
  "@base" : "http://example.com/people/" }

{ "@type" : "Class",
  "@id" : "CodeBlock",
  "code" : "xsd:string",
  "comment" : { "@type" : "Optional",
                 "@class" : "xsd:string" }}
```

Here we can supply an optional `comment` field on our `CodeBlock`.
Both of the following documents are valid:

```javascript
{ "@type" : "CodeBlock",
  "@id" : "my_code_block",
  "code" : "print('hello world')",
  "comment" : "This is a silly bit of code" }
```
OR

```javascript
{ "@type" : "CodeBlock",
  "@id" : "my_code_block",
  "code" : "print('hello world')" }
```

### `List`

The `List` definition specifies an ordered collection, with
multiplicity, of values of either a class or datatype.

```javascript
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "TaskList",
  "@type" : "Class",
  "tasks" : { "@type" : "List",
              "@class" : "Task" } }

{ "@id" : "Task",
  "@type" : "Class",
  "@key" : "ValueHash",
  "name" : "xsd:string" }
```

Here we have an object, a `Task` which can be contained in a list of
elements known as a `TaskList`. The form an example object might take would be:

```javascript
{ "@id" : "my_task_list",
  "@type" : "TaskList",
  "tasks" : [{ "@type": "Task", "name" : "Laundry" },
             { "@type": "Task", "name" : "Take_Garage_Out" }
            ]}
```

This list will always be retrieved with the same order that it is
inserted. It is also be capable of storing duplicates.

### `Set`

The `Set` definition specifies an unordered set of values of either a
class or datatype.

```javascript
{ "@type" : "@context",
  "@base" : "http://i/",
  "@schema" : "http://s/" }

{ "@id" : "Person",
  "@type" : "Class",
  "name" : "xsd:string",
  "friends" : { "@type" : "Set",
                "@class" : "Person" } }
```

Here we have an object, a `Person` which can have any number
(including zero) of friends.

```javascript
{ "@id" : "Me",
  "@type" : "Person",
  "tasks" : [{ "@type": "Person",
               "@id" : "you",
               "name" : "You" },
             { "@type": "Person",
               "@id" : "someone_else",
               "name" : "Someone Else" }
            ]}
```

This list has no order, and may come out of the database in a
different order. If duplicates are inserted they will not create
additional linkages, and only a single of the multply supplied results
will be returned.
