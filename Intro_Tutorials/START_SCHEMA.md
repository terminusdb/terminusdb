# Creating a Schema

The easiest way to create a schema in TerminusDB is by using a WOQL
query.

First thing, you should create a database in which to put the
schema. You can do this through the console.

Our schema is comprised of classes and properties. WOQL will allow us
to construct them with the `WOQL.doctype` keyword.

```javascript
WOQL.doctype("Person")
  .label("Person")
  .description("A Person with person-like properties")
  .property("forename", "xsd:string")
  .property("surname", "xsd:string")
  .property("age", "xsd:integer")
```

This object has 3 properties, with three different datatypes. It is
essentially a simple record.

The data which can be put into this object should look something like
the following:

```javascript
WOQL.and(
  WOQL.add_triple("doc:jim_martin", "type", "scm:Person"),
  WOQL.add_triple("doc:jim_martin", "forename", "Jim"),
  WOQL.add_triple("doc:jim_martin", "surname", "Martin"),
  WOQL.add_triple("doc:jim_martin", "age", 38)
)
```

This query will insert a valid record for our class above. As you can
see we are only pointing at datapoints at the moment, but we can also
have properties point to other objects.

## Graph Schemas

We can go ahead and modify the previous query by adding a
`scm:friends_with` field. As long as we're only adding things, there
is no problem repeating assertions we've already made. So we'll just
copy the person from above and try again.

```javascript
WOQL.doctype("Person")
  .label("Person")
  .description("A Person with person-like properties")
  .property("forename", "xsd:string")
  .property("surname", "xsd:string")
  .property("age", "xsd:integer")
  .property("friends_with", "Person")
```

You can go ahead and go to the Schema button at the top, and you'll
see that there is still only one Person class. All we did is suggest
that the "friends_with" property should be added.

Now we can add a couple of people and have them point to each other.

```javascript
WOQL.and(
  WOQL.add_triple("doc:sarah_cohen", "type", "scm:Person"),
  WOQL.add_triple("doc:sarah_cohen", "forename", "Sara"),
  WOQL.add_triple("doc:sarah_cohen", "surname", "Cohen"),
  WOQL.add_triple("doc:sarah_cohen", "age", 42),
  WOQL.add_triple("doc:sarah_cohen", "friends_with", "doc:jim_martin"),
  WOQL.add_triple("doc:jim_martin", "friends_with", "doc:sarah_cohen")
)
```

Our schema as designed allows us to control the types of fields. We
can see this by trying to put in some data which is not correct
according to the schema.


```javascript
WOQL.add_triple("doc:sarah_cohen", "forename", "doc:jim_martin")
```

This gives us a predictable error that jim is not a string.

However, as `scm:Person` is currently written we do not constraint the
*number* of times a property is used. For instance we can write:


```javascript
WOQL.and(
  WOQL.add_triple("doc:jim_martin", "type", "scm:Person"),
  WOQL.add_triple("doc:jim_martin", "forename", "Jim"),
  WOQL.add_triple("doc:jim_martin", "forename", "James"),
  WOQL.add_triple("doc:jim_martin", "surname", "Martin"),
  WOQL.add_triple("doc:jim_martin", "age", 38)
)
```

## Cardinality

Jim now has two names. Maybe we want this, but perhaps we want a
formal legal forename, and we constraint his aliases to something
called `scm:nick` for instance.

Let's rewrite our person to add cardinality restrictions on the
properties.

```javascript
WOQL.doctype("Person")
  .label("Person")
  .description("A Person with person-like properties")
  .property("forename", "xsd:string").cardinality(1)
  .property("surname", "xsd:string").cardinality(1)
  .property("nickname", "xsd:string")
  .property("age", "xsd:integer").cardinality(1)
  .property("friends_with", "Person")
```

Now we have an unconstrained "scm:friends_with" relationship and any
number of nicknames on the "scm:nickname" property but we can only
have one of each of the other properties.

## Schema migration

Unfortunately, this query results in an error because we have data
already in our system which does not conform to our schema changes. We
need to alter the data to make it conform. We can simply remove the
offending forename in the same stroke that we add the schema change.

```javascript
WOQL.and(
  WOQL.delete_triple("doc:jim_martin", "forename", "James"),
  WOQL.doctype("Person")
    .label("Person")
    .description("A Person with person-like properties")
    .property("forename", "xsd:string").cardinality(1)
    .property("surname", "xsd:string").cardinality(1)
    .property("nickname", "xsd:string")
    .property("age", "xsd:integer").cardinality(1)
    .property("friends_with", "Person")
)
```

Now we have added cardinality and removed the violation in a single
transaction.

## Creating hierarchies

So far, we have a very flat structure. Let's add a subclass for
`scm:Person` called `scm:Doctor`.

```javascript
WOQL.doctype("Doctor")
  .parent("Person")
  .label("Doctor")
  .description("A Doctor is a person with patients")
  .property("patient", "Person")
```

A doctor shares all of the properties available to a Person, but it
also has a patient. Let us add a Doctor.

```javascript
WOQL.and(
  WOQL.add_triple("doc:sigmund_freud", "type", "scm:Doctor"),
  WOQL.add_triple("doc:sigmund_freud", "forename", "Freud"),
  WOQL.add_triple("doc:sigmund_freud", "surname", "Sigmund"),
  WOQL.add_triple("doc:sigmund_freud", "age", 51),
  WOQL.add_triple("doc:sarah_cohen", "friends_with", "doc:sigmund_freud"),
  WOQL.add_triple("doc:sigmund_freud", "friends_with", "doc:sarah_cohen"),
  WOQL.add_triple("doc:sigmund_freud", "patient", "doc:jim_martin")
)
```

This saved us a lot of time as we factored out the joint qualities of
a person first. You can make any sort of hierarchy without worrying
about multiple inheritence restrictions that often occur in other
systems.

Good luck schema-writing!

