# Data Modeling

## TerminusDB Graph Basics

TerminusDB organizes data in a very simple structure to make it easy and natural to model the real world.

Whereas traditional relational databases divide data into tables, columns and rows, in TerminusDB everything is an object - objects can have properties and some of these properties may link to other objects. The network of interlinked objects forms a graph structure (in the mathematical sense - nodes and edges).

This is the basic idea of the graph database - using data structures that are things rather than cells allows users to create models that are much closer to the real world things that they want to model - because people perceive the world as things, not cells.

To explain with a very simple example - if we were building a database to hold a family tree and we wanted to record the parents and grandparents of an individual. In a relational database, we might record this in a simple table

![TerminusDB Graph Basics ](/docs/assets/uploads/capture.jpg)

In a graph database, this would look like the following

![TerminusDB Graph Basics ](/docs/assets/uploads/family-tree.png)

The big advantage is that it is much easier to interpret the model and understand how it maps to real world entities compared to the relational example. This increased ease also extends to querying the database.

## [](https://terminusdb.com/docs/getting-started/intro-graph/#query)Query

For example, if we wanted to fetch the name of john’s mother and grandmother from the database, if we were using a relational database, we could use the following two SQL queries to get the name of the mother and grandmother respectively:

```
SELECT Name from TABLE where Person_ID = (SELECT mother from TABLE where Name="John")
SELECT Name from TABLE where Person_ID = (SELECT mother from TABLE WHERE Person_ID = (SELECT mother from TABLE where Name="John"))
```

In a graph database this is much simpler: we can use a triple pattern such as the following to get both names in the same query - we do not have to explicitly join the records together, the joins are implicit - we use the same ID in different parts of the query:

```
WOQL.and(
   WOQL.triple("v:Person", "mother", "v:MotherID"),
   WOQL.triple("v:MotherID", "name", "v:MotherName"),
   WOQL.triple("v:MotherID", "mother", "v:GrandmotherID"),
   WOQL.triple("v:GrandmotherID", "name", "v:GrandmotherName"),
)
```

By using “v:MotherID” multiple times in the query, we create a chain: v:Person =mother=> v:Mother =mother=> v:Grandmother

This makes our queries much easier to understand - we can follow them naturally across multiple patterns.

## [](https://terminusdb.com/docs/getting-started/intro-graph/#classes--properties)Classes & Properties

Under the hood, TerminusDB uses a very rich data modelling language called OWL - the Web Ontology Language. Although it is a very rich language, the basic ideas are simple.

In our schema, we can define classes - which are definitions of types of complex data structure. Classes can be subclasses of other classes, which means that they inherit all the parent’s definitions (much like inheritance in object-oriented programming). We can also define properties in the schema - each property has a **domain** (the class that is the subject of the property) and a **range** - the type of data that the property points to. The range can either be a simple datatype literal (e.g. an integer or string) or it can be a class.

For most use cases, that’s all you really need to know. You define a hierarchy of classes to represent the different types of things that you want to represent in your database, then you define properties to represent the different attributes of these things that you want to record and the relationships between the things. From these simple building blocks - class hierarchies and typed properties, you can build almost arbitrarily complex data structures to represent whatever real world entities you are interested in.

Here are some posts:

1. [Data Modeling: Painting Pictures in the Medium of Pure Abstraction](https://youtu.be/IdsUAiQ6TS8) 

   ![Data Modeling ](/docs/assets/uploads/model.jpg)
2. [Anatomy of a Knowledge Graph](https://www.youtube.com/watch?v=UWEoJVrgJSA)
