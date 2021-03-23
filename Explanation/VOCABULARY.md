# Basic Terminology of TerminusDB

 1. [Triple](#triple)
 2. [Object](#object)
 3. [Document](#document)
 4. [Abstract](#abstract)
 5. [Enum](#enum)
 6. [Cardinality](#Cardinality)
 

# TerminusDB
TerminusDB organises data in a very simple structure to make it easy and natural to model the real world.

Whereas traditional relational databases divide data into tables, columns and rows, in TerminusDB everything is an object - objects can have properties and some of these properties may link to other objects. The network of interlinked objects forms a graph structure (in the mathematical sense - nodes and edges).

# Triple
In TerminusDB every single fragment of information is always and universally stored and accessible as triples. Triples are the universal and fundamental atom of information in TerminusDB - objects and documents and everything else are stored as collections of triples and it is always possible to break any object down in a reliable and deterministic way into a precise set of triples. Not only that, TerminusDB also adheres to the RDF standard, which means all triples have a regular structure and interpretation and strong norms. From a query writer point of view, this regularity is very helpful - you don’t have to care about the low-level structure of the data (what table it is stored in, how many columns there are, etc.), you just have to care about the patterns of meaning you are interested in.

A triple is just a data structure with three slots in which we can put values. Each row in the table below shows a different view of what each of the three slots means.
[Example](/#/Explanation/QUERY?id=rule-1-triples-all-the-way-down)

# Object
Contains a piece of the graph and can be accessed by a unique url. An Object can have multiple properties and can be contained inside other objects. In general you should use Object for data that is owned by a Document and is not shared, or if you need to group Document end Object under the some parent.  

# Document
A Document is a special type of Object. Documents are always top-level objects, never embedded inside other objects, using the Link Property you can link documents to each other. The Documents do not depend on another entity for their existence.

[Details](/#/Explanation/DOCINTERFACE?id=defining-document-types)

# Abstract
An abstract Document or Object is used to group related properties, you can not insert data in an abstract class.

# Enum
Enum or Enumerated is a special data type that enables a property to be a set of predefined constants. The property must be equal to one of the values that have been predefined for it. To use this set of values you’ll need to create an Enum Property and link it to an Emun Element.

# Cardinality
The Cardinality in a property is a measure of the "number of values" for this property in a Document instance, example If a document Person has a property date_of_birth with cardinality 1, means that this property id mandatory and you have to insert 1 value in every document instance.
