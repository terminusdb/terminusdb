# Closed World Interpretation – Tell Me More?

In semantic web research an open-world interpretation is typically used - this means that we cannot assume that our database holds a complete picture about the entities that it contains - there may be other information out there that we don't know about. 

When we are dealing with a database or knowledge graph that we have constructed to store information about the entities that are of interest to us, the open world assumption is not helpful at all - we need to ensure that our data is coherent and consistent by itself - referential integrity and so on - which is impossible in practice with open world reasoning. 

## Unique Name Assumption

Another aspect of open world reasoning that is used in semantic web research is the absence of the "Unique Name Assumption" - this means that things might have different IDs and still be the same thing. This is useful when we are trying to reason about things that are defined in different places - when we are reasoning about the entries in a particular database which we define, by definition, to be unique, this is not at all helpful. 

Internally in TerminusDB, by contrast, there is a unique name assumption - that is that each entity has exactly one ID. 

## Schema vs Instance

The most important aspect of the closed world assumption relates to the division between schema and instance data. In the semantic web there is no firm distinction - all statements are just logical assertions of one type or another. In practice, all useful systems should separate schema from instance data as the schema should be much more closed than the instance data - schematic stability relative to instance data is required to model anything in an error prone world. 
