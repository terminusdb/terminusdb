# TerminusDB Schema

The Terminus schemas are a self-documenting, formal description of all of the internal datatypes and classes used by the TerminusDB engine. They cover such aspects as internal configuration; documents; relationships; time, space and geo-temporal scoping of data and values; annotations and provenance; and a range of basic building blocks such as some useful datatypes and classes.  

On top of these core schemas, the TerminusDB schemas define the governance structure of the database itself - they form the schema of the capability database that governs the system. It's schemas all the way down - every part of the server's configuration and saved state has a schema managed by the system, even the capability database. We eat our own dogfood. This makes a lot of sense because we get user-interfaces and forms for free if we use our own system. Using schemas to describe the internal datatypes and structures allows us to much more easily extend and improve our system as we learn more. 

The specific schema files and how they are used by TerminusDB are detailed below. 

## terminus - Terminus Configuration Schema 
terminus.owl.ttl

Contains classes to describe and configure, Agent, User, Role, Capability, Resource, Server, Database, their properties and the relationships between them.  

## vio - Violations Schema
vio.owl.ttl

Contains classes and properties to describe the format and contents of the quality control error messages produced by the system - both schema checks and instance data checks produce structured error messages that can be traced back to specific fragments of data. 

## documentation - Documentation Schema 
documentation.owl.ttl

Contains classes to represent the documentation of the system - Content, API & Function Definitions, arguments, types, etc. 

## xdd - Extended Datatypes 
xdd.owl.ttl

Contains 11 extended datatypes which allows use of new datatypes: uncertainty ranges, email, url, html and json basic datatypes. 

## tbs - Terminus Box Schema 
tbs.owl.ttl

Contains wrapper classes for all basic datatypes to support 'boxing' of datatype properties for annotation and qualification.  This is a solution to the problem whereby, in RDF, we cannot directly annotate datatype values (literals). Boxing is a solution to the problem that keeps the integrity of the basic underlying triple format intact. 

## tcs - Terminus Core Schema 
tcs.owl.ttl

Contains core classes to describe, Document, Entity, Relationship classes, Annotations, Provenance objects, temporal, geographic and geotemporal scopings. Basic building blocks from which sophisticated and complex data structures can be readily constructed. 

## datatypes - Terminus Datatypes Examples
datatypes.owl.ttl

Contains core 
