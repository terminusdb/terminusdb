# What is this document interface all about? What should I do with it? 

TerminusDB 4.0 has full surfability, clickability and editability of database documents through the console. It is a wiki or catalogue of all your data that you can edit in place.

The document API allows you to update the database one document at a time with the update_object, read_object and delete_object WOQL calls.

## Defining Document Types

Document types are defined in the schema: 

```js
doctype("MyDoc").label("My Document Type").property("age", "integer")
```
Will define a document of type MyDoc with an integer property called "age"

Documents are just made up of triples, so we can add documents just be adding the triples:
```js
add_triple("doc:test", "type", "MyDoc").add_triple("doc:test", "label", "Test Doc").add_triple("doc:test", "age", 23)
```

## read_object

Will create a document of type MyDoc. However, we can also just get the full document  

```js
read_object("doc:test", "v:Document")
```

Will return the jsonld for the full document in the v:Document variable

## update_object

Will create or overwrite a full document in one call by passing the full JSON LD  

```js
update_object({
   "@type": "scm:MyDoc",
   "@id": "doc:test",
   "scm:age": { "@value": 23, "@type": "xsd:integer"}
})
```
Will produce the same document as above (with triples)

## delete_object

```js
delete_object("doc:test")
```
Will delete an entire document, including all properties and embedded objects and links from other documents and objects to the deleted document. 
