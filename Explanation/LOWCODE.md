# What do YOU mean by low-code?

The TerminusDB Console is a ReactJS application that provides users with a visual interface for managing and querying TerminusDB.
TerminusDB Console is included when you run TerminusDB itself.

Here we cover areas which includes low code in TerminusDB

## Model Builder Tool

Assuming you have TerminusDB Console installed and ready to run, we can click on the Schema tab of database and click on Schema Builder to get a GUI in which you can build a schema drag and drop tools.

Below example shows how Likes are related to Hotel and Person. We can add properties of different types example string, dateTime, link properties and Enum properties by just button clicks.

![schema_example](https://assets.terminusdb.com/docs/schema-hotel.JPG)

Refer https://terminusdb.com/blog/2020/11/19/model-builder-and-data-modeling/ for further details.

## Document Interface

So now that we have a schema, lets go ahead and create a hotel. Go to Document tab and click on the plus icon to add a new document where you will get a document form which gets the document frames based on schema we created

![document-hotel](https://assets.terminusdb.com/docs/document-hotel.JPG)


You can also drag and drop files and add documents to your database.
Go to Document tab, click on Add Files, you will be getting a file explorer from which you can opt to choose both CSV or JSON documents.

The document API allows you to update the database one document at a time with the update_object, read_object and delete_object WOQL calls.

## Using WOQL to define Documents

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


### TerminusDB Console

Refer to https://terminusdb.github.io/terminusdb/#/Intro_Tutorials/Start_With_Console on further details.
