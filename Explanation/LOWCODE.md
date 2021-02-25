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

## WOQL - no code

We have WOQL queries which we can use to do various db operations such as

### update_object()

Updates a document (or any object) in the db with the passed json-ld

```javascript
let obj = {
    "@id": "doc:joe",
    "@type": "scm:Person",
    "rdfs:label": {
        "@type": "xsd:string",
        "@value": "Joe"
    }
}
update_object(obj)
```

###read_object()

Saves the entire document with IRI DocumentIRI into the JSONLD variable

```javascript
let [mydoc] = vars("mydoc")

read_object("doc:joe", mydoc)
//mydoc will have the json-ld document with ID doc:x stored in it
```

### delete_object()

Deletes the entire refered document and all references to it. You can pass either a full JSON-LD document, an IRI literal or a variable containing either

```javascript
delete_object("doc:joe")
```

### TerminusDB Console

Refer to https://terminusdb.github.io/terminusdb/#/Intro_Tutorials/Start_With_Console on further details.
