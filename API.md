# TerminusDB API 

The Terminus DB Server includes a built in HTTP server which implements the Terminus API consisting of 12 endpoints:

1. connect - GET http://terminus.db/  
2. create_database - POST http://terminus.db/dbname  
3. delete_database - DELETE http://terminus.db/dbname 
4. create_document - POST http://terminus.db/dbname/document/docid  
5. get_document - GET http://terminus.db/dbname/document/docid  
6. update_document - POST http://terminus.db/dbname/document/docid  
7. delete_document - DELETE http://terminus.db/dbname/document/docid 
8. woql_select - GET http://terminus.db/dbname/woql  
9. woql_update - POST http://terminus.db/dbname/woql  
10. get_schema - GET http://terminus.db/dbname/schema  
11. update_schema - POST http://terminus.db/dbname/schema  
12. class_frame - GET http://terminus.db/dbname/frame  

where dbname and docid are the local identifiers of a specific database and document respectively. 

## Connect

GET http://terminus.db/
Arguments: terminus:user_key="..."

### Return: terminus:Capability

```
curl "http://localhost?terminus:user_key=secret"

RETURN:

"@context": {
    "doc":"http://localhost/terminus/document/",
    "owl":"http://www.w3.org/2002/07/owl#",
    "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
    "terminus": "http://terminusdb.com/schema/terminus#"
  },
  "@id":"doc:admin",
  "@type":"terminus:User",
  "rdfs:comment": {"@language":"en", "@value":"This is the server super user account"},
  "rdfs:label": {"@language":"en", "@value":"Server Admin User"},
  "terminus:authority": {
    "@id":"doc:access_all_areas",
    "@type":"terminus:ServerCapability",
    "rdfs:comment": {"@language":"en", "@value":"Access all server functions"},
    "rdfs:label": {"@language":"en", "@value":"All Capabilities"},
    "terminus:action": [
      {"@id":"terminus:class_frame", "@type":"terminus:DBAction"},
      {"@id":"terminus:create_database", "@type":"terminus:DBAction"},
      {"@id":"terminus:create_document", "@type":"terminus:DBAction"},
      {"@id":"terminus:delete_database", "@type":"terminus:DBAction"},
      {"@id":"terminus:delete_document", "@type":"terminus:DBAction"},
      {"@id":"terminus:get_document", "@type":"terminus:DBAction"},
      {"@id":"terminus:get_schema", "@type":"terminus:DBAction"},
      {"@id":"terminus:update_document", "@type":"terminus:DBAction"},
      {"@id":"terminus:update_schema", "@type":"terminus:DBAction"},
      {"@id":"terminus:woql_select", "@type":"terminus:DBAction"},
      {"@id":"terminus:woql_update", "@type":"terminus:DBAction"}
    ],
    "terminus:authority_scope": [
      {
        "@id":"doc:dbWhichIAmGoingToDelete",
        "@type":"terminus:Database",
        "rdfs:comment": {"@language":"en", "@value":"dbWhichIAmGoingToDelete"},
        "rdfs:label": {"@language":"en", "@value":"dbWhichIAmGoingToDelete"},
        "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
        "terminus:id": {
          "@type":"xsd:anyURI",
          "@value":"http://localhost/dbWhichIAmGoingToDelete"
        },
        "terminus:instance": {"@type":"xsd:string", "@value":"document"},
        "terminus:schema": {"@type":"xsd:string", "@value":"schema"}
      },
      {
        "@id":"doc:dima",
        "@type":"terminus:Database",
        "rdfs:comment": {
          "@language":"en",
          "@value":"This is a DB created for Dima to test the ontoogies"
        },
        "rdfs:label": {"@language":"en", "@value":"Dima Test DB"},
        "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
        "terminus:id": {
          "@type":"xsd:anyURI",
          "@value":"http://localhost/dima"
        },
        "terminus:instance": {"@type":"xsd:string", "@value":"document"},
        "terminus:schema": {"@type":"xsd:string", "@value":"schema"}
      },
      {
        "@id":"doc:documentation",
        "@type":"terminus:Database",
        "rdfs:comment": {"@language":"en", "@value":"This is the documentation db"},
        "rdfs:label": {"@language":"en", "@value":"Documentation database"},
        "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
        "terminus:id": {
          "@type":"xsd:anyURI",
          "@value":"http://localhost/documentation"
        },
        "terminus:instance": {"@type":"xsd:string", "@value":"document"},
        "terminus:schema": {"@type":"xsd:string", "@value":"schema"}
      },
      {
        "@id":"doc:documentaton",
        "@type":"terminus:Database",
        "rdfs:comment": {"@language":"en", "@value":"This is the documentation db"},
        "rdfs:label": {"@language":"en", "@value":"Documentation database"},
        "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
        "terminus:id": {
          "@type":"xsd:anyURI",
          "@value":"http://localhost/documentaton"
        },
        "terminus:instance": {"@type":"xsd:string", "@value":"document"},
        "terminus:schema": {"@type":"xsd:string", "@value":"schema"}
      },
      {
        "@id":"doc:terminus",
        "@type":"terminus:Database",
        "rdfs:comment": {
          "@language":"en",
          "@value":"The master database contains the meta-data about databases, users and roles"
        },
        "rdfs:label": {"@language":"en", "@value":"Master Database"},
        "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
        "terminus:id": {
          "@type":"xsd:anyURI",
          "@value":"http://localhost/terminus"
        }
      },
      {
        "@id":"doc:test",
        "@type":"terminus:Database",
        "rdfs:comment": {"@language":"en", "@value":"test"},
        "rdfs:label": {"@language":"en", "@value":"test"},
        "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
        "terminus:id": {
          "@type":"xsd:anyURI",
          "@value":"http://localhost/test"
        },
        "terminus:instance": {"@type":"xsd:string", "@value":"document"},
        "terminus:schema": {"@type":"xsd:string", "@value":"schema"}
      },
      {
        "@id":"doc:server",
        "@type":"terminus:Server",
        "rdfs:comment": {
          "@language":"en",
          "@value":"The current Database Server itself"
        },
        "rdfs:label": {"@language":"en", "@value":"The DB server"},
        "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
        "terminus:id": {"@type":"xsd:anyURI", "@value":"http://localhost"},
        "terminus:resource_includes": [
          {
            "@id":"doc:dbWhichIAmGoingToDelete",
            "@type":"terminus:Database"
          },
          {"@id":"doc:dima", "@type":"terminus:Database"},
          {"@id":"doc:documentation", "@type":"terminus:Database"},
          {"@id":"doc:documentaton", "@type":"terminus:Database"},
          {"@id":"doc:dummy", "@type":"terminus:Database"},
          {"@id":"doc:terminus", "@type":"terminus:Database"},
          {"@id":"doc:test", "@type":"terminus:Database"}
        ]
      }
    ]
  }
}
```


## Create Database

Creates a new database with the requested id in the terminus db server

POST http://terminus.db/DBID

```
curl -d "@my-db.json" -X POST http://localhost/tcre/
```
where DBID is the ID of the new DB

### Arguments
POST: Content-Type: application/json

```
{
      "@context":{
           "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
           "terminus":"http://terminusdb.com/schema/terminus#"
      },
      "terminus:document":{
              "@type":"terminus:Database",
              "rdfs:label":{"@language":"en","@value":"Testing Creation"},
              "rdfs:comment":{"@language":"en","@value":"Testing the create API"},
              "terminus:allow_origin":{"@type":"xsd:string","@value":"*"},
              "@id":"http://localhost/tcre"
      },
      "@type":"terminus:APIUpdate",
      "terminus:user_key":"secret"
      } 
}

```

### Return
A terminus result  message 
```
  {"terminus:status":"terminus:success"}
```
## Delete Database
## Create Document
## Delete Document
## Get Document

GET http://localhost/terminus/document/server?terminus:user_key=secret

```
 "@context": {
    "doc":"http://localhost/terminus/document/",
    "owl":"http://www.w3.org/2002/07/owl#",
    "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
    "terminus":"http://terminusdb.com/schema/terminus#",
    "xsd":"http://www.w3.org/2001/XMLSchema#"
  },
  "@id":"doc:server",
  "@type":"terminus:Server",
  "rdfs:comment": {"@language":"en", "@value":"The current Database Server itself"},
  "rdfs:label": {"@language":"en", "@value":"The DB server"},
  "terminus:allow_origin": {"@type":"xsd:string", "@value":"*"},
  "terminus:resource_includes": [
    {"@id":"doc:dbWhichIAmGoingToDelete", "@type":"terminus:Database"},
    {"@id":"doc:dima", "@type":"terminus:Database"},
    {"@id":"doc:documentation", "@type":"terminus:Database"},
    {"@id":"doc:documentaton", "@type":"terminus:Database"},
    {"@id":"doc:dummy", "@type":"terminus:Database"},
    {"@id":"doc:terminus", "@type":"terminus:Database"},
    {"@id":"doc:test", "@type":"terminus:Database"}
  ]
}
```

## Update Document
## WOQL Select
## WOQL Update
## Get Schema
## Update Schema
## Class Frame
