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

Where *dbname* and *docid* are the local identifiers of a specific database and document respectively. 

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
Send a HTTP delete to the Database URL

```
curl -X "DELETE" http://localhost/tcre
```

### Return
A terminus result  message 
```
  {"terminus:status":"terminus:success"}
```

## Create Document

POST http://localhost/DBID/document/DOCID

curl -d "@my-doc.json" -X POST http://localhost/dima/document/terminusAPI

```
{
   "@context":
   {
         "doc":"http://195.201.12.87:6363/dima/document/",
         "owl":"http://www.w3.org/2002/07/owl#",
         "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
         "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
         "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "terminus:document":{
        "rdfs:label":[{"@value":"The Terminus API itself","@type":"xsd:string"}],
        "rdfs:comment":[{"@value":"here we go","@type":"xsd:string"}],
        "@type":"http://terminusdb.com/schema/documentation#APIDefinition",
        "@id":"http://localhost/dima/document/terminusAPI"
     },
     "@type":"terminus:APIUpdate",
     "terminus:user_key":"secret"
}
```
## Delete Document
Send a HTTP delete to the Document URL

```
curl -X "DELETE" http://localhost/dima/document/terminusAPI
```

### Return
A terminus result  message 
```
  {"terminus:status":"terminus:success"}
```


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

POST http://localhost/DBID/document/DOCID

curl -d "@my-doc.json" -X POST http://localhost/dima/document/terminusAPI

```
{
      "@context": {
                "doc":"http://localhost/dima/document/",
                "owl":"http://www.w3.org/2002/07/owl#",
                "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
                "xdd":"https://datachemist.net/ontology/xdd#",
                "xsd":"http://www.w3.org/2001/XMLSchema#"
       },
       "terminus:document":{
          "rdfs:label":[{"@value":"The Terminus API itself","@type":"xsd:string"}],
          "rdfs:comment":[{"@value":"here we goaaaa","@type":"xsd:string"}],
          "@type":"http://terminusdb.com/schema/documentation#APIDefinition",
          "@id":"http://localhost/dima/document/terminusAPI"
       },
      "@type":"terminus:APIUpdate",
      "terminus:user_key":"secret"
}
```

## WOQL Select
## WOQL Update
## Get Schema

GET http://localhost/terminus/schema?terminus:encoding=terminus:turtle&terminus:user_key=secret

### Arguments: 
terminus:encoding	terminus:turtle
terminus:user_key	"api key"

### Return
```
"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix 
    ... OWL schema serialised as turtle
"
```

## Update Schema
POST  http://localhost/dima/schema

curl -d "@my-schema.ttl" -X POST http://localhost/dima/document/terminusAPI

### Argument
```
{
            "terminus:turtle": "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                                            ... OWL schema serialised as turtle ...",         
            "terminus:schema":"schema",
            "@type":"terminus:APIUpdate",
            "terminus:user_key":"secret"
}
```
### Return

A terminus result message 

```
  {"terminus:status":"terminus:success"}
```

## Class Frame

terminus:class	https://datachemist.net/ontology/documentation#APIEndpointSpecification
terminus:user_key	root

```
[
  {
    "@context": {
      "dcog":"https://datachemist.net/ontology/dcog#",
      "dcogbox":"https://datachemist.net/ontology/dcogbox#",
      "doc":"http://195.201.12.87:6363/dima/document/",
      "ex":"http://example.org/",
      "owl":"http://www.w3.org/2002/07/owl#",
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
      "rvo":"https://datachemist.net/ontology/rvo#",
      "scm":"http://195.201.12.87:6363/dima/schema/",
      "terminus":"https://datachemist.net/ontology/terminus#",
      "xdd":"https://datachemist.net/ontology/xdd#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "domain":"https://datachemist.net/ontology/documentation#APIEndpointSpecification",
    "frame": {
      "operands": [
	[
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#type",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#summary",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#name",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIReturnValue",
	    "frame": {
	      "elements": [
		{
		  "class":"https://datachemist.net/ontology/documentation#json"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#text"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#jsonld"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#css"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#ttl"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#html"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#owl"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#csv"
		}
	      ],
	      "type":"oneOf"
	    },
	    "property":"https://datachemist.net/ontology/documentation#encoding",
	    "range":"https://datachemist.net/ontology/documentation#DataLanguage",
	    "restriction":"true",
	    "type":"objectProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#description",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  }
	],
	[
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#type",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#summary",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#name",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#description",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  }
	],
	[
	  {
	    "domain":"https://datachemist.net/ontology/documentation#ReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#type",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#ReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#summary",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#ReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#name",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#ReturnValue",
	    "property":"https://datachemist.net/ontology/documentation#description",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  }
	]
      ],
      "type":"class_choice"
    },
    "property":"https://datachemist.net/ontology/documentation#return",
    "range":"https://datachemist.net/ontology/documentation#ReturnValue",
    "restriction": {
      "allValuesFrom":"https://datachemist.net/ontology/documentation#APIReturnValue",
      "property":"https://datachemist.net/ontology/documentation#return",
      "uri":"https://datachemist.net/ontology/documentation#RestrictEndpointReturn"
    },
    "type":"objectProperty"
  },
  {
    "@context": {
      "dcog":"https://datachemist.net/ontology/dcog#",
      "dcogbox":"https://datachemist.net/ontology/dcogbox#",
      "doc":"http://195.201.12.87:6363/dima/document/",
      "ex":"http://example.org/",
      "owl":"http://www.w3.org/2002/07/owl#",
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
      "rvo":"https://datachemist.net/ontology/rvo#",
      "scm":"http://195.201.12.87:6363/dima/schema/",
      "terminus":"https://datachemist.net/ontology/terminus#",
      "xdd":"https://datachemist.net/ontology/xdd#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "domain":"https://datachemist.net/ontology/documentation#APIEndpointSpecification",
    "property":"https://datachemist.net/ontology/documentation#name",
    "range":"xsd:string",
    "restriction":"true",
    "type":"datatypeProperty"
  },
  {
    "@context": {
      "dcog":"https://datachemist.net/ontology/dcog#",
      "dcogbox":"https://datachemist.net/ontology/dcogbox#",
      "doc":"http://195.201.12.87:6363/dima/document/",
      "ex":"http://example.org/",
      "owl":"http://www.w3.org/2002/07/owl#",
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
      "rvo":"https://datachemist.net/ontology/rvo#",
      "scm":"http://195.201.12.87:6363/dima/schema/",
      "terminus":"https://datachemist.net/ontology/terminus#",
      "xdd":"https://datachemist.net/ontology/xdd#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "domain":"https://datachemist.net/ontology/documentation#APIEndpointSpecification",
    "frame": {
      "elements": [
	{"class":"https://datachemist.net/ontology/documentation#get"},
	{"class":"https://datachemist.net/ontology/documentation#put"},
	{"class":"https://datachemist.net/ontology/documentation#post"},
	{
	  "class":"https://datachemist.net/ontology/documentation#delete"
	}
      ],
      "type":"oneOf"
    },
    "property":"https://datachemist.net/ontology/documentation#method",
    "range":"https://datachemist.net/ontology/documentation#HTTPMethod",
    "restriction":"true",
    "type":"objectProperty"
  },
  {
    "@context": {
      "dcog":"https://datachemist.net/ontology/dcog#",
      "dcogbox":"https://datachemist.net/ontology/dcogbox#",
      "doc":"http://195.201.12.87:6363/dima/document/",
      "ex":"http://example.org/",
      "owl":"http://www.w3.org/2002/07/owl#",
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
      "rvo":"https://datachemist.net/ontology/rvo#",
      "scm":"http://195.201.12.87:6363/dima/schema/",
      "terminus":"https://datachemist.net/ontology/terminus#",
      "xdd":"https://datachemist.net/ontology/xdd#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "domain":"https://datachemist.net/ontology/documentation#APIEndpointSpecification",
    "property":"https://datachemist.net/ontology/documentation#headers",
    "range":"xsd:string",
    "restriction":"true",
    "type":"datatypeProperty"
  },
  {
    "@context": {
      "dcog":"https://datachemist.net/ontology/dcog#",
      "dcogbox":"https://datachemist.net/ontology/dcogbox#",
      "doc":"http://195.201.12.87:6363/dima/document/",
      "ex":"http://example.org/",
      "owl":"http://www.w3.org/2002/07/owl#",
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
      "rvo":"https://datachemist.net/ontology/rvo#",
      "scm":"http://195.201.12.87:6363/dima/schema/",
      "terminus":"https://datachemist.net/ontology/terminus#",
      "xdd":"https://datachemist.net/ontology/xdd#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "domain":"https://datachemist.net/ontology/documentation#APIEndpointSpecification",
    "frame": {
      "operands": [
	[
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIArgument",
	    "property":"https://datachemist.net/ontology/documentation#type",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIArgument",
	    "property":"https://datachemist.net/ontology/documentation#summary",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIArgument",
	    "frame": {
	      "elements": [
		{
		  "class":"https://datachemist.net/ontology/documentation#optional"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#mandatory"
		}
	      ],
	      "type":"oneOf"
	    },
	    "property":"https://datachemist.net/ontology/documentation#required",
	    "range":"https://datachemist.net/ontology/documentation#Optionality",
	    "restriction":"true",
	    "type":"objectProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIArgument",
	    "property":"https://datachemist.net/ontology/documentation#name",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIArgument",
	    "frame": {
	      "elements": [
		{
		  "class":"https://datachemist.net/ontology/documentation#json"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#text"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#jsonld"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#css"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#ttl"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#html"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#owl"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#csv"
		}
	      ],
	      "type":"oneOf"
	    },
	    "property":"https://datachemist.net/ontology/documentation#encoding",
	    "range":"https://datachemist.net/ontology/documentation#DataLanguage",
	    "restriction":"true",
	    "type":"objectProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIArgument",
	    "property":"https://datachemist.net/ontology/documentation#description",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#APIArgument",
	    "property":"https://datachemist.net/ontology/documentation#default_value",
	    "range":"xsd:anySimpleType",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  }
	],
	[
	  {
	    "domain":"https://datachemist.net/ontology/documentation#Argument",
	    "property":"https://datachemist.net/ontology/documentation#type",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#Argument",
	    "property":"https://datachemist.net/ontology/documentation#summary",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#Argument",
	    "frame": {
	      "elements": [
		{
		  "class":"https://datachemist.net/ontology/documentation#optional"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#mandatory"
		}
	      ],
	      "type":"oneOf"
	    },
	    "property":"https://datachemist.net/ontology/documentation#required",
	    "range":"https://datachemist.net/ontology/documentation#Optionality",
	    "restriction":"true",
	    "type":"objectProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#Argument",
	    "property":"https://datachemist.net/ontology/documentation#name",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#Argument",
	    "property":"https://datachemist.net/ontology/documentation#description",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#Argument",
	    "property":"https://datachemist.net/ontology/documentation#default_value",
	    "range":"xsd:anySimpleType",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  }
	],
	[
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionArgument",
	    "property":"https://datachemist.net/ontology/documentation#type",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionArgument",
	    "property":"https://datachemist.net/ontology/documentation#summary",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionArgument",
	    "frame": {
	      "elements": [
		{
		  "class":"https://datachemist.net/ontology/documentation#optional"
		},
		{
		  "class":"https://datachemist.net/ontology/documentation#mandatory"
		}
	      ],
	      "type":"oneOf"
	    },
	    "property":"https://datachemist.net/ontology/documentation#required",
	    "range":"https://datachemist.net/ontology/documentation#Optionality",
	    "restriction":"true",
	    "type":"objectProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionArgument",
	    "property":"https://datachemist.net/ontology/documentation#order",
	    "range":"xsd:nonNegativeInteger",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionArgument",
	    "property":"https://datachemist.net/ontology/documentation#name",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionArgument",
	    "property":"https://datachemist.net/ontology/documentation#description",
	    "range":"xsd:string",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  },
	  {
	    "domain":"https://datachemist.net/ontology/documentation#FunctionArgument",
	    "property":"https://datachemist.net/ontology/documentation#default_value",
	    "range":"xsd:anySimpleType",
	    "restriction":"true",
	    "type":"datatypeProperty"
	  }
	]
      ],
      "type":"class_choice"
    },
    "property":"https://datachemist.net/ontology/documentation#argument",
    "range":"https://datachemist.net/ontology/documentation#Argument",
    "restriction": {
      "allValuesFrom":"https://datachemist.net/ontology/documentation#APIArgument",
      "property":"https://datachemist.net/ontology/documentation#argument",
      "uri":"https://datachemist.net/ontology/documentation#RestrictEndpointArgument"
    },
    "type":"objectProperty"
  },
  {
    "@context": {
      "dcog":"https://datachemist.net/ontology/dcog#",
      "dcogbox":"https://datachemist.net/ontology/dcogbox#",
      "doc":"http://195.201.12.87:6363/dima/document/",
      "ex":"http://example.org/",
      "owl":"http://www.w3.org/2002/07/owl#",
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
      "rvo":"https://datachemist.net/ontology/rvo#",
      "scm":"http://195.201.12.87:6363/dima/schema/",
      "terminus":"https://datachemist.net/ontology/terminus#",
      "xdd":"https://datachemist.net/ontology/xdd#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "domain":"https://datachemist.net/ontology/documentation#APIEndpointSpecification",
    "property":"rdfs:label",
    "range":"xsd:string",
    "restriction":"true",
    "type":"datatypeProperty"
  },
  {
    "@context": {
      "dcog":"https://datachemist.net/ontology/dcog#",
      "dcogbox":"https://datachemist.net/ontology/dcogbox#",
      "doc":"http://195.201.12.87:6363/dima/document/",
      "ex":"http://example.org/",
      "owl":"http://www.w3.org/2002/07/owl#",
      "rdf":"http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs":"http://www.w3.org/2000/01/rdf-schema#",
      "rvo":"https://datachemist.net/ontology/rvo#",
      "scm":"http://195.201.12.87:6363/dima/schema/",
      "terminus":"https://datachemist.net/ontology/terminus#",
      "xdd":"https://datachemist.net/ontology/xdd#",
      "xsd":"http://www.w3.org/2001/XMLSchema#"
    },
    "domain":"https://datachemist.net/ontology/documentation#APIEndpointSpecification",
    "property":"rdfs:comment",
    "range":"xsd:string",
    "restriction":"true",
    "type":"datatypeProperty"
  }
]
```
