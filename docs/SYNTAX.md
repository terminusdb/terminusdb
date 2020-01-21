# The WOQL query language 


The WOQL query language is a unified model query language. It allows us to treat our database as a document store or a graph interchangeably, and provides powerful query features which make relationship traversals easy.

WOQL's primary syntax and interchange format is in JSON-LD. This gives us a relatively straightforward human-readable format which can also be easily stored in TerminusDB itself. 

```
{"@context" : {"@import": "https://terminusdb/contexts/woql/syntax/context.jsonld",
               "@propagate": true,
               "db" : "http://localhost:6363/testDB004/"},
 "from" : [ "db:main", 
            { "select" : [ "v:Object", "v:Class", "v:Class_Label", "v:Label", "v:Type", 
                           {"and" : [{"triple" : ["v:Object", "rdf:type", "v:Class"] }, 
                                     {"sub" : ["v:Class", "dcog:Entity"] }, 
                                     {"eq" : ["v:Type", {"@value" :"Entity", "@type" : "xsd:string"}]},
                                     {"quad" : ["v:Class", "rdfs:label", "v:Class_Label", "db:schema"]}, 
                                     {"opt" : [{"triple" : ["v:Object", "rdfs:label", "v:Label"]}]}
                                    ]
                           }
                         ]
            }
          ]
}
```

The default imported JSON-LD context at `https://terminusdb/contexts/woql/syntax/context.jsonld"` allows the syntax to be less verbose by automatically adding "@id" to each of the syntactic elements and providing a default WOQL base URI. 

```
{"@context" : 
  { 
      "@version": 1.1,
      "@vocab": "http://terminusdb.com/woql#", 
      "rdf" : "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs" : "http://www.w3.org/2000/01/rdf-schema#",
      "dcog" : "https://datachemist.net/ontology/dcog#",
      "v" : "http://terminusdb.com/woql/variable/",
      "select" : {"@type" : "@id"},
      "from" : {"@type" : "@id"}, 
      "and" : {"@type" : "@id"},
      "triple" : {"@type" : "@id"},
      "eq" : {"@type" : "@id" }, 
      "sub" : {"@type" : "@id" },
      "opt" : {"@type" : "@id"}
      ...
   }
}
```

This demonstrates a simple query which allows us to retrieve all documents in the database, along with their labels and types. 

# Syntax 

## Variables 
`http://terminusdb.com/woql/variable/X`

Where X is some string. The default TerminusDB context allows this to be written as `"v:X"`. 

## Literals

`{ "@value" : Value, "@type" : Type }`
* Value := A string, boolean, or integer
* Type := An xsd datatype IRI

Literals are the basic data-types types and can only be at the object end of an edge.

`{ "@value" : Value, "@lang" : Lang }`
* Value := A string, boolean, integer or variable IRI.
* Lang := A valid IANA code.

A Language specific string. 

## Query And Transaction Terms

The following JSON objects allow the construction of complex queries and 
transactional updates.

### and
`{ "and" : [ Q1, Q2, …, Qn ] }`
* Q_i := A WOQL Query

Attempts to find solutions in which Q_i hold simultaneously. 

#### Example: 
```
{ "@context" : { "doc" : "http://localhost:6363/terminus/document/" },
  "from" : [ 
    "http://localhost:6363/terminus",
    { "and" : [ { "triple" : [ "doc:server", "rdfs:label", "v:Label"] }, 
                { "triple" : [ "doc:server", "rdfs:comment", "v:Comment"] } ]}
  ]
}
```
#### Returns: 

```
{ "bindings": 
   [ { "v:Comment": {"@language":"en", "@value":"The current Database Server itself"}, 
       "v:Label": {"@language":"en", "@value":"The DB server"}
     }
   ]
}

```

### or
`{ "or" : [ Q1, Q2, …, Qn ] }`
* Q_i := A WOQL Query

Attempts to find a solution for any of Q_i hold

#### Example: 
```
{ "@context" : { "doc" : "http://localhost:6363/terminus/document/" },
  "from" : [ 
    "http://localhost:6363/terminus",
    { "or" : [ { "triple" : [ "doc:server", "rdfs:label", "v:Label"] }, 
               { "triple" : [ "doc:server", "rdfs:comment", "v:Comment"] } ]}
  ]
}
```
#### Returns: 

```
{
  "bindings": [
    {
      "v:Comment":"unknown",
      "v:Label": {"@language":"en", "@value":"The DB server"}
    },
    {
      "v:Comment": {"@language":"en","@value":"The current Database Server itself"},
      "v:Label":"unknown"
    }
  ]
}
```

### from
`{ "from" : [ Database, Query ] }`
* Database := A database identifier IRI. 
* Query := A WOQL Query

Sets the current default database for Query. 

```
{"@context" : {scm : "http://localhost:6363/terminus/document#",
               doc : "http://localhost:6363/terminus/document/",
               s : "http://localhost:6363/"},
 "from":["s:terminus",
         {"triple":[ "doc:admin", "rdfs:comment", "v:comment"]}
        ]}
```

#### Returns 

```
{
  "bindings": [
    {
      "v:comment": {"@language":"en", "@value":"This is the server super user account"}
    }
  ],
  "graphs": {}
}

```

### select
`{ "select" : [ Var_List, Query ] }`
* Vars := List of Variable IRIs
* Query := A WOQL Query

Obtain bindings for variables in Var\_List compatible with
Query. Select will mask out any variables not listed in the intial list segment.

#### Example: 
```
{ "@context" : { "doc" : "http://localhost:6363/terminus/document/" },
  "from" : [ 
    "http://localhost:6363/terminus",
    { "select" : [ "v:Label", 
                  { "triple" : [ "v:Server", "terminus:resource_includes", "v:Label"] }
                 ]}
  ]
}
```
#### Returns 

```
{ "bindings" : [ { "v:Label':'doc:terminus'} ] } 
```

### triple
`{ "triple" : [ Subject, Predicate, Object_or_Literal ] }`
* Subject := Var IRI, IRI or JSON-LD
* Predicate := Var IRI, IRI
* Object\_or\_Literal := Var_IRI, IRI, JSON-LD, Literal, or mixed Literal / Var structure.

Find a triple compatible with the current default graph. 

### quad
`{ "quad" : [ Subject, Predicate, Object_or_Literal, Graph_IRI ] }`
* Subject := Var IRI, IRI or JSON-LD
* Predicate := Var IRI or IRI
* Object\_or\_Literal := Var_IRI, IRI, JSON-LD, Literal, or mixed Literal / Var structure.
* Graph\_IRI := A Graph IRI

Find a triple compatible with Graph_IRI.
### sub
`{ "sub" : [ Class_A, Class_B ] }`
* Class\_A := A Class IRI or Variable IRI
* Class\_B := A Class IRI or Variable IRI

The subsumption of Class_A holds of Class\_B.

### eq
`{ "eq" : [ Elt_A, Elt_B ] }`
* Elt\_A := JSON-LD, IRI or Literal
* Elt\_B := JSON-LD, IRI or Literal

Elt\_A is identical to Elt_B or the "@id" fields of the JSON-LDs.

### update
`{ "update" : [ JSON ] }`
* JSON := A JSON-LD document to replace the document at the "@id" given at the top level of the document. 

This is used to update documents in the database as a single operation. 

#### Example:
```
{"@context" : {scm : Scm_Base,
               doc : "http://terminusdb.com/terminus/document/",
               db : "http://terminusdb.com/terminus/",
               rdfs:"http://www.w3.org/2000/01/rdf-schema#",
               s : "http://terminusdb.com/"},
 "from": ["s:terminus",
          {"into": ["db:document",
                      {"when": [{"true": []},
                                {"update": [
                                
                                    {"@id":"doc:admin",
                                     "@type":"terminus:User",
                                     "rdfs:comment":_{"@language":"en", 
                                                      "@value":"A WOQL updated superuser"},
                                     "rdfs:label":{"@language":"en", 
                                                   "@value":"Server Admin User"}
                                     }
                                     
                                           ]}
                               ]}
                     ]}
            ]}
```

#### Returns

```
{"bindings": [ {} ], "graphs": {}}
```

### delete
`{ "delete" : [ JSON_or_IRI ] }`
* JSON\_or\_IRI := A JSON document or IRI of an "@id"of a  document.

Delete the object at the IRI or the IRI in the "@id" of the JSON document. The actual structure of the JSON document is currently completely ignored. 

### delete_triple,
`{ "delete_triple" : [ Subject, Predicate, Object_or_Literal ] }`
`{ "delete_quad" : [ Graph, Subject, Predicate, Object_or_Literal ] }`
* Subject : Subject IRI or Variable IRI
* Predicate : Predicate IRI or Variable IRI
* Object\_or\_Literal : Object, Literal or Variable IRI

Deletes a triple from the graph.

### delete_quad
`{ "delete_quad" : [ Graph, Subject, Predicate, Object_or_Literal ] }`
* Graph : Graph IRI (or the default)
* Subject : Subject IRI or Variable IRI
* Predicate : Predicate IRI or Variable IRI
* Object\_or\_Literal : Object, Literal or Variable IRI

Deletes a quad from the graph.

### add_triple
`{ "add_triple" : [ Subject, Predicate, Object_or_Literal ] }`
* Subject : Subject IRI or Variable IRI
* Predicate : Predicate IRI or Variable IRI
* Object\_or\_Literal : Object, Literal or Variable IRI

Inserts the triple into the given graph IRI (or the default).

### add_quad
`{ "add_quad" : [ Graph, Subject, Predicate, Object_or_Literal ] }`
* Graph : Graph IRI (or the default)
* Subject : Subject IRI or Variable IRI
* Predicate : Predicate IRI or Variable IRI
* Object\_or\_Literal : Object, Literal or Variable IRI
 
### when
`{ "when" : [ Query, Update ] }`
* Query := A WOQL Query
* Update := A WOQL Update

For every Solution to Query, perform Update. "_when Query, then Update_"

### with
```
{ "with" : [ Graph_Name,
             Source_Spec,
             Query
           ]}
```
* Graph\_Name := A string describing the full graph name URI.
* Source\_Spec := A string describing the source using either "file" or "remote".

Allows you to create a temporary graph from a file locally or at a
remote location based on the type of the file and TerminusDB's file
format ontologies. RDF is read in "pure", whereas CSVs are read into a
generic ontology. Query is then executed in a context in which the
graph is available for querying.

### trim
`{ "trim" : [ String , Trimmed_String ] }`
* String := An xsd:string
* Trimmed_String := A trimmed xsd:string

### eval
`{ "eval" : [ Arith, Var_IRI ] }`
* Var_IRI := A WOQL variable IRI
* Arith := An artithmetic statement. 

Evaluate the expression Arith and bind the result to Var_IRI

### isa
`{ "isa" : [ X, Class_IRI ] }`
* X := A variable IRI or IRI
* Class_IRI := A Variable IRI or Class IRI

True when X is of class Class_IRI.

## Arithmetic

The following statements allow arithmetic statements to be built. 

### plus 
`{ "plus" : [ A_1 , A_2, ... A_n ] }`
* A_i := An arithmetic expression.

Add A\_1 to A\_2.

### minus 
`{ "minus" : [ A_1 , A_2, ... A_n ] }`
* A_i := An arithmetic expression.

Subtract A\_2 from A\_1.

### times
`{ "times" : [ A_1 , A_2, ... A_n ] }`
* A\_i := An arithmetic expression.

Multiply A\_1 by A\_2.

### divide
`{ "divide" : [ A_1 , A_2, ... A_n ] }`
* A\_i := An arithmetic expression.

Divide A\_1 by A\_2.

### exp
`{ "exp" : [ A_1 , A_2] }`
* A\_i := An arithmetic expression.


