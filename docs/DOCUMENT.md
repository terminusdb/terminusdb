# A document interface
The document interface consists of two endpoints. The first endpoint, `document`, is how we get documents into and out of TerminusDB. Since schemas consist of documents too, this is also how you'd update the schema.

The second endpoint, `schema`, is how we can easily get schema information out of TerminusDB. While technically it is possible to get all schema information through the document interface, the schema interface is more convenient for this purpose, as it takes class inheritance into account to give a complete image of all the properties that are usable on a certain class.

# The document endpoint

## Getting documents

all document retrieval is done through GET requests on the following endpoint:

```
GET /api/document/<resource path>
```

Where resource path is the usual strings like `admin/foo` for database foo, or `_system` for the system graph, or `admin/foo/_meta` for the metadata graph of the foo database, etc.

By default, this will return a stream of all documents to be found at this location. What exactly is returned can be modified using parameters, which are to be provided as query parameters.

### Parameters

| parameter | default | explanation |
|---|---|---|
| graph_type | | either instance or schema. Used to switch between getting documents from the instance or the schema graph. |
| type | | If given, only documents of the given type are returned. |
| id | | If given, only the document with the given ID is returned. |
| prefixed | true | If true (the default), return IRIs using a prefixed notation wherever possible. If false, full IRIs are used. |
| minimized | false | If true, forego pretty printing, and return the documents with very little whitespace. Each json document will be on its own line. |
| unfold | true | If true (the default), any subdocuments contained in the returned document are returned too. If false, these are referred to by their ID instead. |
| skip | 0 | How many results to skip |
| count | | How many results to return. If this option is absent, all results are returned. |
| as_list | false | If true, don't return a stream of json objects, but a list. This makes parsing the json easier in some environments. |

### Alternate query mechanism

The above table shows parameters that are supposed to be provided as query parameters. There's however another mechanism, where instead, the parameters are passed in as a posted json document. In this calling style, an additional parameter is allowed, `"query"`, by which the returned documents are be filtered by matching against some template.

The alternative method uses a POST rather than a get, specifies the header `X-HTTP-Method-Override: GET`, and posts a json document with the various query parameters instead:

```jsx
{
    "type": "Person",
    "count": 10,
    "query": { "age": 42 },
}
```

The above example would find the first 10 documents of class `Person`, whose age is 42.

This may provide a more convenient style for querying from a library, especially when a (large) query document has to be provided for filtering purposes. However, unlike a pure GET request with query parameters, a POST with a method override does not result in a page that can be bookmarked in a browser. If that is desirable, the GET style is better.

## Posting documents

All new document submission is done through POST requests on the following endpoint:

```
POST /api/document/<resource path>
```

Where resource path is the usual strings like `admin/foo` for database foo, or `_system` for the system graph, or `admin/foo/_meta` for the metadata graph of the foo database, etc.

The documents to be submitted are given as post data. Multiple documents can be specified at once, either as a stream of JSON objects, or as a JSON list containing the documents to be inserted. If a document is specified that already exists, and overwrite is false (the default), an error is returned.

### Parameters
| parameter | default | explanation |
|---|---|---|
| author | | The commit author |
| message | | The commit message |
| graph_type | instance | either instance or schema. Used to switch between submitting to the instance or the schema graph. |
| full_replace | false | If true, all existing documents are deleted before inserting the posted documents. This allows the full replacement of the contents of a database. This is especially useful for replacing the schema. |

### Result
After a successful post, the result will be a list of ids of the newly added documents.

## Replacing documents

Existing documents can be replaced through a PUT request on the following endpoint:

```
PUT /api/document/<resource path>
```

Where resource path is the usual strings like `admin/foo` for database foo, or `_system` for the system graph, or `admin/foo/_meta` for the metadata graph of the foo database, etc.

The documents to be submitted are given as post data. Multiple documents can be specified at once, either as a stream of JSON objects, or as a JSON list containing the documents to be replaced. If a document is specified that does not exist in the database, an error is returned.

### Parameters

| parameter | default | explanation |
|---|---|---|
| author | | The commit author |
| message | | The commit message |
| graph_type | instance | either instance or schema. Used to switch between submitting to the instance or the schema graph. |

## Deleting documents
Existing documents can be deleted through a DELETE request on the following endpoint:

```
DELETE /api/document/<resource path>
```

Where resource path is the usual strings like `admin/foo` for database foo, or `_system` for the system graph, or `admin/foo/_meta` for the metadata graph of the foo database, etc.

### Parameters

| parameter | default | explanation |
|---|---|---|
| author | | The commit author |
| message | | The commit message |
| graph_type | instance | either instance or schema. Used to switch between submitting to the instance or the schema graph. |
| id | | If given, the document to delete. If not given, it is expected that the post data will contain a list of ids to delete. |
| nuke | false | If true, delete everything at this resource location (dangerous!). |

### Specifying what documents to delete
As shown above, deleting a single document can be done through query parameters alone. If multiple documents are to be deleted at once, a document has to be posted of the following format:

```jsx
[ "..id 1..",
  "..id 2..",
  ...
]
```

In other words, a json list of document IDs.

# The schema endpoint
The schema endpoint can be used to query information about classes in a resource. These queries happen through a GET on the following endpoint:

```jsx
GET /api/schema/<resource path>
```

Where resource path is the usual strings like `admin/foo` for database foo, or `_system` for the system graph, or `admin/foo/_meta` for the metadata graph of the foo database, etc.

The purpose of this endpoint is to quickly discover the supported fields of a particular type. The primary envisioned use case for this is automatic generation of forms and other UI elements, as well as client code generation.

## Parameters
| parameter | default | explanation |
|---|---|---|
| type | | If given, the type to get information for. If omitted, information for all types is returned. |

## Result
The result of this GET is a stream of documents describing all the types in a particular resource.

## Switching between schema-checking and schemaless mode
The schema endpoint can also be used to switch between schema checking and schemaless mode.

Switching between checking or not checking does not delete the schema itself. After disabling schema checking it is still possible to update the schema, or to query it through the schema endpoint. However, when disabled, it is possible to submit documents that do not match the schema. Re-enabling schema checking is only possible if all the documents in the given resource match the current schema.

```
POST /api/schema/<resource path>
```

### Parameters
| parameter | default | explanation |
|---|---|---|
| author | | The commit author |
| message | | The commit message |
| schema_checking | | Value should be either enabled or disabled |
