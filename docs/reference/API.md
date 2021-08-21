# API

API definitions for terminusdb.

### General rule 

The TerminusDB Server HTTP API JSON documents have optional elements notated with angle-brackets, for instance:

```jsx
{
  <"optional" : "foo">,
  "required" : "bar"
}
```

---

## Connect

```
GET http://localhost:6363/api/ -u admin:root
```

The Connect API endpoint returns the `system:User` object associated
with the authentication provided. If no authentication is provided,
the user will be the predefined `terminusdb:///system/data/anonymous`
user.

## Create Database

```
POST http://localhost:6363/api/db/<organization>/<dbid>
```

The post argument is a JSON document of the following form:

```jsx
{ 
   < "prefixes" : { < "doc" : Document_Prefix >,
                   < "scm" : Schema_Prefix > } >
  "label" : "A Label",
  "comment" : "A Comment",
  < "public" : Boolean >,
  < "schema" : Boolean >
}
```
Creates a new database with database ID `dbid` for organization `organization`.

The default prefixes associated with the document and the schema can be specified.


`label`: display name of thre database

`comment`: database description

`label` and `comment` are required fields.


The `public` boolean will determine if this database has read visibility
to the anonymous user. It defaults to false.

The `schema` boolean will determine if this database is created with
an empty schema, or if it is running in "schema free" mode. It
defaults to false.

### Example:

Create a database with the following:
organization: admin
dbid: owl_db
label: label
comment "information about owls"

The payload in this case is: 
 ```jsx
{  
   "prefixes": {"@type": "doc_prefix", 
              "@schema": "schema_prefix"}, 
   "comment" : "information about owls",
   "label" : "owl label" 
 }
 ```

## Delete Database

```
DELETE http://localhost:6363/api/db/<organization>/<dbid>
```
The post argument is a JSON document of the following form

```jsx
{ < "force" : Boolean >
}
```

## Add User to Organization

```
POST http://localhost:6363/api/organization
```
The JSON API post parameter is:

```jsx
{ "organization_name" : Organization_Name,
  "user_name" : User_Name }
```

This endpoint will add the user `User_Name` to the organization
`Organization_Name`.

## Delete Organization

```
POST http://localhost:6363/api/organization
```
The JSON API post parameter is:

```jsx
{ "organization_name" : Organization_Name }
```

This endpoint will delete the organization `Organization_Name`.


## Update Organization Name

```
POST http://localhost:6363/api/organization/<organization_name>
```
The JSON API post parameter is:

```jsx
{ "organization_name" : New_Name }
```

This endpoint will update the name of the organization in the path to `New_Name`.


## Add User

```
POST http://localhost:6363/api/user
```

The JSON API post parameter is:

```jsx
{ "user_identifier" : User_ID,
  "agent_name" : Agent_Name,
  "comment" : Comment,
  <"password" : Password>
}
```

This endpoint adds the user `User_ID` and an organization of the
same name to which the user will automatically be added, along with
an optional password.

## Delete User

```
DELETE http://localhost:6363/api/user/<user_name>
```

This deletes the user named `user_name`.

## Update User

```
POST http://localhost:6363/api/user/<user_name>
```

The JSON API post parameter is:

```jsx
{ <"user_identifier" : User_ID>,
  <"agent_name" : Agent_Name>,
  <"comment" : Comment>,
  <"password" : Passord>
}
```

This endpoint allows a user to be updated with any of the supplied
information in the JSON document.

## Get Roles

```
POST http://localhost:6363/api/role
```
The JSON API post parameter is:

```jsx
{ <"agent_name" : Agent_Name >,
  <"database_name" : Database_Name >,
  <"organisation_name" : Organization_Name >
}
```

This returns all roles in the system which match the passed
parameters.

## Update Roles

```
POST http://localhost:6363/api/update_role
```

The JSON API post parameter is:

```jsx
{ "agent_names" : Agents,
  "organization_name" : Organization,
  "actions" : Actions,
  <"database_name" : Database_Name >
}
```

This endpoint will update the roles in the database with the
associated list of actions for the named agents.


## Clone

```
POST http://localhost:6363/api/clone/<organization>/[<new_dbid>]
```

The JSON payload is:

```jsx
{
   "comment" : Comment,
   "label" : Label,
   "remote_url" : Remote,
   < "public" : Bool >
}
```


The API call creates a new database under the same DB ID as the cloned
database, or with the new database ID `new_dbid` if provided.

The other options are exactly as with create db.

### Example: 
Clone the database `admin/owlid`.

```  
POST http://localhost:6363/api/clone/admin/owlid

```
JSON payload:

```jsx

{
   "comment":"information about owls",
   "label" : "owl label" 
   "remote_url" : "http://remoteurl/"
}
```

## Fetch

```
POST http://localhost:6363/api/fetch/<organization>/<dbid>
```

Fetches new layers from the remotes for this database along with the
commit history.

## Rebase

```
POST http://localhost:6363/api/rebase/<organization>/<dbid>[/<repo>/branch/<branchid>]
```

The JSON API document is:

```jsx
{
   "rebase_from" : Resource,
   "author" : Author,
}
```

The `rebase_from` contains an absolute string descriptor for the reference we are rebasing from. It may be a ref or a branch. Author should be the author of the newly produced commits.

This operation will attempt to construct a new history which has the
same contents as that given by "rebase_from" by repeated application
of diverging commits.

### Example:
If the `user` wants to rebase the database `admin/owlid`, from `branch_a` to `main`, then the argument is:

```
POST "http://127.0.0.1:6363/api/rebase/admin/owlid/local/branch/main"
```
and the payload is:

```jsx
{  
   "rebase_from" : "admin/owlid/local/branch/branch_a",
   "author" : "user@terminusdb.com",

}
```

## Push

```
POST http://localhost:6363/api/push/<organization>/<dbid>[/<repo>/branch/<branchid>/]
```
The JSON API document is:

```jsx
{ "remote" : Remote_Name,
  "remote_branch" : Remote_Branch,
  <"push_prefixes" : Boolean> }
```

This endpoint pushes deltas from the branch specified in the path to
the remote repository with the specified remote from the JSON object.

If `"push_prefixes"` is true, then it will also push the prefixes
associated with the database to the remote.

### Example:
Push the local branch `branch_a` to the branch `main` of the remote repository `owl_information`.
Include the prefixes of the local database and update the prefixes of the remote. 

```
POST http://localhost:6363/api/push/admin/owlid/local/branch/branch_a
```

The JSON payload is:

```jsx
{ "remote" : "owl_information",
  "remote_branch" : "main",
  "push_prefixes" : "True" }
```


## Pull

```
POST http://localhost:6363/api/pull/<organization>/<dbid>[/<repo>/branch/<branchid>/]
```
JSON API document is:

```jsx
{ "remote" : Remote_Name,
  "remote_branch" : Remote_Branch_Name
}
```

Fetch layers from `remote`, then attempt a rebase from the remote branch `remote_branch` onto the local branch specified in the URL.

### Example:
Fetch layers from remote repository: `owl_information`, branch `main` and rebase to the local branch `branch_a`

``` POST http://localhost:6363/api/pull/admin/owlid/local/branch/branch_a
```
The JSON payload is: 

```jsx
{ "remote" : "owl_information",
  "remote_branch" : "main",
}
```


## Branch

```
POST http://localhost:6363/api/branch/<organization>/<dbid>/<repo>/<new_branchid>
```

JSON API document is:

```jsx
{ <"origin" : Remote_Name >
}
```

Creates a new branch as specified by the URI, starting from the branch given by `origin` or empty if it is unspecified.

### Example:
Create a new branch called `branch_a` from the remote repository `owl_information`, branch `main`

``` POST "http://127.0.0.1:6363/api/branch/admin/owlid/local/branch_a" 
```

JSON payload:

```jsx
{ "origin" : "owl_information/main"
}
```

## Delete Branch

```
DELETE http://localhost:6363/api/branch/<organization>/<dbid>/<repo>/<branchid>
```
Post argument is a JSON document of the following form

```jsx
{ < "force" : Boolean >
}
```

### Example:
Delete branch `local/branch_a`.
```
DELETE http://localhost:6363/api/branch/admin/owlid/local/branch_a
```


## Create Graph

```
POST http://localhost:6363/api/graph/<organization>/<dbid>/<repo>/branch/<branchid>/<instance|schema|inference>/<graphid>
```

This takes a post parameter:

```jsx
{"commit_info" : { "author" : Author, "message" : Message }}
```

This API call creates a new graph as specified by the absolute graph descriptor in the URI.

### Example:
Create a graph for `admin/owlid`
```
POST http://localhost:6363/api/graph/admin/owlid/local/main/
```
JSON payload
```jsx
{"commit_info" : { "author" : "user@terminusdb.com", "message" : "created a graph!" }}
```


## Delete Graph

```
DELETE http://localhost:6363/api/graph/<organization>/<dbid>/<repo>/branch/<branchid>/<instance|schema|inference>/<graphid>
```

This takes a post parameter:

```jsx
{"commit_info" : { "author" : Author, "message" : Message }}
```
This API deletes the graph specified by the absolute graph descriptor in the URI.
If multiple graphs are created with different commits as above, the graphid needs to be specified. 


### Example:
delete the graph for: ```admin/owlid```
```
DELETE http://localhost:6363/api/graph/admin/owlid/local/main/
```
JSON payload
```jsx
{"commit_info" : { "author" : "user@terminusdb.com", "message" : "created a graph!" }}
```

## Squash

```
POST http://localhost:6363/api/squash/<organization>/<dbid>/
POST http://localhost:6363/api/squash/<organization>/<dbid>/local/branch/<branchid>
```

This takes a post parameter:

```jsx
{ "commit_info" : Commit_String }
```

This API endpoint allows you to squashes a branch to a single
commit. If the branch is left unspecified, it defaults to
`"local/main"`. The commit created can be attached to an arbitrary branch using ```reset`` (see below).

It returns a json object of the form

```jsx
{ "@type":"api:SquashResponse",
  "api:commit": New_Commit_Path,
  "api:old_commit" : Old_Commit_Path,
  "api:status":"api:success"}
```

This commit path can be used with reset, to add the commit to a specified branch as described above.

### Example:
Squash branch ```local/main``` into a single commit.

```
POST http://127.0.0.1:6363/api/squash/admin/owlid/local/branch/main
```
JSON payload:
```jsx
{"commit_info" : { "author" : "user@terminusdb.com", "message" : "squashed_main" }}
```

JSON return:

```jsx
{ "@type":"api:SquashResponse",
  "api:commit": "local/main",
  "api:old_commit" : "local/main",
  "api:status":"api:success"}
```


## Reset

```
POST http://localhost:6363/api/reset/<organization>/<dbid>/
POST http://localhost:6363/api/reset/<organization>/<dbid>/local/branch/<branchid>
```

This takes a post parameter:

```jsx
{ "commit_descriptor" : Ref }
```

This API endpoint allows you to set a branch to an arbitrary
commit. If the branch is left unspecified, it defaults to `"local/main"`.
The commit descriptor has to be a valid one, for example the return from ```squash``` above. 

### Example:
Set the brach ```local/main``` to the squashed commit specified above.
```
POST http://localhost:6363/api/reset/admin/owl/local/main
```
JSON payload:

```jsx
{ "commit_descriptor" : { "@type":"api:SquashResponse",
                          "api:commit": "local/main",
                          "api:old_commit" : "local/main",
                          "api:status":"api:success"
                        } 
}
```


## Get Triples

```
GET http://localhost:6363/api/triples/<organization>/<dbid>/<repo>/branch/<branchid>/<type>/<name><?format=turtle>
GET http://localhost:6363/api/triples/<organization>/<dbid>/<repo>/commit/<refid>/<type>/<name><?format=turtle>
```

This call returns a "Turtle" format file representation of the graph
specified in the URL path as a JSON string. It takes a get parameter
`format` which, if supplied, must always be "turtle". In the future we
hope to support other formats.

## Replace Triples

```
POST http://localhost:6363/api/triples/<organization>/<dbid>/local/branch/<branchid>/<type>/<name>
```
Post argument is a JSON document of the following form

```jsx
{ "turtle" : TTL_String,
  "commit_info" : { "author" : Author, "message" : Message } }
```

This call creates the update required to make the graph referred to in
the URL have exactly the triples specified in the `turtle` field of
the JSON document. It must be supplied with a commit message (though
it can be an empty string).

## Update Triples

```
PUT http://localhost:6363/api/triples/<organization>/<dbid>/local/branch/<branchid>/<type>/<name>
```
Post argument is a JSON document of the following form

```jsx
{ "turtle" : TTL_String,
  "commit_info" : { "author" : Author, "message" : Message } }
```

This call will simply add the passed triples from the `"turtle"` file
to the graph specified.

## Query

```
POST http://localhost:6363/api/woql
POST http://localhost:6363/api/woql/<organization>/<dbid>
POST http://localhost:6363/api/woql/<organization>/<dbid>/_meta
POST http://localhost:6363/api/woql/<organization>/<dbid>/<repo>
POST http://localhost:6363/api/woql/<organization>/<dbid>/<repo>/_commits
POST http://localhost:6363/api/woql/<organization>/<dbid>/<repo>/branch/<branchid>
POST http://localhost:6363/api/woql/<organization>/<dbid>/<repo>/commit/<refid>
```

Post argument is a JSON document of the following form

```jsx
{ <"commit_info" : { "author" : Author, "message" : Message } >,
  <"all_witnesses" : false >
  "query" : Query }
```

The commit message is a requirement if an update is being made, whereas `query` should be a JSON-LD object as specified by the ontology `woql.owl.ttl`.

If `"all_witnesses"` is false, then the end-point will return
immediately when an schema violation is encountered with the first
witness of failure.

This API call performs a WOQL query and returns an `api:WoqlResponse`
result object, which has the form:

```jsx
{ "@type" : "api:WoqlResponse",
  "api:status" : "api:success",
  "api:variable_names" : Variable_Names,
  "bindings" : Bindings,
  "inserts" : Number_Of_Inserts,
  "deletes" : Number_Of_Deletes,
  "transaction_retry_count" : Retries
  }
```


## Optimize

```
POST http://localhost:6363/api/optimize/_system
POST http://localhost:6363/api/optimize/<organization>/<dbid>
POST http://localhost:6363/api/optimize/<organization>/<dbid>/_meta
POST http://localhost:6363/api/optimize/<organization>/<dbid>/<repo>/_commits
POST http://localhost:6363/api/optimize/<organization>/<dbid>/<repo>/branch/<branch>
```

This API endpoint will attempt to optimize the database using an
appropriate strategy. This call is not recursive, i.e. it will only
optimize access to the respective graph collection specified.

In the case of an unspecified branch, `main` is assumed.
