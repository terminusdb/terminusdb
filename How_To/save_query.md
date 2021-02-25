
## Save a Query

There might be many instances in which it would help to save a query.

Steps:
1. Create query object with a woql query and assign an id to it
2. use update_object() to store this query object into the db
2. use read_object() to retrieve back query object


The below example is constructed in javascript and works under the assumption that Terminus Client is installed and connected.

```javascript

let WOQL=TerminusClient.WOQL

// create a query object
var query ={}
let q=WOQL.triple("v:X", "v:Y", "v:Z")
query=q.query // only take the query object from q
query['@id'] = 'doc:my_triple_query' // add an id to the query object

//saving query
WOQL.update_object(query).execute(nClient)
    .then((results) => {
	console.log("Success in saving query, do something")
    })
    .catch((err) => console.log("Error in saving query, do something"))
    .finally(() => console.log("do something"))

//reading query back
WOQL.read_object('doc:my_triple_query', "v:Query").execute(nClient)
    .then((results) => {
	let my_triple_query = results['bindings'][0]['Query']
	console.log("Printing query", my_triple_query)
    })
    .catch((err) => console.log("Error in reading query, do something"))
    .finally(() => console.log("do something"))
```

Please visit https://github.com/terminusdb/terminusdb-tutorials/tree/master/woql for a working tutorial written in python.
