# How to load Turtle files

## NOTE on update/load

Update triples will completely replce the contents of the current
graph with the new contents supplied in the turtle file by creating an
appropriate delta. If you want to append to the current graph, simply
replace `update` with `load` (and `update_triples` with
`load_triples`).

## Using the CLI

To load a turtle file *schema* from the command line:

```shell
gavin@titan:~/terminusdb$ ./terminusdb db create admin/woql
gavin@titan:~/terminusdb$ ./terminusdb triples update \
                              admin/woql/local/branch/main/schema/main \
                              src/terminus-schema/woql.owl.ttl
```
Note that we have to specify the full *graph path* of the turtle to load as it will be directly loaded into a specific graph.

To load *instance* data we need to change the graph specifier.

```shell
gavin@titan:~/terminusdb$ cat << EOF > instance.ttl
@prefix woql: <http://terminusdb.com/schema/woql#> .
@prefix doc: <terminusdb:///data/> .

doc:Triple_895b21c8dab64d2856b8281a80cfde8b
  a woql:Triple ;
  woql:object doc:Variable_4d7e113ebfdca7a6052ca6817ac93047 ;
  woql:predicate doc:Variable_b37499f2a3f77a6590c7ae80523ba665 ;
  woql:subject doc:Variable_96dd1292b44ce04cf18b5a608fc997f6 .

doc:Variable_4d7e113ebfdca7a6052ca6817ac93047
  a woql:Variable ;
  woql:variable_name "Z" .

doc:Variable_96dd1292b44ce04cf18b5a608fc997f6
  a woql:Variable ;
  woql:variable_name "X" .

doc:Variable_b37499f2a3f77a6590c7ae80523ba665
  a woql:Variable ;
  woql:variable_name "Y" .
EOF
gavin@titan:~/terminusdb$ ./terminusdb triples update admin/woql/local/branch/main/instance/main instance.ttl
```

## Using Python Client

To load a turtle file schema using the python client:

```python
#!/usr/bin/python3

import os
import sys
import time
import math

from terminusdb_client import WOQLClient
from terminusdb_client import WOQLQuery as WQ

server_url = "https://127.0.0.1:6363"
db = "woql"
db_label = "WOQL"
db_comment = "WOQL Queries"
user = "admin"
account = "admin"
key = "root"

client = WOQLClient(server_url, insecure=True)
client.connect(user=user, account=account, key=key, db=db, insecure=True)

filename = f'src/terminus-schema/woql.owl.ttl'
ttl_file = open(filename)
contents = ttl_file.read()
ttl_file.close()

client.update_triples(
    "schema","main",
    contents,
    f"Adding WOQL schema")
```

Similarly to load into instance data, simply replace `"schema"` with
`"instance"` in the `client.update_triples` call.


