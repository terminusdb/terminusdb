# Add a date-time property

## In Python

First we'll set up the database:

```python
import os
import time
import datetime
from terminusdb_client import WOQLClient
from terminusdb_client import WOQLQuery as WOQL

# Connect to the server
server_url = "https://127.0.0.1:6363"
db = "test_db"
label = "test db"
description = "test db"
user = "admin"
account = "admin"
key = "root"
client = WOQLClient(server_url)
client.connect(user=user, account=account, key=key, db=db)

try:
    client.create_database(db,user,label=label, description=description)
except Exception as E:
    pass
```

Next we create the schema:

```python
schema = WOQL().doctype("Object", label="Object").property("created_dt", "dateTime", label="created time")
```

Finally we can add a date-time to our date-time property:

```python
client.query(schema)
dt = datetime.datetime.now().strftime("%Y-%m-%dT%H:%M:%S"))

query = WOQL().woql_and(
    WOQL().insert("doc:my_obj","scm:Object"),
    WOQL().add_triple("doc:my_obj", "created_dt", WOQL().datetime(dt)))
client.query(query)
```
