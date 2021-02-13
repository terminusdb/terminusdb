# How do I find the object state at a given time?

Given you have a database, (try downloading Seshat from TerminusdbHub)
you can find the object state at a time in the following way:

In Python:

```python

import os
import time
import datetime
from terminusdb_client import WOQLClient
from terminusdb_client import WOQLQuery as WOQL

# Connect to the server
server_url = "https://127.0.0.1:6363"
db = "seshat"
user = "admin"
account = "admin"
key = "root"
client = WOQLClient(server_url)
client.connect(user=user, account=account, key=key, db=db)

# Find the first commit before this date (as a timestamp)
date_string = "14/10/2020"
timestamp = time.mktime(datetime.datetime.strptime(date_string,
                                                   "%d/%m/%Y").timetuple())

# This query gets the commit which is after the timestamp
commit_query = WOQL().using("admin/seshat/local/_commits",
    WOQL().limit(1,
       WOQL.woql_and(
           WOQL().triple("v:Branch", "ref:branch_name", "v2"),
           WOQL().triple("v:Branch", "ref:ref_commit", "v:Head"),
           WOQL().path("v:Head", "ref:commit_parent+", "v:Tail", "v:Path"),
           WOQL().triple("v:Tail", "ref:commit_id", "v:TailID"),
           WOQL().triple("v:Tail",  "ref:commit_timestamp", "v:TimeStamp"),
           WOQL().greater(timestamp, "v:TimeStamp"),
)))

# Extract the commit id
results = client.query(commit_query)
commit_id = results['bindings'][0]['TailID']['@value']

# Read the object from this commit
path = f"admin/seshat/local/commit/{commit_id}"
object_query = WOQL().using(path,
        WOQL().read_object("terminusdb:///data/afghazn", "v:Document"))

results = client.query(object_query)
print(results)
```
