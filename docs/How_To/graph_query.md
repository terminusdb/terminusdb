# Graph Query

## Shortest Path

Shortest path between an object `a` and an object `b` via a
predicate `p` can achieved with the use of a *path query*.

This example uses the DBPedia database which can be cloned from
TerminusHub. It gives the shortest path between band members of
Whitesnake, bands that these members have been in, the members of
these bands, the bands that these members have been in, etc. until
finally we arrive at a band which plays Country music (a 6 hop path).

In Python:

```python
#!/usr/bin/python3

import os
import time
import datetime
from terminusdb_client import WOQLClient
from terminusdb_client import WOQLQuery as WOQL

# Connect to the server
server_url = "https://127.0.0.1:6363"
db = "dbpedia_1"
user = "admin"
account = "admin"
key = "root"
client = WOQLClient(server_url)
client.connect(user=user, account=account, key=key, db=db)

query = WOQL().limit(1,
      WOQL.woql_and(
        WOQL().path("Whitesnake", "(bandMember>,<bandMember)+", "v:Band","v:Path"),
        WOQL().triple("v:Band", "genre", "Country_music"),
    ))
result = client.query(query)
print(result['bindings'][0]['Band'])
print(result['bindings'][0]['Path'])
```

The answer we obtain for the band is `Angelfire_(band)` with the path:

```
"Whitesnake" - "bandMember" -> "Reb_Beach"
"Winger_(band)" - "bandMember" -> "Reb_Beach"
"Winger_(band)" - "bandMember" -> "Rod_Morgenstein"
"Dixie_Dregs" - "bandMember" -> "Rod_Morgenstein"
"Dixie_Dregs" - "bandMember" -> "Steve_Morse"
"Angelfire_(band)"- "bandMember" -> "Steve_Morse"
```
