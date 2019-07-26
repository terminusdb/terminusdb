
# WOQL in prolog

When using RegulumDB directly from prolog the ast syntax enables us to
embed queries directly in our prolog predicates.

This syntax is used internally to the database to leverage metalogical
data such as the capabilities system.

An example of the capabilities system being queries for access rights
by a given user:

```
connect('http://localhost/capability', DB),
ask(DB, 
	[User,Database,Access], 
    (
	    t( _ , _ , _ ), 
		t( _ , _ , _ )
    ) 
).
```
