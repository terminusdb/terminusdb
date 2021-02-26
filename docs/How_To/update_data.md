# How to update data

To update a triple with an unknown value which may or may not exist we
have to find the old value.

NOTE: If there are multiple values, this will delete all and replace
it with one.

## Python

In the python client:

```python
query = WOQL().when(
    WOQL().triple("doc:my_document","scm:my_property","v:Value"),
    WOQL().woql_and(
            WOQL().delete_triple("doc:my_document","scm:my_property","v:Value"),
            WOQL().add_triple("doc:my_document","scm:my_property",
                                  WOQL().literal("New Value","xsd:string"))
    )),
client.query(query)
```

## Javascript

To update a triple in the python client:

```javascript
when(
    triple("doc:Person_1","scm:gender","v:Value"),
    and(
        delete_triple("doc:Person_1","scm:gender","v:Value"),
        add_triple("doc:Person_1","scm:gender",literal("Male","string"))
    ))
```
