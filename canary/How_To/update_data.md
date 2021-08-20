# How to update data

To update a triple with an unknown value which may or may not exist we
have to find the old value.

NOTE: If there are multiple values, this will delete all and replace
it with one.

## Python

In the python client:

```python
query = WOQL().when(
    WOQL().triple("my_document","my_property","v:Value"),
    WOQL().woql_and(
            WOQL().delete_triple("my_document","my_property","v:Value"),
            WOQL().add_triple("my_document","my_property",
                                  WOQL().literal("New Value","xsd:string"))
    )),
client.query(query)
```

## Javascript

To update a triple in the python client:

```javascript
when(
    triple("Person_1","gender","v:Value"),
    and(
        delete_triple("Person_1","gender","v:Value"),
        add_triple("Person_1","gender",literal("Male","string"))
    ))
```
