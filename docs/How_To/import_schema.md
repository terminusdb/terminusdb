
## Import a Schema

You can import a schema from a file dump of turtle format using insert_triples or update_triples

Assuming TerminusDB Client is installed

```javascript
client.update_triples(
    "schema","main",
    contents,
    "Adding WOQL schema")
```

```javascript
client.insert_triples(
    "schema","main",
    contents,
    "Adding WOQL schema")
```

Please refer to tutorial https://github.com/terminusdb/terminusdb-tutorials/blob/master/woql/woql_schema.py for a working example
