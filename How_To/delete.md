## Delete a database with console

In the home page of the database that you want to delete, click on the delete button (see example below)

![delete graph step 1](https://assets.terminusdb.com/docs/delete-graph1.png)

Then, type out the database id to confirm (see example below)

![delete graph step 2](https://assets.terminusdb.com/docs/delete-graph2.png)

## Delete database via Python client

The `WOQLClient` provide a `delete_database` method to do so. For example:

```
>>> client = WOQLClient("https://127.0.0.1:6363/")
>>> client.delete_database("pybike")
```
