## TerminusDB Client Python

The terminusdb_client library consists of 4 major components: WOQLClient, WOQLQuery, WOQLViews and WOQLDataFrame (optional, included only if installed with the [dataframe] option). In this guide, we will show the basic usage of each component. For detail documentation of the Python client, please refer to the [full documentation](https://terminusdb.github.io/terminusdb-client-python/). For a full example of using all the code shown below, see the [bike example in Jupyter notebook](https://github.com/terminusdb/terminusdb-tutorials/blob/master/bike-tutorial/python/Create%20TerminusDB%20Graph%20with%20Python%20Client.ipynb).

### WOQLClient

A connection object that allows you to connect with the TerminusDB (local or remote) with basic auth. To create an client object, you will have to provide a URL to the server.

```python
from terminusdb_client import WOQLClient
client = WOQLClient("https://127.0.0.1:6363/, insecure=True")
```

For connection to a local server, we also have to set the insecure flag to True.

The most common way to use the client is to 1) connected to an existing database or 2) create a brand new database.

To connect to an existing database:

```python
client.connect(key="root", account="admin", user="admin", db="example_db")
```

To create a new database:
```python
client.connect(key="root", account="admin", user="admin")
client.create_database("example_db")
```

### WOQLQuery

The WOQLQuery object provides a query building tool for WOQL query, instead of JSON-LD -- the native format of WOQL. Most WOQLQuery object methods return a WOQLQuery object that is chainable, one of the exceptions is `execute` which will send the query built with the client provided.

```python
from terminusdb_client import WOQLQuery as wq
conditions = [wq().triple("v:Journey", "type", "scm:Journey"),
              wq().triple("v:Journey", "start_station", "v:Start"),
              wq().opt().triple("v:Start", "label", "v:Start_Label"),
              wq().triple("v:Journey", "end_station", "v:End"),
              wq().opt().triple("v:End", "label", "v:End_Label"),
              wq().triple("v:Journey", "journey_bicycle", "v:Bike")]
query = wq().select("v:Start", "v:Start_Label", "v:End",  "v:End_Label").woql_and(*conditions)
result = query.execute(client)
```

### WOQLViews

The WOQLViews object provides a tool for visualizing your query result in an interactive graph in Jupyter notebook. You can customize the look of the nodes and the edges.

```python
from terminusdb_client import WOQLView
view = WOQLView()
view.edges(["Start", "End"])
view.node("Start_Label", "End_Label").hidden(True)
view.node("End").color([53,105,53]).icon({"color": [255,0,0], "unicode": "🏁"}).text("v:End_Label").size(25).charge(-10)
view.node("Start").icon({"color": [255,0,0], "label": True}).text("v:Start_Label").size(25).collision_radius(10)
view.edge("Start", "End").weight(100)
view.show(result)
```
which will result in something like this

 <iframe src="https://assets.terminusdb.com/docs/bike-graph.html" width="100%" height="600px"></iframe>

### WOQLDataFrame

WOQLDataFrame is an optional component in `terminusdb-client`. If you would like to include the installation of the DataFrame module, you need to [install it with the [dataframe] option when you do pip install](https://terminusdb.github.io/terminusdb/#/Install/PYTHON_INSTALL?id=install-using-pip).

With WOQLDataFrame you can easily convert the result from the query, which is in JSON format, into a [pandas DataFrame](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.DataFrame.html).

```python
from terminusdb_client import WOQLDataFrame
WOQLDataFrame.query_to_df(result)
```

which will give you something like

![bike dataframe](https://assets.terminusdb.com/docs/bike-dataframe.png)




