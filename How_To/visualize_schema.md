
## Visualize a Schema

The Console provides various ways in which a schema can be visualized.

1. Schema Builder menu displays a diagram of the schema where you can edit/ save on the fly.
2. Classes menu displays a table format of all the available instance of classes.  
3. Properties menu displays a table/ graph format and graph view of all the available instance of properties.
4. You can also query for this in the Query Pane and visualize this in a graph view

### Schema Builder
In the below screen shot, when you go to Schema Tab -> Schema Builder, the example shows how the database has a model with Contract linked to an Agent (Person or Organization)
via relationship Owner

![schema_builder_screen_shots](https://assets.terminusdb.com/docs/schema_builder_screen_shots.JPG)


### Table View for Classes

The below screenshot shows a table view of Classes when you go to Schema Tab -> Classes

![schema-classes-view](https://assets.terminusdb.com/docs/schema-classes-view.JPG)

### Table/ Graph View for Properties

On Schema Tab -> Properties, we can toggle between table and graph View.

![schema-properties-table](https://assets.terminusdb.com/docs/schema-properties-table.JPG)

The graph view visually helps in identifying the relationships between various classes

![schema-properties-graph](https://assets.terminusdb.com/docs/schema-properties-graph.JPG)


### Query

The below query is a library function which simply displays all the available properties

```javascript

lib().properties() // shows properties

lib().classes() // shows classes

```

We are currently working on views to make visualizing graphs and tables more flexible, this is coming soon !!!
