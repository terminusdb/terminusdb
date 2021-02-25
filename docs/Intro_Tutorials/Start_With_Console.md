
## Start with console

The TerminusDB Console is a ReactJS application that provides users with an interface for managing and querying TerminusDB. Assuming TerminusDB is installed successfully (Refer https://terminusdb.github.io/terminusdb/#/Intro_Tutorials/Getting_Started), we can carry on with how to get started with Console.

The landing page of console will be as shown below, with a list of database already created. A freshly installed console with
no database should directly lead you to creating a database work flow for you to get started.

![landing_page](https://assets.terminusdb.com/docs/console-landing-page.JPG)

### Create a database

Here we use the Bikes database as mentioned in https://terminusdb.com/blog/2020/09/01/my-first-terminusdb-3-0-graph-bike-share-data/.

![create_db](https://assets.terminusdb.com/docs/console-create-new-db.JPG)

This will lead you to the home page of the new db created where you have a tab with different actions that can be done.
1. DBHome - is the homepage where you can view the summary of db such as latest updates/ size/ number of classes or properties
2. Synchronize - helps you to perform Hub operations on the db once your logged in ( in this tutorial we do not cover hub operations)
3. Manage - Allows you to manage the database between different branches where you can perform actions such as merge or reset a particular branch etc .
4. Query - You can query the db
5. Documents - Provides you with a view of documents where you can view and create documents
6. Schema - Allows you to create schema for the database and actions related to it

### Add a schema

There are 2 ways in which you can create a schema

#### First Method - use the schema builder

Click on Schema Tab -> Schema Builder -> Click on Edit button and create 3 Document Classes - Station, Bikes, Journey as shown below
![schema_builder](https://assets.terminusdb.com/docs/console-schema-builder.JPG)

#### Second Method - Run a Query to create schema

Likewise you can also create a schema by a WOQL query

```javascript

WOQL.and(         
    WOQL.doctype("Station")             
        .label("Bike Station")             
        .description("A station where bicycles are deposited"),         
    WOQL.doctype("Bicycle")             
        .label("Bicycle"),         
    WOQL.doctype("Journey")             
        .label("Journey")             
        .property("start_station", "Station")
            .label("Start Station")             
        .property("end_station", "Station")
            .label("End Station")                                                
        .property("duration", "integer")
            .label("Journey Duration")         
        .property("start_time", "dateTime")
            .label("Time Started")
        .property("end_time", "dateTime")
            .label("Time Ended")
        .property("journey_bicycle", "Bicycle")
            .label("Bicycle Used")
)

```

![query_pane_schema](https://assets.terminusdb.com/docs/console-query-editor-add-schema.JPG)

So now we have the schema, on click of Schema tab, and click of Classes and properties will give you a table view of available
classes and properties of the db respectively.

Below is an image of table view for Properties

![query_pane_schema](https://assets.terminusdb.com/docs/schema-properties-table-view.JPG)

Below is an image for the equivalent graph view for how Journey is related to End Station and Start Station

![query_pane_schema](https://assets.terminusdb.com/docs/schema-properties-graph-view.JPG)
