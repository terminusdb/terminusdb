
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

Click on Schema Tab -> Schema Builder -> Click on Edit button and create 3 Document Classes - Station, Bikes, Journey as shown below. More details covered in https://terminusdb.com/blog/2020/11/19/model-builder-and-data-modeling/
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

![schema_properties_table](https://assets.terminusdb.com/docs/schema-properties-table-view.JPG)

Below is an image for the equivalent graph view for how Journey is related to End Station and Start Station

![schema_properties_graph](https://assets.terminusdb.com/docs/schema-properties-graph-view.JPG)

### Add Documents

So now that we have a schema, lets go ahead a create some documents

There are 3 ways in which you can create documents

#### First Method - Create via Document Interface

Here we are going to create a journey before which, create two stations - doc:Station_001 and doc:Station_002

![create-journey](https://assets.terminusdb.com/docs/console-create-journey.JPG)

On click of Save

![view-journey](https://assets.terminusdb.com/docs/console-view-journey.JPG)

#### Second Method - Create via Query

Another way to add documents are to run the below query in Query tab

Refer - https://terminusdb.com/blog/2020/09/01/my-first-terminusdb-3-0-graph-bike-share-data/

```javascript

const csv = WOQL.get(
    WOQL.as("Start station","v:Start_Station")
        .as("End station", "v:End_Station")
        .as("Start date", "v:Start_Time")
        .as("End date", "v:End_Time")
        .as("Duration", "v:Duration")
        .as("Start station number", "v:Start_ID")
        .as("End station number", "v:End_ID")
        .as("Bike number", "v:Bike")
        .as("Member type", "v:Member_Type")
).remote("https://terminusdb.com/t/data/bike_tutorial.csv") // reads columns from a csv

//Clean data for insert
const wrangles = [
    WOQL.typecast("v:Duration", "xsd:integer", "v:Duration_Cast"),
    WOQL.typecast("v:Bike", "xsd:string", "v:Bike_Label"),
    WOQL.typecast("v:Start_Time", "xsd:dateTime", "v:ST_Cast"),
    WOQL.typecast("v:End_Time", "xsd:dateTime", "v:ET_Cast"),
    WOQL.typecast("v:Start_Station", "xsd:string", "v:SS_Label"),
    WOQL.typecast("v:End_Station", "xsd:string", "v:ES_Label"),
    WOQL.idgen("doc:Journey",["v:Start_ID","v:Start_Time","v:Bike"],"v:Journey_ID"),       
    WOQL.idgen("doc:Station",["v:Start_ID"],"v:Start_Station_URL"),
    WOQL.idgen("doc:Station",["v:End_ID"],"v:End_Station_URL"),
    WOQL.idgen("doc:Bicycle",["v:Bike_Label"],"v:Bike_URL"),    WOQL.concat("v:Start_ID - v:End_ID @ v:Start_Time","v:J_Label"),
    WOQL.concat("Bike v:Bike from v:Start_Station to v:End_Station at v:Start_Time until v:End_Time","v:Journey_Description")
];

//combine inputs
const inputs = WOQL.and(csv, ...wrangles)
//generate data to be inserted
const inserts = WOQL.and(
    WOQL.insert("v:Journey_ID", "Journey")
        .label("v:J_Label")
        .description("v:Journey_Description")
        .property("start_time", "v:ST_Cast")
        .property("end_time", "v:ET_Cast")
        .property("duration", "v:Duration_Cast")
        .property("start_station", "v:Start_Station_URL")
        .property("end_station", "v:End_Station_URL")
        .property("journey_bicycle", "v:Bike_URL"),
    WOQL.insert("v:Start_Station_URL", "Station")
        .label("v:SS_Label"),
    WOQL.insert("v:End_Station_URL", "Station")
        .label("v:ES_Label"),
    WOQL.insert("v:Bike_URL", "Bicycle")
        .label("v:Bike_Label")
);
//Combine inputs and inserts with when clause
WOQL.and(inputs, inserts);

```

#### Third Method - Add CSV or JSON files directly

Click on Add Files, you will be getting a file explorer from which you can opt to choose both CSV or JSON documents.

![console-document-view](https://assets.terminusdb.com/docs/console-document-view.JPG)

On click of Upload you will the csv file will be saved as a new CSV document type. Try this out.
More details covered in https://terminusdb.com/blog/2020/11/23/creating-a-database-with-csvs-using-terminusdb-console/

![console-document-add-csv-json](https://assets.terminusdb.com/docs/console-document-add-csv-json.JPG)

### Query the Data

Here is a small query which display all the stations and their names. Go to Query Pane and paste the below query and click on Run.

```javascript
triple("v:A", "type", "scm:Station").
triple("v:A", "label", "v:Names")
```

### Manage database

This allows you to create a branch and perform various branch operations. Actions that can be performed are
1. Merge a branch into current branch
2. Reset a branch to a commit of interest
3. Squash many commits to a single commit


![console-manage](https://assets.terminusdb.com/docs/console-manage.JPG)

![console-manage-detail](https://assets.terminusdb.com/docs/console-manage-detail.JPG)
