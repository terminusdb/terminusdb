# How do I use the model builder tool? 

This is the second part of the Tutorial [**How Should I model my data?**](/Explanation/MODELHOW.md) The first part of the tutorial showed you how to analyze your data before starting to model it. 

This second article shows how you can create a "schema model" using the Model Builder Tools in the Console interface. The model builder is a tool that lets you visualize and edit your database schema.

To get started:
* [Install TerminusDB](#/How_To/Install) 
* Run TerminusDB Console and Create a new Database called myOrganization

We are going, step by step to model the myOrganization database schema.

![naming](https://assets.terminusdb.com/docs/model_org03.png)

Form the Organization main page.

1. Select the menu Schema->Schema Builder to arrive at the model builder interface.

![naming](https://assets.terminusdb.com/docs/org-model01.png)


We did our analysis, now we have to choose our elements to model our data.

![naming](https://assets.terminusdb.com/docs/org-model02.png)

Let's start to model our Documents **Organization**, **Person**,**Team** and **Project**.

> These elements are **Documents** because they do not depend on another entity for their existence. Think at a **Document**  like a table in a relational database.


### Add the Document Node
![entities](https://assets.terminusdb.com/docs/org-model03.png)

1. In the whiteboard select **myOrganization Schema** node and click the + icon, the list of all available node types will show up.
2. Choose Add Documents from the menu. A new node will be added under the node Documents
3. We start to add Organization Document. Fill the form in the right sidebar with the data for the image below. (Unique Id:Organization..)
4. Follow the same steps above for adding the other Documents **Person**, **Project** and **Team/Group** 
5. Click on the Save icon button in the tools bar to save your work.

![entities](https://assets.terminusdb.com/docs/org-model04.png)

### Add the Object Node 

![entities](https://assets.terminusdb.com/docs/org-model05.png)

1. In the whiteboard select **myOrganization Schema** node and click the + icon, the list of all available node types will show up.
2. Choose Add Object from the menu. A new node will be added under the node Objects
3. Fill the form in the right sidebar with the data for the image below. (Unique Id:Task..)
4. Follow the same steps above for adding the other Object **Job**
5. Click on the Save icon button in the tools bar to save your work.

> The **Job** Object add an extra level of granularity in our schema. I use **Job** to link one to one **Task** and **Person** to track the work progress for every single Person.

> Up to the user choose the level of detail (Granularity) at which data are stored in TerminusdDB.

### Create a parent element to group the nodes

If we look at the first image we can see that we have 4 properties are common at all the elements **start** and **end** time, name and description. 

This is why we are adding an **abstract Object** called **Entity** with the follow properties **lifespan_start**, **lifespan_stop**, **title** and **summary**.

1. Create the **Entity** node, and fill the form in the right sidebar
2. Check the Abstract checkbox.
3. Click on the Save icon button in the tools bar to save your work.

>The **Entity** node is an **Object**, an **Object** can have
documents and objects as children.

>An **abstract** node is used to group related properties, you can not insert data in an abstract node.

![entities](https://assets.terminusdb.com/docs/org-model06.png)

### Mode the elements under the Entity Parent

1. Select the node **Organization** in the whiteboard, in the right panel Select **Relationships**
2. Under **Add/Remove Parents** panel from the menu Select Object option
3. In **Add Object as Parent** list Select **Entity**
4. The node Organization will be moved under the new parent Entity.
5. Follow the same steps above for moving the other Node **Person**,**Team/Group**,**Project**, **Task**, **Job** under **Entity**

![entities](https://assets.terminusdb.com/docs/org-model07.png)


### Add Property

![entities](https://assets.terminusdb.com/docs/org-model08.png)

**Datatype property**

1. Select the node **Entity**
2. Select the tab Properties in the right sidebar, from the menu Add Property Choose Temporal Property
3. The Temporal Property Panel will show up, fill the fields (Unique ID: lifespan_start,.....)
4. Follow the same steps above for adding **lifespan_end** 
5. Click on the Save icon button in the tools bar to save your work.

![entities](https://assets.terminusdb.com/docs/org-model09.png)

**Link property**

1. Select the Node, **Project**
2. Select the tab Properties in the right sidebar, from the menu **Add Property** Choose **Link Property**
3. The Link Property Panel will show up, fill the fields (Unique ID: project_task,.....)
4. In the **Links To Type** menu select **Task**
5. Click on the Save icon button in the tools bar to save your work.


![entities](https://assets.terminusdb.com/docs/org-model10.png)

> Node have properties, which are details we want to track about them. You can think of properties as the columns in a table. 

>**Link Property** create a relatioship between elements 

### Add an Enum Node

1. Select the Schema myOrganization, Select the + icon, from the menu Select Add Enum
2. The Enum node will be added in the whiteboard
3. Fill the fields in the right panel (Unique Id:Roles..)
4. Select values, Fill the fields and click the button Add a value for adding the list of possible values (developer,project_manager...)
5. Follow the same steps above for adding **Status** and **Functions** 
6. Click on the Save icon button in the tools bar to save your work.
![entities](https://assets.terminusdb.com/docs/org-model11.png)

**Add an Enum Property**

1. Select the Node **Project**, Select the tab Properties in the right sidebar
2. from the menu **Add Property** Choose **Enum Property** 
3. Fill the fields in the right panel (Unique Id:project_status..)
4. In the **Enum Type** menu select **Status**
6. Click on the Save icon button in the tools bar to save your work.

You can clone the full myOrganization schema from [TerminusHub](https://terminusdb.com/hub/)

![model](https://assets.terminusdb.com/docs/org-model12.png)

> Remember that if I remove an instance of a Documen all the objects related to this instance will be removed. Example: I have a document called **Project 01** which has 2 Tasks and these Tasks are divided into 10 **Jobs** assigned to 10 **Person**. If I remove **Project 01** all the Tasks and Jobs will be removed. 

In a real application this model will be more complex, but I think this is a good starting point.
