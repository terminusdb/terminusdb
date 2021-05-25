# How should I model my data? 

Data has always been important to businesses but how can I choose the right approach for a particular application? TerminusDB organizes data in a very simple structure to make it easy and natural to model the real world.

What we are doing together is an analysis of an Organization data domain, step by step we'll follow the process to find a Terminusdb schema model that can fit our data.

In this tutorial we'll model the database schema for a small organization. We simulate that our organization is divided into specialized groups. Our goal is to create a schema from mapping the process of monitoring and controlling projects. We would like to model all the events that track our project execution status.


### Analyze the Organization process and get out the main entities

An entity is an object with independent existence that can be differentiated from other entities. 
In TerminusDB we have two type of entities: [**Document elements**](#classes-or-node-element) can exist apart from other entities. [**Object elements**](#classes-or-node-element) can not exist without relationships connected to other entities.

Let's analyze our organization

* The **Organization** divides into functional **Team/Group** entities
* A **Person** belongs to a **Team/Group**
* A **Person** has a Role in the **Team/Group**
* The **Team/Group** schedules a **Project**
* The **Project** is divided into **Tasks**.
* Every **Task** is assigned to a **Person** to complete the **Job**.

![The main Entities](https://assets.terminusdb.com/docs/model_org01.png)


### Identify attributes. For every element

* Each entity type will have one or more properties.

![Attribute](https://assets.terminusdb.com/docs/model_org02.png)

### Identify relationships

We define a Relationship as the associations or interactions between Documents and Objects

* A **Person** is assigned to one **Team/Group** but can join in several **Projects**.
* The **Team/Group** schedules **Project** and divide it in **Task**. 
* The **Task** will be assign at a **Person** with a deadline that can change (we assume that a person works at one project at time)

![naming](https://assets.terminusdb.com/docs/model_org03.png)

### Apply naming conventions
A naming convention is an important part of a well-built data model.
Use understandable names that are applied consistently throughout the model.

It is recommended to follow the standard naming convention as described below.
![naming](https://assets.terminusdb.com/docs/model_org04.png)

### Apply data model patterns

TerminusDB uses a very rich data modelling language called OWL - the Web Ontology Language. Although it is a very rich language, the basic ideas are simple.

In our schema, we can define classes - which are definitions of types of complex data structure. Classes can be subclasses of other classes, which means that they inherit all the parent’s definitions (much like inheritance in object-oriented programming). We can also define properties in the schema - each property has a domain (the class that is the subject of the property) and a range - the type of data that the property points to. The range can either be a simple datatype literal (e.g. an integer or string) or it can be a class.

Let's see the building blocks to model the database schema.

#### Classes or Node Element

**Object** Contains a piece of the graph and can be accessed by a unique url. An Object can have multiple properties and can be contained inside other objects. In general you should use Object for data that is owned by a document and is not shared, or if you need to group Document end Object under the some parent. For example, in the Organization database, we should model the **Task** entity as an **Object** because is existence dependent on the **Project** entity.

**Document** is a special type of Object. Documents are always top-level objects, never embedded inside other objects, using the Link Property you can link documents to each other.
The Documents do not depend on another entity for their existence. If we refer back to our Organization database, examples of a Document entity include **Person**, **Organization**,
**Team/Group** and **Project**

**Enum** or Enumerated is a special data type that enables a property to be a set of predefined constants. The property must be equal to one of the values that have been predefined for it. To use this set of values you’ll need to create an Enum Property and link it to an Emun Element.
In the Organization database we can model as Enum **Roles** ,**Functions** and **Status**


#### Properties

To describe your Documents and to link them with each other, we need to add properties to our documents. We have various different type of properties identified by datatypes.
(e.g., Person = (Name, Birthdate, Address, Salary).

**DataProperty** : The data type of a property defines what value the field can hold, 
                    String/Numeric/Geo/Temporal 

**ObjectProperty** : Enum/Link Property this property are special properties used to create relationship between elements. You can make any class into a relationship by adding Object Properties that point at the related entities.


### Translate a TerminusDB graph data model to a relational data model

If you are used to programming with a relational database management system, here is a comparison that can help you. 

A relational database structures data into tables and rows, while TerminusDB structures data into Documents, Objects and Properties.

* Documents could be defined as tables that hold specific information (data)
* Properties could be the Columns on those tables
* Object could be a special Columns that group together extra fields
* Relationship properties replace foreign keys, 
* In terminusDB we don't need to denormalize to improve performance, we can avoid data duplication creating a new entity.

Now jump and see how to model our database schema. [**How do I use the model builder tool?**](#/Explanation/MODELHOW02)
















