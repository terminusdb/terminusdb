# Quick Start

This guide shows you how to create an application that uses the terminusDB-client-js driver to connect to TerminusDB and work with your data.
Terminusdb-client-js is an javascript asynchronous API which allows you to interact with TerminusDB using Promises.

>[How Install TerminusDB](https://terminusdb.com/docs/terminusdb/#/README?id=getting-started) 


We assume that you already have created and initialized you [node projects](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm) and installed [terminusdb library](https://terminusdb.com/docs/terminusdb/#/Install/JS_INSTALL)

# Connect to your TerminusDB server

For connecting to terminusdb server you need to pass the server url, the string includes information on the hostname or IP address and port of your server. The connection params includes all you need for Authentication and db setup, you can find the complete options [here](https://terminusdb.github.io/terminusdb-client-js/#/) 

```js
const TerminusDB = require('@terminusdb/terminusdb-client')

const client = new TerminusDB.WOQLClient('https://127.0.0.1:6363', {
    user: 'admin',
    key: 'mykey'
})
```

> Make sure to replace the "mykey" value in the connection object with the key you created for your admin user in terminusDB server.

# Create Database
To create a new db you need to pass the id of the database and the database details.

> Remember to add schema true or you not will be allow to add a schema for your database.

```js

async function createDB() {
    const dbId = 'db_user'
    const dbdetails = {
        id: dbId,
        label: 'User db',
        comment: 'create a test db',
        schema: true,
    }
    const createDBCall = await client.createDatabase(dbId, dbdetails)

    const queryTmpData = TerminusDB.WOQL.all()
    const res = await client.query(queryTmpData)
    return res
}

```

# Create Schema
We rea creating a simple document.

```js
function createSchema() {
    const addDocument = TerminusDB.WOQL.doctype('User')
        .label('User Name')
        .description('A User Document')
        .property('name', 'string')
        .label('Name')
        .cardinality(1)
        .property('surname', 'string')
        .label('Surname')
        .cardinality(1)
        //The cardinality in a property is a measure of the "number of values" for this property in a Document instance.
        //Cardinality 1 for date_of_birth means that for every Document Person you have insert one date_of_birth value (mandatory property)
        .property('email', 'string')
        .label('Email')
        .cardinality(1)
        .property('knows', 'User')
        .label('Knows')

    return client.query(addDocument)
}
```

# Understand Branch
Each Database can have one or more branches, when you create a new TerminusDB it comes with the default branch called **main**. TerminusDB saves each version of your database as a snapshot of the data exactly as it was at the moment you committed it. 

We are working with TerminusDB inserting data in the Databases to one branch and merging them back into the main Branch.
Let’s go through a simple example of branching and merging, you can use this workflow in the real world. Follow these steps:

1. create a new branch
2. insert data into the new branch **tmp_data**
3. merge the data from **tmp_data** to the **main** branch
4. remove the **tmp_data** branch

```js
    async function branchAndInsertData() {
        const newBranch = 'tmp_data'
        const WOQL = TerminusDB.WOQL

        //Created a new branch "tmp_data"
        await client.branch(newBranch)
        //Switched to a new branch "tmp_data"
        client.checkout(newBranch)

        //insert data into 'tmp_data' branch
        const insertQuery = WOQL.insert_data({
            id: 'User__Tom233455fhdght7',
            type: 'User',
            label: 'Tom Smith',
            surname: 'Smith',
            name: 'Tom',
            email: 'tom@fdgd.com',
        })

        await client.query(insertQuery)

        // query the 'tmp_data' branch and get back all the data

        const queryTmpData = WOQL.all()
        const allData = await client.query(queryTmpData)
        return allData
}

```

I this point the main branch is still empty. The **main** branch is the one where all changes eventually get merged back into.

```js
   async function rebase() {
        //Check out main branch
        client.checkout('main')

        //rebase tmp_data into main branch
        await client.rebase({
            rebase_from: 'admin/db_user/local/branch/tmp_data',
            message: 'Merging from tmp_data to main',
        })

        //query the main branch to see the data
        const queryTmpData = TerminusDB.WOQL.all()
        const allData = await client.query(queryTmpData)
        return allData
}
```
Now run all the functions. The main branch will points at the same place of the **tmp_data**.
You can delete the tmp_data branch with the deleteBranch method.

```js
    async function run() {
        try {
            await createDB()
            console.log('Created__database')
            client.db('db_user')
            await createSchema()
            console.log('Created__schema')
            await branchAndInsertData()
            console.log('Created__branch')
            await rebase()
            console.log('Rebase')
            client.deleteBranch('tmp_data').then(result => console.log(result))
            console.log('Deleted__tmp_data')
        } catch (err) {
            console.log('__ERROR__', err.message)
        }
    }
    run()
```
let's try yourself

