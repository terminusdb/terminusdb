# **Frequently Asked Questions**

## (Ask a question enough times and it will appear here)

**[What is TerminusDB?](#head1)**

**[What is TerminusHub?](#head2)**

**[Wait… what? You can just pass complex and queryable databases around the web for free?](#head3)**

**[Why is TerminusDB useful?](#head4)**

**[When should I use TerminusDB?](#head5)**

**[What is the underlying TerminusDB architecture?](#head6)**

**[TerminusDB is open source and TerminusHub is free to use, what do you sell?](#head7)**

**[Can I get a commercial license for TerminusDB?](#head8)**

**[How is data stored in TerminusDB?](#head9)**

**[As a dev or a data scientist, how do I work with TerminusDB?](#head10)**

**[Is TerminusDB ACID?](#head11)**

**[How does TerminusDB scale?](#head12)**

**[What’s up with the CowDuck?](#head13)**

**[How is TerminusDB different from relational databases like Oracle?](#head14)**

**[What is the history of the project?](#head15)**

**[Why did you write your own database?](#head16)**

**[Why do you use WOQL - web object query language?](#head17)**

**[And why not SPARQL or Cypher?](#head18)**

**[Why JSON-LD?](#head19)**

**[Why Prolog and Rust?](#head20)**

**[What's the performance like? Do you have any benchmarks?](#head21)**

**[Can TerminusDB be used for big datasets?](#head22)**

**[Do I have to register anywhere to use TerminusDB?](#head23)**

**[Why not just use another well-known graph DB or data versioning tool?](#head24)**

**[Can I use TerminusDB as a document store?](#head25)**

- - -

### <a name="head1"></a>What is TerminusDB?

TerminusDB is an open-source general-purpose graph database that stores data like Git. It is built for data people and allows fierce data integration, versioning out of the box, and unparalleled query. No data platform is more productive.

[Download center](https://terminusdb.com/hub/download)

- - -

### <a name="head2"></a>What is TerminusHub?

TerminusDBs are linked through TerminusHub. The hub allows the user to manage access to databases and to collaboratively work on shared resources. You can make changes to a database, push them to hub and have a collaborator pull the synced version of the database.

You can share and collaborate on curated datasets in a distributed manner using git-like operations (push, pull, fetch, clone, fork).

Data Catalogs can be easily built on top of curated datasets giving verified information about contents and allowing discovery both inside the organization, and outside using TerminusHub

Collaboration is easy with the clone/fork operation, allowing data to be moved to your cloud or servers. Merge and branch operations let you mix and match data sources, significantly simplifying integration tasks

[More information and sign up](https://terminusdb.com/hub/)

- - -

### <a name="head3"></a>Wait… what? You can just pass complex and queryable databases around the web for free?

Yes, you can. Distributed revision control for structured data is here.

- - -

### <a name="head4"></a>Why is TerminusDB useful?

TerminusDB is built around three core design principles that collectively enable you to build faster, and with higher quality:

1. Fierce git-like collaborative data integration. Merge and branch operations let you mix and match data sources.
2. Revision Control enables collaboration, so you can work on different versions of the same asset at the same time in a controlled way without risk of data loss.
3. TerminusDB is a powerful graph database and analytic engine. It is built around relationships that power core data and software products. In today’s world, relationships among data ARE the data and they do not typically fit in rows and columns.

TerminusDB is the only database which enables and supplies all of these operations.

- - -

### <a name="head5"></a>When should I use TerminusDB?

TerminusDB and TerminusHub can be used across a range of OLTP and analytical apps.

TerminusDB Server provides TerminusDB with a RESTful API for interacting with knowledge graphs via the JSON-LD exchange format. This means you can easily compose applications within your own toolchain which utilize the powerful features of graph search and graph storage.

We think TerminusDB is perfect for OLTP use cases with transactions in the tens of thousands per day, such as headless-CMS, data curation, inventory management etc.

- - -

### <a name="head6"></a>What is the underlying TerminusDB architecture?

We use an advanced git-like model, storing append-only changes to graphs represented in succinct data structures using terminusdb-store.

You can read a description of the architecture in our [Succinct Data Structures and Delta Encoding for Modern Databases](https://github.com/terminusdb/terminusdb-server/blob/dev/docs/whitepaper/terminusdb.pdf) whitepaper.

- - -

### <a name="head7"></a>TerminusDB is open source and TerminusHub is free to use, what do you sell?

TerminusDB will be fully featured and open source (Apache 2.0) forever. TerminusHub has a freemium tier and paid subscriptions for private collaboration. We borrowed the GitHub commercial model.

More information on pricing and subscriptions [here](https://terminusdb.com/hub/).

If you are working with an open-source or public interest project, we want you to be free forever, so send us a [message](https://terminusdb.com/hub/).

- - -

### <a name="head8"></a>Can I get a commercial license for TerminusDB?

We have support for TerminusDB and enterprise licenses for TerminusHub. [Get in touch](https://terminusdb.com/hub/).

- - -

### <a name="head9"></a>How is data stored in TerminusDB?

TerminusDB Store is an RDF triple store implemented in Rust.

- - -

### <a name="head10"></a>As a dev or a data scientist, how do I work with TerminusDB?

To accelerate your productivity, TerminusDB has developed SDKs for Python and JavaScript (we are planning more so let us know what should be next). All supported SDKs are designed to be idiomatic for the given programming language (designing a Pythonic query language). This makes it much more natural for developers to work with data than string-based languages like SQL, and eliminates the need for cumbersome and fragile ORM abstraction layers.

You can also interact with TerminusDB and Hub graphically using our console, the GUI for TerminusDB. You can explore and manipulate your data, visually create queries, create and manage branches and much more.

- - -

### <a name="head11"></a>Is TerminusDB ACID?

Yes.

- - -

### <a name="head12"></a>How does TerminusDB scale?

TerminusDB primarily scales vertically. It uses succinct data structures' advantage of ever-increasing main memory available in modern machines, to get very large graphs in-memory.

Graph partitioning is used to scale beyond a single machine.

- - -

### <a name="head13"></a>What’s up with the CowDuck?

When our core storage engineer first presented the shiny new storage engine, he used an example of a cow that liked a duck, but the duck didn’t like the cow. From that, and a subsequent internal debate, CowDuck was born! There is still some evidence of the original cow and duck on the TerminusDB Store GitHub page.

- - -

### <a name="head14"></a>How is TerminusDB different from relational databases like Oracle?

Relational databases like Oracle, MySQL, SQL Server, Postgres and others are built on architectures originally designed over 40 years ago. The requirements for applications were very different back in the 1970s. Codd's innovations were indeed a benefit, as they brought schemas to a wild west. The problem is that they are unnecessarily restrictive and poor at data modeling. In data modeling, inheritance, multiple inheritance, and object identity are big advantages with complex data science and ML/AI projects or application.

RDBMSs are simply too primitive and not built with the developer or the data engineer/scientist in mind.

- - -

### <a name="head15"></a>What is the history of the project?

We are originally a University spin-out. There is a great blog on [the technical history](https://terminusdb.com/blog/2020/04/14/terminusdb-a-technical-history/).

- - -

### <a name="head16"></a>Why did you write your own database?

We started off by testing and building on other databases but constantly faced performance issues. By building all the way down to the ground, we could deliver a *native* revision control database.

Some solutions are just a version control layer on a traditional (usually relational) database; however, piecemeal additions without a fundamentally append-only, immutable store often cause more problems than they solve. We think that a native approach makes the most sense and will ultimately deliver the most value.

- - -

### <a name="head17"></a>Why do you use WOQL - web object query language?

The user interacts with TerminusDB through our JavaScript and Python SDKs. We hope to extend the number of SDKs over time.

WOQL is a powerful, highly expressive and composable graph query language. In its fluent form, WOQL is human-readable and easy to understand. It allows recursive queries and manipulation of paths through the graph.

WOQL is easy to compose from JavaScript or Python. JavaScript, because we live in a web age, and Python, because it is the choice for many data scientists. JSON-LD provides a nice intermediate language in which to write queries in either of these languages. Because of the choice of JSON-LD, we can naturally store our queries in the graph (and even query our queries).

- - -

### <a name="head18"></a>And why not SPARQL or Cypher?

We debated using SPARQL initially; however, we found SPARQL to have a number of shortcomings that we wanted to see addressed. We wanted tighter integration with OWL and schema-awareness, so if we were going to need to fundamentally alter the semantics, it didn't seem that important to start with SPARQL. We wanted better composability and SPARQL feels quite ad-hoc.

We also assessed Cypher and found that it lacked some of the features we needed. It is also primarily geared towards property graphs and not RDF graphs. It also does not enable the smooth SDK manipulation that WOQL facilitates by using JSON-LD as a query interchange format.

We think that the Python and Javascript SDKs for TerminusDB are the main way that users will interact with the database.

- - -

### <a name="head19"></a>Why JSON-LD?

Using JSON-LD as the definition, storage and interchange format for the query language has advantages. Since we can marshal JSON-LD into and out of the graph, it is easy to store queries in the graph. It is also simple to write query libraries for a range of languages by just building up a JSON-LD object and sending it off.

- - -

### <a name="head20"></a>Why Prolog and Rust?

Prolog is the language of AI. It is powerful and geared towards logic and reason. It is perfect for querying, constraint checking, query compilation, query analysis and user interaction. However, it is not good at nitty-gritty bit manipulation – that is where Rust comes in! It is low-level like C/C++, but memory-safe.

- - -

### <a name="head21"></a>What's the performance like? Do you have any benchmarks?

We started with a graph simulation in PostgreSQL and found it to be too slow for our purposes. We settled on or current data-structures after satisfying ourselves that it was much faster for deep multi-hop queries. We are developing benchmarks and doing lots of work on performance. Watch this space – or get involved and put the DB to the test.

- - -

### <a name="head22"></a>Can TerminusDB be used for big datasets?

Yes. TerminusDB is optimized to work with big datasets on the scale of tens of millions to billions of graph edges.

- - -

### <a name="head23"></a>Do I have to register anywhere to use TerminusDB?

No, you can use the open-source TerminusDB without registering or doing anything else.

If you want to use TerminusHub, we need an email address as a signature to put into the commit-graph, so you’ll have to register.

Sign up [here](https://terminusdb.com/hub/)!

- - -

### <a name="head24"></a>Why not just use another well-known graph DB or data versioning tool?

[Here](https://terminusdb.com/blog/2020/08/07/terminusdb-graph-basics/) is a blog discussing just that point!

- - -

### <a name="head25"></a>Can I use TerminusDB as a document store?

Yes, but to do so you need to first define the schema which defines a document. TerminusDB is a graph so it needs to know how you mean to store your documents as a graph. The advantage, however, is that you'll be able to search any property or value of your documents
