
# How do I import the Prov Ontology / Other Ontology

In general, it is not a good idea to assume that you want to import a given ontology. Many ontologies use a random assortment of namespaces and predicates that have no real definition and must be avoided if you want to make a well defined data model. [see paper](https://terminusdb.com/t/papers/linked-data-schemata-fixing-unsound-foundations.pdf)

However, there are some high-quality well crafted ontologies out there that may be useful. Prov is well crafted at least - it's utility is an open question. 

## Importing Prov

You can import any ontology by simply using the insert_triples() API endpoint for the schema graph

```js
client.insert_triples("schema", "main", "full text of ontology in turtle format")
```

And this will add the ontology to the schema, making all of the classes and properties available - instances of the classes can then be added to the DB easily. 

