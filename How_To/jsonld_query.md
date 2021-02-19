# Constructing a JSON-LD Query

Every WOQL query is converted into a JSON-LD document format for transmission over the network.

You can access the JSON-LD from the javascript query object:

```javascript
let jsonld = query.json()
```

```python
jsonld = WOQLQuery().dict()
```


