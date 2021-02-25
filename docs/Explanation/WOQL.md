# Explaining  WOQL


## Fluent Style

The TerminusDB query libraries make extensive use of the fluent style to simplify the expression of complex compound queries. Many WOQL query words accept a sub-query as an argument and, rather than using a functional (Lisp-like) style of capturing containment, a style where sub-queries are appended to the initial function as a new function is preferred.

rather than using a functional style:

```javascript
    select(a, b, triple(c, d, e))
```

the fluent style would be:

```javascript
    select(a, b).triple(c, d, e)
```

Both styles are legal WOQL and semantically equivalent. However, the second 'fluent' style is preferred because it is easier to read and easier to write primarily becaue it greatly reduces the amount of vizual parameter matching that the reader and writer have to perform in order to verify that their query is correct.

Fluent queries are parsed left to right - functions to the right of a given function are considered as sub-queries of the first, with one important exception - conjunction.

the functional style of expresing conjunction using the WOQL and() function is straightforward and is often most useful for clarity:

```javascript
    and(triple(a, b, c), triple(d, e, f))
```

the fluent style allows us to use any of the following forumlations with the same semantics:

```javascript
    and(triple(a, b, c)).triple(d, e, f)
    triple(a, b, c).and().triple(d, e, f)
    triple(a, b, c).triple(d, e, f)
```

The third concise form is unambiguous in situations where the WOQL functions that are chained together do not take sub-clauses - and because conjunction is so frequently used, this short-hand form, where the and() is implicit, is convenient in many situations. However it should be used with care - the conjunction is always applied to the function immediately to the left of the '.' in the chain and not to any functions further up the chain.  If used improperly, with clauses that do take sub-clauses, it will produce improperly specified queries, in particular with negation (not) and optional functions (opt).

So, for example, the following query:

```javascript
    triple(a, b, c).opt().triple(d, e, f).triple(g, h, i)
```

is equivalent to the following query in the functional style:

```javascript
    and(
        triple(a, b, c),
        opt(
            and(
                triple(d, e, f),
                triple(g, h, i)
            )
        )
    )
```

It is easy to misinterpret it when you mean to express:
```javascript
    and(
        triple(a, b, c),
        opt().triple(d, e, f),
        triple(g, h, i)
    )
```

As a general rule, if in doubt, use the functional style explicitly with and() as this makes it clear and explicit which functions are sub-clauses of other functions.

## WOQL.js and JSON-LD

WOQL uses JSON-LD and a formally specified ontology to define the language and to transmit queries over the wire.  WOQL.js is designed primarily to be as easy as possible for programmers to write because JSON-LD is itself tedious for humans to read and write. All WOQL.js queries are translated into the equivalent JSON-LD format for transmission over the wire.  The WOQL.js json() function can be used to translate any WOQL query from its JSON-LD format to and from it's WOQL.js equivalent (a WOQLQuery() object). If passed a JSON-LD argument, it will generate the equivalent WOQLQuery() object, if passed no argument, it will return the JSON-LD equivalent of the WOQLQuery(), in general the following semantic identity should always hold:

```javascript
let wjs = new WOQLQuery().json(json_ld)
json_ld == wjs.json()
```

### Embedding JSON-LD directly in WOQL.js

It is possible to use JSON-LD interchangably within WOQL.js - wherever a WOQL function or argument can be accepted directly in WOQL.js, the JSON-LD equivalent can also be supplied. So, for example, the following two WOQL statements are identical:

```javascript
triple(a, b, 1) == triple(a, b, {"@type": "xsd:integer", "@value": 1})
```

There should never be a situation in which it is necessary to use JSON-LD directly - WOQL.js is sufficiently rich to express all queries that are expressible in the underlying JSON-LD, however it can be convenient to embed JSON-LD in queries in some cases.

## WOQL Variables

With the exception of resource identifiers which are used to specify the graphs against which operations such as queries are carried out (functions: using, with, into, from), WOQL allows variables to be substituted for any of the arguments to all WOQL functions. WOQL variables follow the logic of unification - borrowed from the Prolog engine which implements WOQL within TerminusDB.  That is to say that each valid value for a variable, as constrained by the totality of the query, will produce a new row in the results, and when there are multiple variables, the rows that are returned will be the cartesian product of all the possible combinations of variables values in the query.

In WOQL.js, there are 3 distinct ways of expressing variables within queries. All are semantically equivalent, although the first is generally preferred as it is easier to type and it is easier to distinguish variables from constants at a glance due to the lack of quote marks around the variables

1

```javascript
    let [a, b, c] = vars('a', 'b', 'c')
    triple(a, b, c)
```

2

```javascript
    triple('v:a', 'v:b', 'v:c')
```

3

```javascript
    triple({'@type': 'woql:Variable', 'woql:variable_name': {"@type": 'xsd:string', '@value': 'a'}} ....)
```

WOQL uses the formal logical approach to variables known as unification - this allows most WOQL functions to serve as both pattern matchers and pattern generators, depending on whether a variable or constant is provided as an argument. If a variable is provided, WOQL will generate all possible valid solutions which fill the variable value. If a constant is provided, WOQL will match only those solutions with exactly that value. With the exception of resource identifiers, WOQL functions accept either variables or constants in virtually all of their arguments.

## Prefixes in WOQL.js

Internally, TerminusDB uses strict RDF rules to represent all data. This means that all identifiers and properties are represented by IRIs (which are a superset of URLs). However, IRIs are difficult to remember and tedious to type. RDF in general gets around this problem by allowing prefixed forms as shorthand - so for example, we can use "rdf:type" rather than "http://obscure.w3c.url/with/embedded/dates#type". TerminusDB defines a set of standard prefixes which are availabe to use and also allows users to extend this by adding their own prefix designations to the system. The set of standard prefixes includes the basic language elements (rdf, rdfs, owl), datatype elements (xsd, xdd) and internal namespaces (ref, repo, system, vio). It also pre-defines two prefixes for user-use - the 'doc' prefix for instance data IRIs and the 'scm' prefix for schema IRIs. So we can write "doc:X" or "scm:X" and this will always resolve to a valid IRI in all databases.


WOQL goes a step beyond supporting prefixes and automatically applies prefixes wherever possible allowing users to specify prefixes only when necessary.

The default prefixes are applied in the following way
    - "doc" applies to woql:subject (first argument to triple) where instance data IRIs are normally what is required
    - "scm" applies to woql:predicate and other arguments (sub, type) where schema elements are normally required
    - when standard predicates are used with no prefix (label, type, comment, subClassOf, domain, range) the standard correct prefixes are applied
    - otherwise if no prefix is applied a string is assumed

## WOQL Functions


The JSON-LD form of WOQL supports a well-defined set of functions (woql:Triple, woql:Regexp...) - in WOQL.js these functions are known as primitives. WOQL.js supports all of these primite functions and adds several extensions on top - functions that compose multiple primitives, functions that compose partial primitives and can be chained together, and simple helper functions to make it easier to format the arguments correctly. The table below shows the full range of functions supported by WOQL.js and groups them together into categories to make it easier to find the required function for specific problems.
