
# Query


TerminusDB allows you to query graphs in either Javascript or
Python. This tutorial is about using Javascript, which is the method
which is available in the console.


## Open a Database

First you'll need a database which you can query. You can get started
by opening TerminusDB and going to the clone page.

From there you should select "TerminusDB Bikes Tutorial", and clone it
to your local machine.

Once you've cloned it, you should be opened up into the WordNet
database automatically. Click on the Query button.

## First steps

Graph databases are made up out of edges in a graph. You can query
these edges by using the word `triple` (a triple is a source node, a
named edge and a target node).  Now, the WordNet graph is fairly
large, so we'll also need to limit our queries to make sure we get
something manageable to look at. We can do that with `limit`.

Try out the following query in the query box:

```javascript
let [subject, predicate, object] = vars('subject','predicate','object')
limit(1).triple(subject,predicate,object)
```

This query just gets back any random edge without any constraints. But
graphs are about connections, so we should make a query that goes more
than one hop in order to find something interesting. In this case we
just get back the triple:


| `X`                  | `P`        | `Y`           |
| ---                  | ---        | ---           |
| `doc:Bicycle_W00017` | `rdf:type` | `scm:Bicycle` |


If we want to use the graph aspects, we have to create chains:


```javascript
let [subject, predicate1, intermediate, predicate2, object] =
    vars('subject','predicate1','intermediate','predicate2','object')
limit(1).triple(subject,predicate1,intermediate)
        .triple(intermediate,predicate2,object)
```

This is a two-hop query, so now we are actually exploring the graph a
bit.

First, let's do a little exploration to see what is in the
database. As it turns out, predicates are not as common as source or
target nodes (subjects or objects).

We can see all of the predicates using the following query:

```javascript
let [subject, predicate, object] = vars('subject','predicate','object')
distinct(predicate)
    .select(predicate)
    .triple(subject,predicate,object)
```

This query selects only the `predicate`s (it masks out the other
variables) and ensures that we only get distinct `predicate`s. You'll
notice that there are two in particular: `scm:start_station` and
`scm:end_station`. We'll come back to those in a moment.

## Viewing the schema

If you click on the schema tab, you'll be able to peruse the object
types and predicates which are available to you in any database which
uses a schema.

[image here]

There are several tabs, Classes, Properties, Graphs, Triples and
Prefixes. The Triples tab will give you access to the raw OWL schema
information, which is also editable.

[image here]

But for our purposes we just need to know what properties are
available. Here we can see that `scm:start_station` and
`scm:end_station` have a `scm:Journey` as the domain, and
`scm:Station` as a range. This means that our journey object specifies
where we started and where we end up.

## Combining Queries

The basic building block of a query in TerminusDB is the triple. In
order to combine them we tend to build up graph fragments in which we
use the same variable at each node that we want to be the same. We
then combine these triples with `and`.  We call this repeated variable
use for matching *unification*.

Let's look at our `scm:Journey`'s using `and`:

```javascript
let [journey,start_station,end_station] = vars('journey','start_station','end_station')
limit(10)
      .and(
        triple(journey,'scm:start_station',start_station)
        triple(journey,'scm:end_station',end_station)
      )
```

This query gives us back the end points of our journey. However, it
isn't terribly informative as we are only given identifiers (IRIs)
which are opaque. We can however, expand the query to ask for the
names of these stations:

```javascript
let [journey, journey_name ] = vars('journey', 'journey_name')
let [start_station, start_name] = vars('start_station', 'start_name')
let [end_station, end_name] = vars('end_station', 'end_name')
limit(10)
      .and(
        triple(journey,'scm:start_station',start_station),
        triple(journey, 'label', journey_name),
        triple(start_station, 'label', start_name),
        triple(journey,'scm:end_station',end_station),
        triple(end_station, 'label', end_name)
      )
```

Perfect! Now we see the named location of these bike
stations. However, we also have a fair bit of extra information so we
can filter that out again with a `select` statement.


```javascript
let [journey, journey_name ] = vars('journey', 'journey_name')
let [start_station, start_name] = vars('start_station', 'start_name')
let [end_station, end_name] = vars('end_station', 'end_name')
limit(10).select(journey_name,start_name,end_name)
      .and(
        triple(journey,'scm:start_station',start_station),
        triple(journey, 'label', journey_name),
        triple(start_station, 'label', start_name),
        triple(journey,'scm:end_station',end_station),
        triple(end_station, 'label', end_name)
      )
```
Now we have something a little bit more human readable. Perhaps you
can try looking at the properties and adding additional information.

We can also ask questions where we want either/or type results. For
this, we can use the WOQL word `or`. We could ask, for instance, for
every journey which started from a station *or* ended at that station
as follows:

```javascript
let [journey, journey_name ] = vars('journey', 'journey_name')
let [some_station, some_station_name] = vars('some_station', 'some_station_name')
limit(10).select(journey_name,some_station_name)
      .and(
          or(
            triple(journey,'scm:start_station',some_station),
            triple(journey,'scm:end_station',some_station)
          ),
          triple(journey, 'label', journey_name),
          triple(some_station, 'label', some_station_name)
      )
```

This gives us back journeys which have to do with a station either
entering or leaving.

## Manipulating text

When you're querying you'll often need to manipulate the text in
entries. WOQL has a number of words for helping you parse and
reassemble text.

One of the most flexible ways of finding and parsing text is to use
the `re` WOQL word. Supposing we want to find every journey starting
from a station on 10th street. We could run the following:

```javascript
let [journey, journey_name ] = vars('journey', 'journey_name')
let [start_station, start_name] = vars('start_station', 'start_name')
let [match] = vars('match')
limit(1).and(
    triple(journey, "scm:start_station", start_station),
    triple(start_station, 'label', start_name),
    re(".*10th.*", start_name, [match])
  )
```

This is great, but perhaps we want to say what the other corner street
was. We can do this by adding matching groups with `(` and `)`:

```javascript
let [journey, journey_name ] = vars('journey', 'journey_name')
let [start_station, start_name] = vars('start_station', 'start_name')
let [match,other] = vars('match', 'other')
and(
    triple(journey, "scm:start_station", start_station),
    triple(start_station, 'label', start_name),
    or(
      re("^(.*) . 10th.*$", start_name, [match, other]),
      re("^10th . (.*)$", start_name, [match, other])
    )
  )
```

This gives us back every journey which started at any station on 10th
street.

Once we have these strings, we might want to use them to put together
new strings. Let's add a message to our report about 10th street.

```javascript
let [journey, journey_name ] = vars('journey', 'journey_name')
let [start_station, start_name] = vars('start_station', 'start_name')
let [match,other] = vars('match', 'other')
let [message] = vars('message')
select(message).distinct(message).and(
    triple(journey, "scm:start_station", start_station),
    triple(start_station, 'label', start_name),
    or(
      re("^(.*) . 10th.*$", start_name, [match, other]),
      re("^10th . (.*)$", start_name, [match, other])
    ),
    concat(["The other street is: ",other], message)
  )
```

The use of distinct here gives us a nice summary view, along with your
message.

## Mathematics

You may also want to perform some mathematical operations on data
going into or out of the database. WOQL has basic mathematical
expressions: `plus`, `multiply`, `divide`, `exp`, `div` (for integer
division) and others. To use them you need to put the mathematical
expression in a form called `WOQL.eval` as follows:

```javascript
let [ x ] = vars('x')
WOQL.eval(plus(1,2), x)
```

This binds `x` to the result of the addition of `1` and `2`. You can
use variables in place of `1` and `2` and re-use `x` in later
queries. For instance we could write:

```javascript
let [product,result] = vars('product', 'result')
and(
    WOQL.eval(times(3,2), product),
    WOQL.eval(div(product,2), result)
)
```

Here of course we get back the number `3` as we'd expect. The complete
definition of mathematical operators is in the python-client
reference documentation.

## Manipulate Data

WOQL has various ways to manipulate data, which you can do from the console query page. Since in TerminusDB all changes are append-only, this will create a new commit describing all the additions and removals your query did.

When manipulating data from the query screen, you need to specify a reason for your update in the text bar at the top of the query pane. This message will be used as the commit message.

### Inserting
```javascript
add_triple("doc:a","scm:b","doc:c")
```

This will insert the triple `("doc:a","scm:b","doc:c")`.
### Deleting
```javascript
delete_triple("doc:a","scm:b","doc:c")
```

This will delete the triple `("doc:a","scm:b","doc:c")`.

## Conclusion

Now you've a few tricks under your belt, you should be able to explore
some of the datasets available on TerminusHub or your own data a
bit. Happy WOQLing!


