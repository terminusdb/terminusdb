# Datalog – what is that? And why is it good for query?

Datalog is an abstract declarative language family which is a *purely
declarative* subset of prolog. It does not refer to a specific syntax,
but rather a more or less defined semantics. Because of its purely
declarative and logical nature it makes an excellent query
language, particularly (but not limited to), its utility for graph
query.

Datalog, like its super-set prolog, is based on the use of
predicates. These predicates fill a role similar to that of relations
from the more familiar relational languages like SQL. Queries can use
these predicates together with *logical variables* to represent
unknowns to which we would like concrete referents which satisfy some
logical formula.  This happens by joining together predicates with
logical connectives such as `and` and `or` and *unifying* logical
variables such that repeated occurrences of the same variable *require*
the query has identical solutions at those points.

## Advantages of Datalog

As against prolog, datalog has some advantages. Variables are only
allowed to take on *atomic* values. In prolog, complex objects with
deep structural matching can occur. If atomic values are limited to
finite sets, such as all nodes which might occur in a graph, we can
guarantee termination of queries, even in the face of recursion. This
means we can also simplify the process of optimisation of queries and
(with some additional restrictions on the use of negation) we can
guarantee we will not "flounder" (flip-flop between alternative
solutions).

The restriction to finite atomic values is relaxed in WOQL to give us
lists, which are useful for aggregation queries (such as group-by) and
dis-aggregation (such as member), in addition to useful objects such as
natural numbers. We however retain the pure declarative quality of
datalog.

As against relational languages, datalog provides a much more flexible
logical framework. It is much easier to extend in a consistent fashion
with recursive and path centric operations. In addition it allows
complex joins to be provided in a more elegant fashion with a less
verbose syntax.

Datalog can be seen as a sort of stepping stone from relational
languages like SQL towards a more fully featured programming language
while still retaining the nice declarative properties which has made
relational approaches so robust, pervasive and resilient as query
languages.

## Unification and Query

In the context of datalog, unification is relatively simple, it is the
process of finding values of logical variables which are consistent
for a given logical sentence or *query*.

A logical variable for a query can only take on *one value* in a given
solution. If the variable is used in two places then these two values
must be the same. We can get the concrete value of solutions for a
logical value either from an equation, or from the definition of a
predicate.

When we search using datalog in WOQL, we implicitly ask for *all*
solutions (this can be restricted by using additioanl words such as
`limit(n,Q)`). This gives us back something that looks quite similar
to a table, but it is actually a list of solutions with bindings for
all logical variables that took on a value during the course of
searching for the solutions to the query.

## An Example

Perhaps the most important predicate in WOQL is `triple` which gives
results about edges in the current graph.

Our logical variables are represented as strings with the prefix
`"v:"`. Our edges are represented by having a position for the
*subject*, *predicate* and *object* of the edge in the graph. The
*predicate* being the labelled name of the edge, and the *subject* and
*object* nodes the source, and target respectively.

```javascript
triple("v:Subject", "v:Predicate", "v:Object")
```

With this query we simply get back the solutions for every possible
assignment of subjects, predicates and objects that our graph
currently has, that is, all edges in the graph. The concrete referents
for the subject, predicate and object are datapoints represented by a
URI (a universal resource indicator).

```javascript
triple("v:Subject", "v:Predicate", "v:Intermediate")
triple("v:Intermediate", "v:Predicate", "v:Object")
```

In this second query we have *joined* two predicates together by
requiring that the target of the first edge is the source of the
second. This gives us back all two hop paths possible in the
graph.

```javascript
triple("doc:My_Object", "v:Predicate", "v:Intermediate")
triple("v:Intermediate", "v:Predicate", "v:Object")
```

And here we refer to a specific starting node, and search for every
two hop path starting from *this* object.

## Conclusion

Datalog is a flexible but powerful declarative query language which
excels at dealing with the kinds of complex and many-hop relationships
that occur in graphs. While many graph query languages exist, those
not based on datalog do not exibit the clarity, simplicity and logical
underpinnings that were present in datalog. Datalog is Occam's razor
for graphs and its clarity and simplicity make it the *right*
foundation for graph query.
