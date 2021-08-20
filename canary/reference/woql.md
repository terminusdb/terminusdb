# WOQL schema

This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products.

**Authored by:** Gavin Mendel-Gleason

---

### AddData

<p class="tdb-f">Add a (terminal) edge with a data value.</p>

**Class:** `AddData`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `DataValue` | A datatype or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### AddLink

<p class="tdb-f">Add an edge which links between nodes in the graph.</p>

**Class:** `AddLink`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `NodeValue` | A URI or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### AddTriple

<p class="tdb-f">Specify an edge to add to the graph.</p>

**Class:** `AddTriple`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `Value` | A URI, datatype or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### AddedData

<p class="tdb-f">Specify an edge pattern with data value which was added in *this* commit*.</p>

**Class:** `AddedData`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `DataValue` | A datatype or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### AddedLink

<p class="tdb-f">Specify an edge pattern which links between nodes at *this* commit.</p>

**Class:** `AddedLink`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `NodeValue` | A URI or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### AddedTriple

<p class="tdb-f">Specify an edge pattern which was *added* at *this commit*.</p>

**Class:** `AddedTriple`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `Value` | A URI, datatype or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### And

<p class="tdb-f">A conjunction of queries which must all have a solution.</p>

**Class:** `And`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `and` | `Query` | List of queries which must hold. |

---

### ArithmeticExpression

<p class="tdb-f">An abstract class specifying the AST super-class of all arithemtic expressions. It is a subdocument</p>

**Class:** `ArithmeticExpression`

---

### ArithmeticValue

<p class="tdb-f">A variable or node. It is a subdocument</p>

**Class:** `ArithmeticValue`

**Super class:** `ArithmeticExpression`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `data` | `xsd:anySimpleType` | An xsd data type value. |
| `variable` | `xsd:string` | A variable. |

---

### Column

<p class="tdb-f">Description pending.</p>

**Class:** `Column`

---

### Concatenate

<p class="tdb-f">Concatenate a list of strings.</p>

**Class:** `Concatenate`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `list` | `DataValue` | The list to concatenate. |
| `result` | `DataValue` | The result string. |

---

### Count

<p class="tdb-f">Counts the number of solutions of a query.</p>

**Class:** `Count`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The query from which to obtain the count. |
| `count` | `DataValue` | The count of the number of solutions. |

---

### Data

<p class="tdb-f">Specify an edge pattern which is terminal, and provides a data value association.</p>

**Class:** `Data`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `DataValue` | A data type or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### DataValue

<p class="tdb-f">A variable or node. It is a subdocument</p>

**Class:** `DataValue`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `data` | `xsd:anySimpleType` | An xsd data type value. |
| `variable` | `xsd:string` | A variable. |
| `list` | `DataValue` | A list of datavalues |

---

### DeleteDocument

<p class="tdb-f">Delete a document from the graph.</p>

**Class:** `DeleteDocument`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `identifier` | `NodeValue` | An identifier specifying the documentation location to delete. |

---

### DeleteLink

<p class="tdb-f">Delete an edge linking nodes.</p>

**Class:** `DeleteLink`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. The variable must be bound. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. The variable must be bound. |
| `object` | `NodeValue` | A URI or variable which is the target or object of the graph edge. The variable must be bound. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### DeleteTriple

<p class="tdb-f">Specify an edge pattern to remove from the graph.</p>

**Class:** `DeleteTriple`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `Value` | A URI, datatype or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### DeletedLink

<p class="tdb-f">An edge pattern specifying a link beween nodes deleted *at this commit*.</p>

**Class:** `DeletedLink`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `NodeValue` | A URI or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### DeletedTriple

<p class="tdb-f">Specify an edge pattern which was *deleted* at *this commit*.</p>

**Class:** `DeletedTriple`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `Value` | A URI, datatype or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### Distinct

<p class="tdb-f">Ensure variables listed result in distinct solutions.</p>

**Class:** `Distinct`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `variables` | `xsd:string` | The variables which must be distinct from the query. |
| `query` | `Query` | The query which will be run prior to selection. |

---

### Div

<p class="tdb-f">Integer divide two numbers.</p>

**Class:** `Div`

**Super class:** `ArithmeticExpression`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `ArithmeticExpression` | First operand of div. |
| `right` | `ArithmeticExpression` | Second operand of div. |

---

### Divide

<p class="tdb-f">Divide two numbers.</p>

**Class:** `Divide`

**Super class:** `ArithmeticExpression`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `ArithmeticExpression` | First operand of divide. |
| `right` | `ArithmeticExpression` | Second operand of divide. |

---

### Dot

<p class="tdb-f">Extract the value of a key in a bound document.</p>

**Class:** `Dot`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `star` | `undefined` | A path patterns. |

---

### Equals

<p class="tdb-f">True whenever 'left' is the same as 'right'. Performs unification.</p>

**Class:** `Equals`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `child` | `undefined` | A URI, data value or variable. |
| `parent` | `undefined` | A URI, data value or variable. |

---

### Eval

<p class="tdb-f">Evaluate an arithmetic expression to obtain a result.</p>

**Class:** `Eval`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `expression` | `ArithmeticExpression` | The expression to be evaluated. |
| `result` | `ArithmeticValue` | The numeric result. |

---

### Exp

<p class="tdb-f">Exponentiate a number.</p>

**Class:** `Exp`

**Super class:** `ArithmeticExpression`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `ArithmeticExpression` | The base. |
| `right` | `ArithmeticExpression` | The exponent. |

---

### Floor

<p class="tdb-f">Find the integral part of a number.</p>

**Class:** `Floor`

**Super class:** `ArithmeticExpression`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `argument` | `ArithmeticExpression` | The number to floor. |

---

### FormatType

<p class="tdb-f">Description pending.</p>

**Class:** `FormatType`

---

### From

<p class="tdb-f">Change the default read graph (between instance/schema).</p>

**Class:** `From`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The subquery with a new default graph. |
| `graph_filter` | `xsd:string` | The graph filter: 'schema' or 'instance' or '*'. |

---

### Get

<p class="tdb-f">Description pending.</p>

**Class:** `Get`

**Super class:** `Query`

---

### Greater

<p class="tdb-f">Predicate determining if one thing is greater than another according to natural ordering.</p>

**Class:** `Greater`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `DataValue` | The greater element. |
| `right` | `DataValue` | The lesser element. |

---

### GroupBy

<p class="tdb-f">Group a query into a list with each element of the list specified by 'template' using a given variable set for the group.</p>

**Class:** `GroupBy`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `template` | `xsd:string` | The template of elements in the result list. |
| `group_by` | `xsd:string` | The variables which should be grouped into like solutions. |
| `query` | `Query` | The subquery providing the solutions for the grouping. |
| `grouped` | `Value` | The final list of templated solutions. |

---

### HashKey

<p class="tdb-f">Generates a key identical to those generated automatically by 'HashKey' specifications.</p>

**Class:** `HashKey`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `base` | `DataValue` | The URI base to the left of the key. |
| `key_list` | `DataValue` | List of data elements required to generate the key. |
| `uri` | `NodeValue` | The resulting URI. |

---

### If

<p class="tdb-f">A conditional which runs the then clause for every success from the test clause, otherwise runs the else clause.</p>

**Class:** `If`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `test` | `Query` | A query which will provide bindings for the then clause. |
| `then` | `Query` | A query which will run for every solution of test with associated bindings. |
| `else` | `Query` | A query which runs whenever test fails. |

---

### Immediately

<p class="tdb-f">Attempts to perform all side-effecting operations immediately. Can have strange non-backtracking effects but can also increase performance. Use at your own risk.</p>

**Class:** `Immediately`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The query from which to obtain the side-effects. |

---

### Indicator

<p class="tdb-f">Description pending.</p>

**Class:** `Indicator`

---

### Into

<p class="tdb-f">Change the default write graph (between instance/schema).</p>

**Class:** `Into`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The subquery with a new default write graph. |
| `graph` | `xsd:string` | The graph filter: schema or instance. |

---

### InversePathPredicate

<p class="tdb-f">A predicate to traverse *backwards*.</p>

**Class:** `InversePathPredicate`

**Super class:** `PathPattern`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `predicate` | `xsd:string` | The predicate to use in reverse direction in the pattern traversal. |

---

### IsA

<p class="tdb-f">Test (or generate) the type of an element.</p>

**Class:** `IsA`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `element` | `NodeValue` | The element to test. |
| `type` | `NodeValue` | The type of the element. |

---

### Join

<p class="tdb-f">Join a list of strings using 'separator'.</p>

**Class:** `Join`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `list` | `DataValue` | The list to concatenate. |
| `separator` | `DataValue` | The separator between each joined string |
| `result` | `DataValue` | The result string. |

---

### Length

<p class="tdb-f">The length of a list.</p>

**Class:** `Length`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `list` | `DataValue` | The list of which to find the length. |
| `length` | `DataValue` | The length of the list. |

---

### Less

<p class="tdb-f">Predicate determining if one thing is less than another according to natural ordering.</p>

**Class:** `Less`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `DataValue` | The lesser element. |
| `right` | `DataValue` | The greater element. |

---

### LexicalKey

<p class="tdb-f">Generates a key identical to those generated automatically by 'LexicalKey' specifications.</p>

**Class:** `LexicalKey`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `base` | `DataValue` | The URI base to the left of the key. |
| `key_list` | `DataValue` | List of data elements required to generate the key. |
| `uri` | `NodeValue` | The resulting URI. |

---

### Like

<p class="tdb-f">Distance between strings, similar to a Levenstein distance.</p>

**Class:** `Like`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `DataValue` | The first string. |
| `right` | `DataValue` | The second string. |
| `similarity` | `DataValue` | Number between -1 and 1 which gives a scale for similarity. |

---

### Limit

<p class="tdb-f">Limit a query to a particular maximum number of solutions specified by 'limit'. Can be used with start to perform paging.</p>

**Class:** `Limit`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The query to perform. |
| `limit` | `xsd:nonNegativeInteger` | Maximum number of solutions. |

---

### Link

<p class="tdb-f">Specify an edge pattern which is not terminal, but a link between objects.</p>

**Class:** `Link`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `NodeValue` | A URI or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### Lower

<p class="tdb-f">Lowercase a string.</p>

**Class:** `Lower`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `mixed` | `DataValue` | The mixed case string. |
| `uppser` | `undefined` | The lower case string. |

---

### Member

<p class="tdb-f">Generate or test every element of a list.</p>

**Class:** `Member`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `member` | `DataValue` | The element to test for membership or to supply as generated. |
| `list` | `DataValue` | The list of elements against which to generate or test. |

---

### Minus

<p class="tdb-f">Subtract two numbers.</p>

**Class:** `Minus`

**Super class:** `ArithmeticExpression`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `ArithmeticExpression` | First operand of minus. |
| `right` | `ArithmeticExpression` | Second operand of minus. |

---

### NamedParametricQuery

<p class="tdb-f">A named parametric query which names a specific query for later retrieval and re-use and allows the specification of bindings for a specific set of variables in the query.</p>

**Class:** `NamedParametricQuery`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `name` | `xsd:string` | The name of the NamedParametricQuery to be retrieved. |
| `parameters` | `xsd:string` | Variable name list for auxilliary bindings. |
| `query` | `Query` | The query AST as WOQL JSON. |

---

### NamedQuery

<p class="tdb-f">A named query names a specific query for later retrieval and re-use.</p>

**Class:** `NamedQuery`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `name` | `xsd:string` | The name of the NamedQuery to be retrieved |
| `query` | `Query` | The query AST as WOQL JSON |

---

### NodeValue

<p class="tdb-f">A variable or node. It is a subdocument</p>

**Class:** `NodeValue`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `node` | `xsd:string` | A URI representing a resource. |
| `variable` | `xsd:string` | A variable. |

---

### Not

<p class="tdb-f">The negation of a query. Provides no solution bindings, but will succeed if its sub-query fails.</p>

**Class:** `Not`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The query which must not hold. |

---

### Once

<p class="tdb-f">Obtains exactly one solution from a query. Simliar to a limit of 1.</p>

**Class:** `Once`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The query from which to obtain a solution. |

---

### Optional

<p class="tdb-f">A query which will succeed (without bindings) even in the case of failure.</p>

**Class:** `Optional`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The query to run. |

---

### Or

<p class="tdb-f">A disjunction of queries any of which can provide a solution.</p>

**Class:** `Or`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `or` | `Query` | List of queries which may hold. |

---

### Order

<p class="tdb-f">Description pending.</p>

**Class:** `Order`

---

### OrderBy

<p class="tdb-f">Orders query results according to an ordering specification.</p>

**Class:** `OrderBy`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The base query giving the solutions to order. |
| `ordering` | `OrderTemplate` | A specification of the ordering of solutions. |

---

### OrderTemplate

<p class="tdb-f">The order template, consisting of the variable and ordering direction. It is a subdocument</p>

**Class:** `OrderTemplate`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `order` | `Order` | An enum either 'asc' or 'desc'. |
| `variable` | `xsd:string` | The variable to order. |

---

### Pad

<p class="tdb-f">Pad a string.</p>

**Class:** `Pad`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `string` | `DataValue` | The starting string. |
| `char` | `DataValue` | The padding character. |
| `times` | `DataValue` | The number of times to repeat the padding character. |
| `result` | `DataValue` | The result of the padding as a string. |

---

### Path

<p class="tdb-f">Find a path through the graph according to 'pattern'. This 'pattern' is a regular graph expression which avoids cycles.</p>

**Class:** `Path`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `Value` | The starting node. |
| `pattern` | `PathPattern` | The pattern which describes how to traverse edges. |
| `object` | `Value` | The ending node. |
| `path` | `Value` | An optional list of edges traversed. |

---

### PathOr

<p class="tdb-f">A set of patterns in which each of the patterns can result in objects starting from our current subject set.</p>

**Class:** `PathOr`

**Super class:** `PathPattern`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `or` | `PathPattern` | A disjunction of path patterns. |

---

### PathPattern

<p class="tdb-f">Description pending.</p>

**Class:** `PathPattern`

---

### PathPlus

<p class="tdb-f">The path pattern specified by 'plus' must hold one or more times in succession.</p>

**Class:** `PathPlus`

**Super class:** `PathPattern`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `plus` | `PathPattern` | A path patterns. |

---

### PathPredicate

<p class="tdb-f">A predicate to traverse.</p>

**Class:** `PathPredicate`

**Super class:** `PathPattern`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `predicate` | `xsd:string` | The predicate to use in the pattern traversal. |

---

### PathSequence

<p class="tdb-f">A sequence of patterns in which each of the patterns in the list must result in objects which are subjects of the next pattern in the list.</p>

**Class:** `PathSequence`

**Super class:** `PathPattern`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `sequence` | `PathPattern` | A sequence of path patterns. |

---

### PathStar

<p class="tdb-f">The path pattern specified by 'star' may hold zero or more times in succession.</p>

**Class:** `PathStar`

**Super class:** `PathPattern`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `star` | `PathPattern` | A path patterns. |

---

### PathTimes

<p class="tdb-f">The path pattern specified by 'times' may hold 'from' to 'to' times in succession.</p>

**Class:** `PathTimes`

**Super class:** `PathPattern`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `star` | `undefined` | A path patterns. |

---

### Plus

<p class="tdb-f">Add two numbers.</p>

**Class:** `Plus`

**Super class:** `ArithmeticExpression`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `left` | `ArithmeticExpression` | First operand of add. |
| `right` | `ArithmeticExpression` | Second operand of add. |

---

### Put

<p class="tdb-f">Description pending.</p>

**Class:** `Put`

**Super class:** `Query`

---

### Query

<p class="tdb-f">An abstract class which represents an arbitrary query AST. It is a subdocument</p>

**Class:** `Query`

---

### QueryResource

<p class="tdb-f">Description pending.</p>

**Class:** `QueryResource`

---

### RandomKey

<p class="tdb-f">Generates a key identical to those generated automatically by 'RandomKey' specifications.</p>

**Class:** `RandomKey`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `base` | `DataValue` | The URI base to the left of the key. |
| `uri` | `NodeValue` | The resulting URI. |

---

### ReadDocument

<p class="tdb-f">Read a full document from an identifier.</p>

**Class:** `ReadDocument`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `document` | `DataValue` | Variable which will be bound to the document. |
| `identifier` | `NodeValue` | The URI of the document to load. |

---

### Regexp

<p class="tdb-f">Test a string against a PCRE style regex pattern.</p>

**Class:** `Regexp`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `pattern` | `DataValue` | The PCRE style pattern. |
| `string` | `DataValue` | The string to test. |
| `result` | `DataValue` | An optional result list of matches. |

---

### Select

<p class="tdb-f">Select specific variables from a query to return.</p>

**Class:** `Select`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `variables` | `xsd:string` | The variables to select from the query. |
| `query` | `Query` | The query which will be run prior to selection. |

---

### Size

<p class="tdb-f">Size of a database in magic units (bytes?).</p>

**Class:** `Size`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `resource` | `xsd:string` | The resource to obtain the size of. |
| `size` | `DataValue` | The size. |

---

### Source

<p class="tdb-f">Description pending.</p>

**Class:** `Source`

---

### Split

<p class="tdb-f">Split a string.</p>

**Class:** `Split`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `string` | `DataValue` | The starting string. |
| `pattern` | `DataValue` | The splitting pattern. |
| `list` | `DataValue` | The result list of strings. |

---

### Start

<p class="tdb-f">Start a query at the nth solution specified by 'start'. Allows resumption and paging of queries.</p>

**Class:** `Start`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `query` | `Query` | The query to perform. |
| `start` | `xsd:nonNegativeInteger` | The numbered solution to start at. |

---

### Substring

<p class="tdb-f">Finds the boundaries of a substring in a string.</p>

**Class:** `Substring`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `string` | `DataValue` | The super-string as data or variable. |
| `substring` | `DataValue` | The super-string as data or variable. |
| `length` | `DataValue` | The length of the string as an integer or variable. |
| `before` | `DataValue` | The count of characters before substring as an integer or variable. |
| `after` | `DataValue` | The count of characters after substring as an integer or variable. |

---

### Subsumption

<p class="tdb-f">Provides class subsumption (the inheritance model) according to the schema design. True whenver 'child' is a child of 'parent'. Can be used as a generator or a check.</p>

**Class:** `Subsumption`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `child` | `NodeValue` | The child class as a URI or variable. |
| `parent` | `NodeValue` | The parent class as a URI or variable |

---

### Sum

<p class="tdb-f">Sum a list of strings.</p>

**Class:** `Sum`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `list` | `DataValue` | The list of numbers to sum. |
| `result` | `DataValue` | The result of the sum as a number. |

---

### Trim

<p class="tdb-f">Trims whitespace from 'untrimmed'.</p>

**Class:** `Trim`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `untrimmed` | `DataValue` | The untrimmed string. |
| `trimmed` | `DataValue` | The string to be trimmed. |

---

### Triple

<p class="tdb-f">Specify an edge pattern in the graph.</p>

**Class:** `Triple`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `subject` | `NodeValue` | A URI or variable which is the source or subject of the graph edge. |
| `predicate` | `NodeValue` | A URI or variable which is the edge-label or predicate of the graph edge. |
| `object` | `Value` | A URI, datatype or variable which is the target or object of the graph edge. |
| `graph` | `xsd:string` | An optional graph (either 'instance' or 'schema') |

---

### TripleCount

<p class="tdb-f">The number of edges in a database.</p>

**Class:** `TripleCount`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `resource` | `xsd:string` | The resource to obtain the edges from. |
| `count` | `DataValue` | The count of edges. |

---

### True

<p class="tdb-f">The query which is always true.</p>

**Class:** `True`

**Super class:** `Query`

---

### Typecast

<p class="tdb-f">Casts one type as another if possible.</p>

**Class:** `Typecast`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `value` | `Value` | The value to cast. |
| `type` | `NodeValue` | The type to which to cast. |
| `result` | `Value` | The resulting value after cast. |

---

### UpdateDocument

<p class="tdb-f">Update a document in the graph.</p>

**Class:** `UpdateDocument`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `document` | `DataValue` | The document to update. Must either have an '@id' or have a class specified key. |
| `identifier` | `NodeValue` | An optional returned identifier specifying the documentation location. |

---

### Upper

<p class="tdb-f">Uppercase a string.</p>

**Class:** `Upper`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `mixed` | `DataValue` | The mixed case string. |
| `uppser` | `undefined` | The upper case string. |

---

### Using

<p class="tdb-f">Select a specific collection for query.</p>

**Class:** `Using`

**Super class:** `Query`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `collection` | `xsd:string` | The resource over which to run the query. |
| `query` | `Query` | The query which will be run on the selected collection. |

---

### Value

<p class="tdb-f">A variable, node or data point. It is a subdocument</p>

**Class:** `Value`

**Properties:**

| Property | Range  | Desc |
| -------- | ------ | ---- |
| `node` | `xsd:string` | A URI representing a resource. |
| `variable` | `xsd:string` | A variable. |
| `list` | `Value` | A list of datavalues |
| `data` | `xsd:anySimpleType` | An xsd data type value. |

---