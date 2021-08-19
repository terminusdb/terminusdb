# WOQL schema

This is the WOQL schema. It gives a complete specification of the syntax of the WOQL query language. This allows WOQL queries to be checked for syntactic correctness, helps to prevent errors and detect conflicts in merge of queries, and allows the storage and retrieval of queries so that queries can be associated with data products.

 **Authored by:** Gavin

---

### AddData

<p class="tdb-f">Description pending.</p>

**Class:** `AddData`

**Super class:** `Query`

---

### AddLink

<p class="tdb-f">Description pending.</p>

**Class:** `AddLink`

**Super class:** `Query`

---

### AddTriple

<p class="tdb-f">Description pending.</p>

**Class:** `AddTriple`

**Super class:** `Query`

---

### AddedData

<p class="tdb-f">Description pending.</p>

**Class:** `AddedData`

**Super class:** `Query`

---

### AddedLink

<p class="tdb-f">Description pending.</p>

**Class:** `AddedLink`

**Super class:** `Query`

---

### AddedTriple

<p class="tdb-f">Description pending.</p>

**Class:** `AddedTriple`

**Super class:** `Query`

---

### And

<p class="tdb-f">Description pending.</p>

**Class:** `And`

**Super class:** `Query`

---

### ArithmeticExpression

<p class="tdb-f">Description pending.</p>

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

<p class="tdb-f">Description pending.</p>

**Class:** `Concatenate`

**Super class:** `Query`

---

### Count

<p class="tdb-f">Description pending.</p>

**Class:** `Count`

**Super class:** `Query`

---

### Data

<p class="tdb-f">Description pending.</p>

**Class:** `Data`

**Super class:** `Query`

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

<p class="tdb-f">Description pending.</p>

**Class:** `DeleteDocument`

**Super class:** `Query`

---

### DeleteLink

<p class="tdb-f">Description pending.</p>

**Class:** `DeleteLink`

**Super class:** `Query`

---

### DeleteTriple

<p class="tdb-f">Description pending.</p>

**Class:** `DeleteTriple`

**Super class:** `Query`

---

### DeletedLink

<p class="tdb-f">Description pending.</p>

**Class:** `DeletedLink`

**Super class:** `Query`

---

### DeletedTriple

<p class="tdb-f">Description pending.</p>

**Class:** `DeletedTriple`

**Super class:** `Query`

---

### Distinct

<p class="tdb-f">Description pending.</p>

**Class:** `Distinct`

**Super class:** `Query`

---

### Div

<p class="tdb-f">Description pending.</p>

**Class:** `Div`

**Super class:** `ArithmeticExpression`

---

### Divide

<p class="tdb-f">Description pending.</p>

**Class:** `Divide`

**Super class:** `ArithmeticExpression`

---

### Dot

<p class="tdb-f">Description pending.</p>

**Class:** `Dot`

**Super class:** `Query`

---

### Equals

<p class="tdb-f">Description pending.</p>

**Class:** `Equals`

**Super class:** `Query`

---

### Eval

<p class="tdb-f">Description pending.</p>

**Class:** `Eval`

**Super class:** `Query`

---

### Exp

<p class="tdb-f">Description pending.</p>

**Class:** `Exp`

**Super class:** `ArithmeticExpression`

---

### Floor

<p class="tdb-f">Description pending.</p>

**Class:** `Floor`

**Super class:** `ArithmeticExpression`

---

### FormatType

<p class="tdb-f">Description pending.</p>

**Class:** `FormatType`

---

### From

<p class="tdb-f">Description pending.</p>

**Class:** `From`

**Super class:** `Query`

---

### Get

<p class="tdb-f">Description pending.</p>

**Class:** `Get`

**Super class:** `Query`

---

### Greater

<p class="tdb-f">Description pending.</p>

**Class:** `Greater`

**Super class:** `Query`

---

### GroupBy

<p class="tdb-f">Description pending.</p>

**Class:** `GroupBy`

**Super class:** `Query`

---

### HashKey

<p class="tdb-f">Description pending.</p>

**Class:** `HashKey`

**Super class:** `Query`

---

### If

<p class="tdb-f">Description pending.</p>

**Class:** `If`

**Super class:** `Query`

---

### Immediately

<p class="tdb-f">Description pending.</p>

**Class:** `Immediately`

**Super class:** `Query`

---

### Indicator

<p class="tdb-f">Description pending.</p>

**Class:** `Indicator`

---

### Into

<p class="tdb-f">Description pending.</p>

**Class:** `Into`

**Super class:** `Query`

---

### InversePathPredicate

<p class="tdb-f">Description pending.</p>

**Class:** `InversePathPredicate`

**Super class:** `PathPattern`

---

### IsA

<p class="tdb-f">Description pending.</p>

**Class:** `IsA`

**Super class:** `Query`

---

### Join

<p class="tdb-f">Description pending.</p>

**Class:** `Join`

**Super class:** `Query`

---

### Length

<p class="tdb-f">Description pending.</p>

**Class:** `Length`

**Super class:** `Query`

---

### Less

<p class="tdb-f">Description pending.</p>

**Class:** `Less`

**Super class:** `Query`

---

### LexicalKey

<p class="tdb-f">Description pending.</p>

**Class:** `LexicalKey`

**Super class:** `Query`

---

### Like

<p class="tdb-f">Description pending.</p>

**Class:** `Like`

**Super class:** `Query`

---

### Limit

<p class="tdb-f">Description pending.</p>

**Class:** `Limit`

**Super class:** `Query`

---

### Link

<p class="tdb-f">Description pending.</p>

**Class:** `Link`

**Super class:** `Query`

---

### Lower

<p class="tdb-f">Description pending.</p>

**Class:** `Lower`

**Super class:** `Query`

---

### Member

<p class="tdb-f">Description pending.</p>

**Class:** `Member`

**Super class:** `Query`

---

### Minus

<p class="tdb-f">Description pending.</p>

**Class:** `Minus`

**Super class:** `ArithmeticExpression`

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

<p class="tdb-f">A named query names a specific query for later retrieval and re-use</p>

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

<p class="tdb-f">Description pending.</p>

**Class:** `Not`

**Super class:** `Query`

---

### Once

<p class="tdb-f">Description pending.</p>

**Class:** `Once`

**Super class:** `Query`

---

### Optional

<p class="tdb-f">Description pending.</p>

**Class:** `Optional`

**Super class:** `Query`

---

### Or

<p class="tdb-f">Description pending.</p>

**Class:** `Or`

**Super class:** `Query`

---

### Order

<p class="tdb-f">Description pending.</p>

**Class:** `Order`

---

### OrderBy

<p class="tdb-f">Description pending.</p>

**Class:** `OrderBy`

**Super class:** `Query`

---

### OrderTemplate

<p class="tdb-f">Description pending.</p>

**Class:** `OrderTemplate`

---

### Pad

<p class="tdb-f">Description pending.</p>

**Class:** `Pad`

**Super class:** `Query`

---

### Path

<p class="tdb-f">Description pending.</p>

**Class:** `Path`

**Super class:** `Query`

---

### PathOr

<p class="tdb-f">Description pending.</p>

**Class:** `PathOr`

**Super class:** `PathPattern`

---

### PathPattern

<p class="tdb-f">Description pending.</p>

**Class:** `PathPattern`

---

### PathPlus

<p class="tdb-f">Description pending.</p>

**Class:** `PathPlus`

**Super class:** `PathPattern`

---

### PathPredicate

<p class="tdb-f">Description pending.</p>

**Class:** `PathPredicate`

**Super class:** `PathPattern`

---

### PathSequence

<p class="tdb-f">Description pending.</p>

**Class:** `PathSequence`

**Super class:** `PathPattern`

---

### PathStar

<p class="tdb-f">Description pending.</p>

**Class:** `PathStar`

**Super class:** `PathPattern`

---

### PathTimes

<p class="tdb-f">Description pending.</p>

**Class:** `PathTimes`

**Super class:** `PathPattern`

---

### Plus

<p class="tdb-f">Description pending.</p>

**Class:** `Plus`

**Super class:** `ArithmeticExpression`

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

<p class="tdb-f">Description pending.</p>

**Class:** `RandomKey`

**Super class:** `Query`

---

### ReadDocument

<p class="tdb-f">Description pending.</p>

**Class:** `ReadDocument`

**Super class:** `Query`

---

### Regexp

<p class="tdb-f">Description pending.</p>

**Class:** `Regexp`

**Super class:** `Query`

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

<p class="tdb-f">Description pending.</p>

**Class:** `Size`

**Super class:** `Query`

---

### Source

<p class="tdb-f">Description pending.</p>

**Class:** `Source`

---

### Split

<p class="tdb-f">Description pending.</p>

**Class:** `Split`

**Super class:** `Query`

---

### Start

<p class="tdb-f">Description pending.</p>

**Class:** `Start`

**Super class:** `Query`

---

### Substring

<p class="tdb-f">Description pending.</p>

**Class:** `Substring`

**Super class:** `Query`

---

### Subsumption

<p class="tdb-f">Description pending.</p>

**Class:** `Subsumption`

**Super class:** `Query`

---

### Sum

<p class="tdb-f">Description pending.</p>

**Class:** `Sum`

**Super class:** `Query`

---

### Times

<p class="tdb-f">Description pending.</p>

**Class:** `Times`

**Super class:** `ArithmeticExpression`

---

### Trim

<p class="tdb-f">Description pending.</p>

**Class:** `Trim`

**Super class:** `Query`

---

### Triple

<p class="tdb-f">Description pending.</p>

**Class:** `Triple`

**Super class:** `Query`

---

### TripleCount

<p class="tdb-f">Description pending.</p>

**Class:** `TripleCount`

**Super class:** `Query`

---

### True

<p class="tdb-f">Description pending.</p>

**Class:** `True`

**Super class:** `Query`

---

### Typecast

<p class="tdb-f">Description pending.</p>

**Class:** `Typecast`

**Super class:** `Query`

---

### UpdateDocument

<p class="tdb-f">Description pending.</p>

**Class:** `UpdateDocument`

**Super class:** `Query`

---

### Upper

<p class="tdb-f">Description pending.</p>

**Class:** `Upper`

**Super class:** `Query`

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