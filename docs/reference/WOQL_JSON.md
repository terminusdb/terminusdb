# WOQL Schema

The WOQL schema describes the shape of a valid WOQL AST. This schema allows WOQL objects to be saved and retrieved from TerminusDB.

---

 ### AddData 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:AddData`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### AddLink 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:AddLink`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### AddTriple 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:AddTriple`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### AddedData 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:AddedData`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### AddedLink 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:AddedLink`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### AddedTriple 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:AddedTriple`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### And 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:And`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### ArithmeticExpression 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:ArithmeticExpression`

 **Super class:** None.

 **Properties:** None.

--- 

 ### ArithmeticValue 

 <p class="tdb-f">A variable or node. It is a subdocument</p>

 **Class:** `woql:ArithmeticValue`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:data` | `xsd:anySimpleType` | An xsd data type value. |
| `woql:variable` | `xsd:string` | A variable. |

--- 

 ### Column 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Column`

 **Super class:** None.

 **Properties:** None.

--- 

 ### Concatenate 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Concatenate`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Count 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Count`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Data 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Data`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### DataValue 

 <p class="tdb-f">A variable or node. It is a subdocument</p>

 **Class:** `woql:DataValue`

 **Super class:** None.

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:data` | `xsd:anySimpleType` | An xsd data type value. |
| `woql:variable` | `xsd:string` | A variable. |
| `woql:list` | `DataValue` | A list of datavalues |

--- 

 ### DeleteDocument 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:DeleteDocument`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### DeleteLink 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:DeleteLink`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### DeleteTriple 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:DeleteTriple`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### DeletedLink 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:DeletedLink`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### DeletedTriple 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:DeletedTriple`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Distinct 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Distinct`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Div 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Div`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** None.

--- 

 ### Divide 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Divide`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** None.

--- 

 ### Dot 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Dot`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Equals 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Equals`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Eval 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Eval`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Exp 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Exp`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** None.

--- 

 ### Floor 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Floor`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** None.

--- 

 ### FormatType 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:FormatType`

 **Super class:** None.

 **Properties:** None.

--- 

 ### From 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:From`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Get 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Get`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Greater 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Greater`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### GroupBy 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:GroupBy`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### HashKey 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:HashKey`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### If 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:If`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Immediately 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Immediately`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Indicator 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Indicator`

 **Super class:** None.

 **Properties:** None.

--- 

 ### Into 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Into`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### InversePathPredicate 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:InversePathPredicate`

 **Super class:** `woql:PathPattern`

 **Properties:** None.

--- 

 ### IsA 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:IsA`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Join 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Join`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Length 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Length`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Less 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Less`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### LexicalKey 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:LexicalKey`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Like 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Like`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Limit 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Limit`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Link 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Link`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Lower 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Lower`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Member 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Member`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Minus 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Minus`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** None.

--- 

 ### NamedParametricQuery 

 <p class="tdb-f">A named parametric query which names a specific query for later retrieval and re-use and allows the specification of bindings for a specific set of variables in the query.</p>

 **Class:** `woql:NamedParametricQuery`

 **Super class:** None.

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:name` | `xsd:string` | The name of the NamedParametricQuery to be retrieved. |
| `woql:parameters` | `xsd:string` | Variable name list for auxilliary bindings. |
| `woql:query` | `Query` | The query AST as WOQL JSON. |

--- 

 ### NamedQuery 

 <p class="tdb-f">A named query names a specific query for later retrieval and re-use</p>

 **Class:** `woql:NamedQuery`

 **Super class:** None.

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:name` | `xsd:string` | The name of the NamedQuery to be retrieved |
| `woql:query` | `Query` | The query AST as WOQL JSON |

--- 

 ### NodeValue 

 <p class="tdb-f">A variable or node. It is a subdocument</p>

 **Class:** `woql:NodeValue`

 **Super class:** None.

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:node` | `xsd:string` | A URI representing a resource. |
| `woql:variable` | `xsd:string` | A variable. |

--- 

 ### Not 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Not`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Once 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Once`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Optional 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Optional`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Or 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Or`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Order 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Order`

 **Super class:** None.

 **Properties:** None.

--- 

 ### OrderBy 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:OrderBy`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### OrderTemplate 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:OrderTemplate`

 **Super class:** None.

 **Properties:** None.

--- 

 ### Pad 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Pad`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Path 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Path`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### PathOr 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:PathOr`

 **Super class:** `woql:PathPattern`

 **Properties:** None.

--- 

 ### PathPattern 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:PathPattern`

 **Super class:** None.

 **Properties:** None.

--- 

 ### PathPlus 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:PathPlus`

 **Super class:** `woql:PathPattern`

 **Properties:** None.

--- 

 ### PathPredicate 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:PathPredicate`

 **Super class:** `woql:PathPattern`

 **Properties:** None.

--- 

 ### PathSequence 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:PathSequence`

 **Super class:** `woql:PathPattern`

 **Properties:** None.

--- 

 ### PathStar 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:PathStar`

 **Super class:** `woql:PathPattern`

 **Properties:** None.

--- 

 ### PathTimes 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:PathTimes`

 **Super class:** `woql:PathPattern`

 **Properties:** None.

--- 

 ### Plus 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Plus`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** None.

--- 

 ### Put 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Put`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Query 

 <p class="tdb-f">An abstract class which represents an arbitrary query AST. It is a subdocument</p>

 **Class:** `woql:Query`

 **Super class:** None.

 **Properties:** None.

--- 

 ### QueryResource 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:QueryResource`

 **Super class:** None.

 **Properties:** None.

--- 

 ### RandomKey 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:RandomKey`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### ReadDocument 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:ReadDocument`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Regexp 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Regexp`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Select 

 <p class="tdb-f">Select specific variables from a query to return.</p>

 **Class:** `woql:Select`

 **Super class:** `woql:Query`

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:variables` | `xsd:string` | The variables to select from the query. |
| `woql:query` | `Query` | The query which will be run prior to selection. |

--- 

 ### Size 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Size`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Source 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Source`

 **Super class:** None.

 **Properties:** None.

--- 

 ### Split 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Split`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Start 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Start`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Substring 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Substring`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Subsumption 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Subsumption`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Sum 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Sum`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Times 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Times`

 **Super class:** `woql:ArithmeticExpression`

 **Properties:** None.

--- 

 ### Trim 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Trim`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Triple 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Triple`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### TripleCount 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:TripleCount`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### True 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:True`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Typecast 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Typecast`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### UpdateDocument 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:UpdateDocument`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Upper 

 <p class="tdb-f">Description pending.</p>

 **Class:** `woql:Upper`

 **Super class:** `woql:Query`

 **Properties:** None.

--- 

 ### Using 

 <p class="tdb-f">Select a specific collection for query.</p>

 **Class:** `woql:Using`

 **Super class:** `woql:Query`

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:collection` | `xsd:string` | The resource over which to run the query. |
| `woql:query` | `Query` | The query which will be run on the selected collection. |

--- 

 ### Value 

 <p class="tdb-f">A variable, node or data point. It is a subdocument</p>

 **Class:** `woql:Value`

 **Super class:** None.

 **Properties:** 

| Property | Range  | Desc | 
| -------- | ------ | ---- |
| `woql:node` | `xsd:string` | A URI representing a resource. |
| `woql:variable` | `xsd:string` | A variable. |
| `woql:list` | `Value` | A list of datavalues |
| `woql:data` | `xsd:anySimpleType` | An xsd data type value. |

---
