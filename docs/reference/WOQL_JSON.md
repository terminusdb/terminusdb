# WOQL

This is the JSON-LD definition of the WOQL language. Those wishing to
implement clients for TerminusDB should construct JSON-LD messages according
to this ontology.

# Terminus DB WOQL Query Syntax Schema (`http://terminusdb.com/schema/woql`)

Authors: Gavin Mendel-Gleason Kevin C. Feeney

Schema describing all the structure of the WOQL query language

## Class:  Node (`woql:Node`)

An arbitrary node identifier

### Super classes 
 *  (`woql:NodeRestriction`)



### Property:  node (`woql:node`)
#### Range:  (`owl:Thing`)

The URI of an object of an edge





## Class:  Variable (`woql:Variable`)

A WOQL logic variable

### Super classes 
 *  (`woql:VariableNameRestriction`)



### Property:  Variable Name (`woql:variable_name`)
#### Range:  (`xsd:string`)

A name for the variable





## Class:  Array Element (`woql:ArrayElement`)

Box for an element of an array

### Super classes 
 *  Indexable (`woql:Indexable`)
 *  (`woql:IndexableRestriction`)
 *  (`woql:Value`)




## Class:  Node (`woql:Datatype`)

An arbitrary node identifier

### Super classes 
 *  (`woql:DatatypeRestriction`)



### Property:  datatype (`woql:datatype`)
#### Range:  (`xsd:anySimpleType`)

The datatype value of an object of an edge





## Class:  Indexable (`woql:Indexable`)

Something with an index

### Super classes 
 *  (`woql:IndexableRestriction`)



### Property:  index (`woql:index`)
#### Range:  (`xsd:nonNegativeInteger`)

The index of a list





## Class:  Query (`woql:Query`)

A WOQL Query

### Super classes 
 *  Document Class (`system:Document`)



### Property:  document (`woql:document`)
#### Range:  (`owl:Thing`)

Document associated with a query





## Class:  query list element (`woql:QueryListElement`)

An element of a list of queries

### Super classes 
 *  Document Class (`system:Document`)
 *  Indexable (`woql:Indexable`)
 *  (`woql:IndexableRestriction`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)




## Class:  Query with indexable sub query (`woql:QueryWithIndexedSubQuery`)

A Query with an indexed sub-query

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  query list (`woql:query_list`)
#### Range:  query list element (`woql:QueryListElement`)

A list of queries





## Class:  Query With Subquery (`woql:QueryWithSubQuery`)

A WOQL Query with a subquery

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)



### Property:  (`woql:query`)
#### Range:  Query (`woql:Query`)







## Class:  And (`woql:And`)

A conjunction of clauses

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  Query with indexable sub query (`woql:QueryWithIndexedSubQuery`)




## Class:  Using (`woql:Comment`)

Introduce a default collection.
{'@type' : 'Comment',
 'comment' : collection_descriptor}


### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)



### Property:  comment (`woql:comment`)
#### Range:  (`xsd:string`)

A comment





## Class:  From (`woql:From`)

A query with a graph filter for selection

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  Query with graph filter (`woql:QueryWithGraphFilter`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)




## Class:  Into (`woql:Into`)

A query with a graph for selection

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  Query with graph (`woql:QueryWithGraph`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)




## Class:  Or (`woql:Or`)

A disjunction of clauses

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  Query with indexable sub query (`woql:QueryWithIndexedSubQuery`)




## Class:  Query with graph (`woql:QueryWithGraph`)

A query with a specified graph

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  graph (`woql:graph`)
#### Range:  (`xsd:string`)

the string to form a graph descriptor





## Class:  Query with graph filter (`woql:QueryWithGraphFilter`)

A query with a graph filter

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  graph filter (`woql:graph_filter`)
#### Range:  (`xsd:string`)

the string to form a graph filter descriptor





## Class:  Select (`woql:Select`)

A Selection of variables from subquery

### Super classes 
 *  Document Class (`system:Document`)
 *  Has Variable List (`woql:HasVariableList`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)




## Class:  Using (`woql:Using`)

Introduce a default collection.
{'@type' : 'Using',
 'collection' : collection_descriptor,
 'query' : query}


### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)



### Property:  Collection (`woql:collection`)
#### Range:  (`xsd:string`)

A collection





## Class:  Variable List Element (`woql:VariableListElement`)

An element of a list by position

### Super classes 
 *  Indexable (`woql:Indexable`)
 *  (`woql:IndexableRestriction`)
 *  Variable (`woql:Variable`)
 *  (`woql:VariableNameRestriction`)




## Class:  Binary Operator (`woql:BinaryOperator`)

A binary operator, with left and right

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  left (`woql:left`)
#### Range:  (`woql:Value`)

The left of a Binary Operator




### Property:  right (`woql:right`)
#### Range:  (`woql:Value`)

The right of a Binary Operator





## Class:  Equals (`woql:Equals`)

Defines unification between two things

### Super classes 
 *  Document Class (`system:Document`)
 *  Binary Operator (`woql:BinaryOperator`)
 *  Query (`woql:Query`)




## Class:  Quad (`woql:Quad`)

A triple with graph filter

### Super classes 
 *  Document Class (`system:Document`)
 *  Object (`woql:HasObject`)
 *  Predicate (`woql:HasPredicate`)
 *  Subject (`woql:HasSubject`)
 *  Query (`woql:Query`)
 *  Query with graph filter (`woql:QueryWithGraphFilter`)
 *  Triple (`woql:Triple`)




## Class:  Substring (`woql:Substring`)

Find substrings of a given string

### Super classes 
 *  Document Class (`system:Document`)
 *  HasLength (`woql:HasLength`)
 *  Query (`woql:Query`)



### Property:  after (`woql:after`)
#### Range:  (`woql:Value`)

The after characters index of a substring search




### Property:  before (`woql:before`)
#### Range:  (`woql:Value`)

The before characters index of a substring search




### Property:  string (`woql:string`)
#### Range:  (`woql:Value`)

The string of a substring search




### Property:  substring (`woql:substring`)
#### Range:  (`woql:Value`)

The substring of a substring search





## Class:  Subsumption (`woql:Subsumption`)

Subsumption of one class by another

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  child (`woql:child`)
#### Range:  (`owl:Thing`)

The child of a subsumption




### Property:  parent (`woql:parent`)
#### Range:  (`owl:Thing`)

The parent of a subsumption





## Class:  Triple (`woql:Triple`)

A edge in the graph specified by subject, predicate, object

### Super classes 
 *  Document Class (`system:Document`)
 *  Object (`woql:HasObject`)
 *  Predicate (`woql:HasPredicate`)
 *  Subject (`woql:HasSubject`)
 *  Query (`woql:Query`)




## Class:  Document Query (`woql:DocumentQuery`)

A query which references a document

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)




## Class:  Read Object (`woql:ReadObject`)

Read an object as JSON

### Super classes 
 *  Document Class (`system:Document`)
 *  Document Query (`woql:DocumentQuery`)
 *  Query (`woql:Query`)




## Class:  Add quad (`woql:AddQuad`)

Add a quad

### Super classes 
 *  Document Class (`system:Document`)
 *  Object (`woql:HasObject`)
 *  Predicate (`woql:HasPredicate`)
 *  Subject (`woql:HasSubject`)
 *  Query (`woql:Query`)
 *  Query with graph (`woql:QueryWithGraph`)
 *  Triple (`woql:Triple`)




## Class:  Add triple (`woql:AddTriple`)

Add a triple

### Super classes 
 *  Document Class (`system:Document`)
 *  Object (`woql:HasObject`)
 *  Predicate (`woql:HasPredicate`)
 *  Subject (`woql:HasSubject`)
 *  Query (`woql:Query`)
 *  Triple (`woql:Triple`)




## Class:  Value (`woql:ArithmeticValue`)

An arithmetic value - float, int, bignum, etc.

### Super classes 
 *  (`woql:Value`)




## Class:  As Var (`woql:AsVar`)

As Var

### Super classes 
 *  Variable (`woql:Variable`)
 *  (`woql:VariableNameRestriction`)



### Property:  var type (`woql:var_type`)
#### Range:  (`xsd:string`)

An AsVar casting type





## Class:  Binary Arithmetic Operator (`woql:BinaryArithmeticOperator`)

Binary Arithmetic Operator

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)



### Property:  first (`woql:first`)
#### Range:  Arithmetic Expression (`woql:ArithmeticExpression`)

The first argument of a binary operator




### Property:  second (`woql:second`)
#### Range:  Arithmetic Expression (`woql:ArithmeticExpression`)

The second argument of a binary operator





## Class:  Concatenate (`woql:Concatenate`)

Concatenate a list of strings

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  list (`woql:concat_list`)
#### Range:  (`woql:Value`)

The list of elements to concatenate




### Property:  concatcatenated (`woql:concatenated`)
#### Range:  (`woql:Value`)

The string which has been concatenated





## Class:  Delete Object (`woql:DeleteObject`)

Delete an object as JSON

### Super classes 
 *  Document Class (`system:Document`)
 *  Document Query (`woql:DocumentQuery`)
 *  Query (`woql:Query`)




## Class:  Delete quad (`woql:DeleteQuad`)

Delete a quad

### Super classes 
 *  Document Class (`system:Document`)
 *  Object (`woql:HasObject`)
 *  Predicate (`woql:HasPredicate`)
 *  Subject (`woql:HasSubject`)
 *  Query (`woql:Query`)
 *  Query with graph (`woql:QueryWithGraph`)
 *  Triple (`woql:Triple`)




## Class:  Delete triple (`woql:DeleteTriple`)

Delete a triple

### Super classes 
 *  Document Class (`system:Document`)
 *  Object (`woql:HasObject`)
 *  Predicate (`woql:HasPredicate`)
 *  Subject (`woql:HasSubject`)
 *  Query (`woql:Query`)
 *  Triple (`woql:Triple`)




## Class:  Div (`woql:Div`)

Integer division

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)
 *  Binary Arithmetic Operator (`woql:BinaryArithmeticOperator`)




## Class:  Divide (`woql:Divide`)

Arithmetic division

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)
 *  Binary Arithmetic Operator (`woql:BinaryArithmeticOperator`)




## Class:  Eval (`woql:Eval`)

Eval an arithmetic expression

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  expression (`woql:expression`)
#### Range:  Arithmetic Expression (`woql:ArithmeticExpression`)

The arithmetic expression under evaluation




### Property:  result (`woql:result`)
#### Range:  (`woql:Value`)







## Class:  Exp (`woql:Exp`)

Arithmetic exponentiation

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)
 *  Binary Arithmetic Operator (`woql:BinaryArithmeticOperator`)




## Class:  File Query Resource (`woql:FileResource`)

A File Query Resource

### Super classes 
 *  Query Resource (`woql:QueryResource`)



### Property:  file (`woql:file`)
#### Range:  (`xsd:string`)

File associated with a query resource





## Class:  Floor (`woql:Floor`)

The floor of a floating point number

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)
 *  Binary Arithmetic Operator (`woql:UnaryArithmeticOperator`)




## Class:  Get (`woql:Get`)

Get variable bindings from a resource

### Super classes 
 *  Document Class (`system:Document`)
 *  Get or Put (`woql:GetOrPut`)
 *  Query (`woql:Query`)




## Class:  Get or Put (`woql:GetOrPut`)

Abstract class for Get or Put

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  as vars (`woql:as_vars`)
#### Range:  As Var (`woql:AsVar`)

The variable captures associated with a get




### Property:  resource (`woql:query_resource`)
#### Range:  Query Resource (`woql:QueryResource`)

A resource for woql:Get





## Class:  Greater (`woql:Greater`)

One value greater than another

### Super classes 
 *  Document Class (`system:Document`)
 *  Binary Operator (`woql:BinaryOperator`)
 *  Query (`woql:Query`)




## Class:  Group By (`woql:GroupBy`)

Group a query by a spec

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)



### Property:  grouped (`woql:group_by`)
#### Range:  Variable List Element (`woql:VariableListElement`)

Variable in which to place the grouping




### Property:  grouped (`woql:group_template`)
#### Range:  Variable List Element (`woql:VariableListElement`)

Variable in which to place the grouping




### Property:  group var (`woql:grouped`)
#### Range:  (`woql:Value`)

Variable which to group





## Class:  ID Gen (`woql:IDGenerator`)

A reversible unique identifier generated from a Base and a Key

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  URI Generator (`woql:URIGenerator`)




## Class:  Is A (`woql:IsA`)

Check that Object 'is a' Type

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  element (`woql:element`)
#### Range:  (`owl:Thing`)

The element of an IsA being type checked




### Property:  of type (`woql:of_type`)
#### Range:  (`owl:Thing`)

Element is 'of type'





## Class:  Join (`woql:Join`)

Join a list of strings

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  join (`woql:join`)
#### Range:  (`woql:Value`)

The join of the list




### Property:  join list (`woql:join_list`)
#### Range:  (`woql:Value`)

The list of things to be joined




### Property:  join separator (`woql:join_separator`)
#### Range:  (`woql:Value`)

The separator in a join list





## Class:  Length (`woql:Length`)

Find the length of a collection

### Super classes 
 *  Document Class (`system:Document`)
 *  HasLength (`woql:HasLength`)
 *  Query (`woql:Query`)



### Property:  list (`woql:length_list`)
#### Range:  (`woql:Value`)

The thing of which we want a length





## Class:  Less (`woql:Less`)

One value less than another

### Super classes 
 *  Document Class (`system:Document`)
 *  Binary Operator (`woql:BinaryOperator`)
 *  Query (`woql:Query`)




## Class:  Like (`woql:Like`)

An element is like another element

### Super classes 
 *  Document Class (`system:Document`)
 *  Binary Operator (`woql:BinaryOperator`)
 *  Query (`woql:Query`)



### Property:  similarity (`woql:like_similarity`)
#### Range:  (`woql:Value`)

The similarity value (between 0 and 1)





## Class:  limit (`woql:Limit`)

A query limited by number of results

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)



### Property:  limit (`woql:limit`)
#### Range:  (`woql:Value`)

The limit of results to obtain





## Class:  Lower (`woql:Lower`)

Lowercase string left to string right

### Super classes 
 *  Document Class (`system:Document`)
 *  Binary Operator (`woql:BinaryOperator`)
 *  Query (`woql:Query`)




## Class:  Minus (`woql:Minus`)

Arithmetic subtraction

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)
 *  Binary Arithmetic Operator (`woql:BinaryArithmeticOperator`)




## Class:  A var by column or header name (`woql:NamedAsVar`)

A var by column or header name

### Super classes 
 *  As Var (`woql:AsVar`)
 *  Variable (`woql:Variable`)
 *  (`woql:VariableNameRestriction`)



### Property:  identifier (`woql:identifier`)
#### Range:  (`xsd:string`)

Identifier for a column name





## Class:  Not (`woql:Not`)

Not the query

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)




## Class:  Optional (`woql:Optional`)

Query which is optional

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)




## Class:  Order By (`woql:OrderBy`)

Order a queries results by the given list

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)



### Property:  Variable Array (`woql:variable_ordering`)
#### Range:  Variable Ordering (`woql:VariableOrdering`)

An array of variables





## Class:  Pad (`woql:Pad`)

Pad a string with character C, N times, to get a result

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  pad char (`woql:pad_char`)
#### Range:  (`woql:Value`)

Character with which to pad to string




### Property:  pad result (`woql:pad_result`)
#### Range:  (`woql:Value`)

Result of padding




### Property:  pad string (`woql:pad_string`)
#### Range:  (`woql:Value`)

String to pad




### Property:  pad times (`woql:pad_times`)
#### Range:  (`woql:Value`)

Number of times to pad woql:pad_char





## Class:  Plus (`woql:Plus`)

Arithmetic addition

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)
 *  Binary Arithmetic Operator (`woql:BinaryArithmeticOperator`)




## Class:  Post Query Resource (`woql:PostResource`)

An HTTP POST Query Resource

### Super classes 
 *  File Query Resource (`woql:FileResource`)
 *  Query Resource (`woql:QueryResource`)




## Class:  Put (`woql:Put`)

Put some variables bindings in a defined resource

### Super classes 
 *  Document Class (`system:Document`)
 *  Get or Put (`woql:GetOrPut`)
 *  Query (`woql:Query`)




## Class:  regexp (`woql:Regexp`)

A regular expression and its results

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  pattern (`woql:pattern`)
#### Range:  (`woql:Value`)

The pattern to match




### Property:  list (`woql:regexp_list`)
#### Range:  (`woql:Value`)

The list of results matched in the pattern




### Property:  string (`woql:regexp_string`)
#### Range:  (`woql:Value`)

The string to match against pattern





## Class:  Remote Query Resource (`woql:RemoteResource`)

A Query Resource

### Super classes 
 *  Query Resource (`woql:QueryResource`)



### Property:  remote uri (`woql:remote_uri`)
#### Range:  (`xsd:anyURI`)

Remote URI associated with a query resource





## Class:  Split (`woql:Split`)

split string on pattern result

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  split list (`woql:split_list`)
#### Range:  (`woql:Value`)

List of splits from string




### Property:  split pattern (`woql:split_pattern`)
#### Range:  (`woql:Value`)

Pattern on which to split a string




### Property:  split string (`woql:split_string`)
#### Range:  (`woql:Value`)

String to split





## Class:  start (`woql:Start`)

The starting point in result set

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)




## Class:  Sum (`woql:Sum`)

Sum of a list of numbers

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  start (`woql:start`)
#### Range:  (`woql:Value`)

The number from which to start




### Property:  sum (`woql:sum`)
#### Range:  (`woql:Value`)

The sum of the list




### Property:  sum list (`woql:sum_list`)
#### Range:  (`woql:Value`)

The list of things to be summed





## Class:  Times (`woql:Times`)

Arithmetic multiplication

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)
 *  Binary Arithmetic Operator (`woql:BinaryArithmeticOperator`)




## Class:  True (`woql:True`)

Always true

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)




## Class:  Typecast (`woql:Typecast`)

A typecast of a variable to a new type

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  typecast result (`woql:typecast_result`)
#### Range:  (`woql:Value`)

The casted value




### Property:  typecast type (`woql:typecast_type`)
#### Range:  (`woql:Value`)

The type we want to typecast to




### Property:  typecast value (`woql:typecast_value`)
#### Range:  (`woql:Value`)

The thing of which we want a typecast





## Class:  URI Generator (`woql:URIGenerator`)

A Generator for IDs

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  base (`woql:base`)
#### Range:  (`woql:Value`)

The document base of a URI




### Property:  key list (`woql:key_list`)
#### Range:  Value List (`woql:ValueList`)

The key list of a URI




### Property:  base (`woql:uri`)
#### Range:  (`woql:Value`)

The document base of a URI





## Class:  Binary Arithmetic Operator (`woql:UnaryArithmeticOperator`)

Binary Arithmetic Operator

### Super classes 
 *  Arithmetic Expression (`woql:ArithmeticExpression`)



### Property:  argument (`woql:argument`)
#### Range:  Arithmetic Expression (`woql:ArithmeticExpression`)

The argument of a unary operator





## Class:  Unique (`woql:Unique`)

An irreversible unique identifier generated from a Base and a Key

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  URI Generator (`woql:URIGenerator`)




## Class:  Update Object (`woql:UpdateObject`)

Update an object as JSON

### Super classes 
 *  Document Class (`system:Document`)
 *  Document Query (`woql:DocumentQuery`)
 *  Query (`woql:Query`)




## Class:  Upper (`woql:Upper`)

Uppercase string left to string right

### Super classes 
 *  Document Class (`system:Document`)
 *  Binary Operator (`woql:BinaryOperator`)
 *  Query (`woql:Query`)




## Class:  Value List Element (`woql:ValueListElement`)

A Value List Element

### Super classes 
 *  Indexable (`woql:Indexable`)
 *  (`woql:IndexableRestriction`)
 *  (`woql:Value`)




## Class:  Variable Ordering (`woql:VariableOrdering`)

A specific variable to de used in ordering

### Super classes 
 *  Indexable (`woql:Indexable`)
 *  (`woql:IndexableRestriction`)
 *  Variable (`woql:Variable`)
 *  Variable List Element (`woql:VariableListElement`)
 *  (`woql:VariableNameRestriction`)



### Property:  Ascending (`woql:ascending`)
#### Range:  (`xsd:boolean`)

Should the ordering be ascending





## Class:  When (`woql:When`)

When A, do B

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)
 *  (`woql:QueryWithSingleSubQuery`)
 *  Query With Subquery (`woql:QueryWithSubQuery`)



### Property:  consequent (`woql:consequent`)
#### Range:  Query (`woql:Query`)

The consequence of a woql:When





## Class:  Dot (`woql:Dot`)

Select from a dictionary

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  dictionary (`woql:dictionary`)
#### Range:  (`woql:Value`)

Dictionary of a selection




### Property:  key (`woql:dictionary_key`)
#### Range:  (`woql:Value`)

Dictionary key of a selection




### Property:  value (`woql:dictionary_value`)
#### Range:  (`woql:Value`)

Dictionary value of a selection





## Class:  Plus (`woql:Edge`)

Follow pattern at least once

### Super classes 
 *  Object (`woql:HasObject`)
 *  Predicate (`woql:HasPredicate`)
 *  Subject (`woql:HasSubject`)




## Class:  Has a resource (`woql:HasResource`)

Has a graph or collection resource descriptor

### Super classes 
 *  Document Class (`system:Document`)
 *  Query (`woql:Query`)



### Property:  resource (`woql:resource`)
#### Range:  (`xsd:string`)

A graph or collection resource descriptor





## Class:  Path (`woql:Path`)

A path query

### Super classes 
 *  Document Class (`system:Document`)
 *  Object (`woql:HasObject`)
 *  Has Pattern (`woql:HasPathPattern`)
 *  Subject (`woql:HasSubject`)
 *  Query (`woql:Query`)



### Property:  path (`woql:path`)
#### Range:  Path Pattern (`woql:PathPattern`)

The path taken by a pattern





## Class:  Sequence (`woql:PathOr`)

path_first followed by path_second

### Super classes 
 *  Path Pattern (`woql:PathPattern`)



### Property:  left (`woql:path_left`)
#### Range:  Path Pattern (`woql:PathPattern`)

The left branch pattern in a fork




### Property:  right (`woql:path_right`)
#### Range:  Path Pattern (`woql:PathPattern`)

The right branch pattern in a fork





## Class:  Plus (`woql:PathPlus`)

Follow pattern at least once

### Super classes 
 *  Has Pattern (`woql:HasPathPattern`)
 *  Path Pattern (`woql:PathPattern`)




## Class:  Predicate (`woql:PathPredicate`)

Contains a predicate to follow in a pattern

### Super classes 
 *  Path Pattern (`woql:PathPattern`)



### Property:  first (`woql:path_predicate`)
#### Range:  (`owl:Thing`)

Pattern to deploy first in a sequence





## Class:  Sequence (`woql:PathSequence`)

path_first followed by path_second

### Super classes 
 *  Path Pattern (`woql:PathPattern`)



### Property:  first (`woql:path_first`)
#### Range:  Path Pattern (`woql:PathPattern`)

Pattern to deploy first in a sequence




### Property:  second (`woql:path_second`)
#### Range:  Path Pattern (`woql:PathPattern`)

Pattern to deploy second in a sequence





## Class:  Star (`woql:PathStar`)

Follow pattern any number of times (including the empty transition)

### Super classes 
 *  Has Pattern (`woql:HasPathPattern`)
 *  Path Pattern (`woql:PathPattern`)




## Class:  Plus (`woql:PathTimes`)

Follow pattern at least once

### Super classes 
 *  Has Pattern (`woql:HasPathPattern`)
 *  Path Pattern (`woql:PathPattern`)



### Property:  maximum (`woql:path_maximum`)
#### Range:  (`xsd:nonNegativeInteger`)

Maximum applications of pattern.




### Property:  minimum (`woql:path_minimum`)
#### Range:  (`xsd:nonNegativeInteger`)

Minumum applications of pattern.





## Class:  Size (`woql:Size`)

Size of a resource

### Super classes 
 *  Document Class (`system:Document`)
 *  Has a resource (`woql:HasResource`)
 *  Query (`woql:Query`)



### Property:  size (`woql:size`)
#### Range:  (`woql:Value`)

Calculated size of a graph or collection resource





## Class:  Size (`woql:TripleCount`)

Size of a resource

### Super classes 
 *  Document Class (`system:Document`)
 *  Has a resource (`woql:HasResource`)
 *  Query (`woql:Query`)



### Property:  triple count (`woql:triple_count`)
#### Range:  (`woql:Value`)

Triple count of a graph or collection resource

