:- module('document/patch',
          [simple_patch/3]).

:- use_module(core(util)).

:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(lists)).
:- use_module(library(when)).

/*

# Patch and Diff

Patch applies a diff to obtain a new object. We will need to be able
to infer Diffs from objects, or from between diff different data
products.

A patch can be applied either singularly or in bulk to a patch
endpoint which will apply the patch to the specified data product
resource.

We specify patch using the operations described below.

## Copy Diff

Copy is implicit

All properties which are not specifically mentioned will be considered
part of an implicit copy. This will make patches more compressed and
easier to specify by hand.

## Mandatory Diff

`@before`/`@after` instructions contain objects specified as tightly as
required to obtain ids, or as ids.

```jsx
{ '@id' : "Person/jim",
  'date_of_birth' : { '@op' : 'SwapValue',
                      '@before' : "1928-03-05",
                      '@after' : "1938-03-05"
                    }}
```

## Optional Diff

Optionals also contain `@before`/`@after` designations, but potentially
`null` fields to describe missing elements.

```jsx
{ '@id' : "Object/my_object",
  'name' : { '@op' : 'SwapValue',
             '@before' : null,
             '@after' : "Jim" }}
```

## Set Diff / Cardinality Diff

Set requires the ability to explicitly remove or add elements - we can do this by maintaining a `@before`/`@after` with a list of those which exist *only* on the left, and *only* on the right.


## List Diff

The list diff requires swaps at a position.  We use, `@copy`, `@swap` and `@keep`.

### Copy List

Copy the previous list from `From_Position` to `To_Position.

```jsx
{ "@op" : "CopyList",
  "@to" : To_Position,
  "@rest" : Diff }
```

### Swap List

Swap out the list starting from the current point from `Previous` to
`Next`. This can be used to extend, or drop elements as well as do
full replacement.

```jsx
{ "@op" : "SwapList",
  "@before" : Previous,
  "@after" : Next,
  "@rest" : Diff }
```

### Patch List

Patch the list starting from the current point with the patch list in
"@patch". The patch must be less than or equal to the length of the list.

```jsx
{ "@op" : "PatchList",
  "@patch" : Patch,
  "@rest" : Diff }
```

### Example:

```jsx
var Patch =
{ '@id' : "TaskList/my_tasks",
  'tasks' : { '@op' : "CopyList",                      % Replace List
              '@to' : 2,
              '@rest' : { '@op' : "PatchList",
                          '@patch' : [{ '@op' : "SwapValue",
                                        '@before' : "Task/shopping",
                                        '@after' : "Task/climbing"},
                                      { '@op' : "SwapValue",
                                        '@before' : "Task/cleaning",
                                        '@after' : "Task/dining"},
                                      { '@op' : "SwapValue",
                                        '@before' : "Task/fishing",
                                        '@after' : "Task/travelling"}],
                          '@rest' : { '@op' : "KeepList" } } }}
var Before =
{ '@id' : "TaskList/my_tasks",
  'tasks' : ["Task/driving", "Task/reading", "Task/shopping",
             "Task/cleaning","Task/fishing", "Task/arguing"] }
var After =
{ '@id' : "TaskList/my_tasks",
  'tasks' : ["Task/driving", "Task/reading", "Task/climbing",
             "Task/dining", "Task/travelling", "Task/arguing"] }

```

## Array Diff

Arrays will allow index swaping or "shrink" and "grow".

## Force Diff

A "Force Diff" will set the value of a location regardless of current
read-state. This is a potentially unsafe operation as there is no
guarantee we are seeing the object state version we think we
are.

```jsx
{ '@id' : "Employee/012" ,
  'name' : { '@op' : 'ForceValue',
             '@after' : "Jake" }}
```

## Table Diff

A Table diff requires swaps at two positions and subdivision of each patch into squares: Top-Left (in which we make the patch) Top-Right, Bottom-Left and Bottom-Right, each of which will be computed with the help of an additional Diff.

We use `CopyTable`, `SwapTable` and `KeepTable`.

Schematically the diff is a context with a the current hole in the
upper-right hand corner as follows:

```jsx

-----------------------
|          |          |
| Swap /   |   Top    |
| Copy     |   Right  |
| instr.   |   Diff   |
|          |          |
-----------------------
|          |          |
|  Bttom   |  Bottom  |
|  Left    |  Right   |
|  Diff    |  Diff    |
|          |          |
-----------------------

```

We will recursively patch the table by applying the diffs in the
various corners.

### Example Table

This might apply to an object as follows:

```jsx
{ '@id' : "Excel/012" ,
  'sheets' : [{ '@id' : "Excel/012/sheet/Sheet/1",
                'cells' :
                { '@op' : "SwapTable",
                  '@before' : [[ { 'Value' : "10", ... },
                                 { 'Value' : "20", ... },
                                 { 'Value' : "30", ... } ],
                               [ { 'Value' : "40", ... },
                                 { 'Value' : "50", ... },
                                 { 'Value' : "60", ... } ]
                               [ { 'Value' : "70", ... },
                                 { 'Value' : "80", ... },
                                 { 'Value' : "90", ... } ] ],
                  '@after' : [[ { 'Value' : "1", ... },
                                { 'Value' : "2", ... },
                                { 'Value' : "3", ... } ],
                              [ { 'Value' : "4", ... },
                                { 'Value' : "5", ... },
                                { 'Value' : "6", ... } ]
                              [ { 'Value' : "7", ... },
                                { 'Value' : "8", ... },
                                { 'Value' : "9", ... } ] ],
                  '@after' : { '@op' : "KeepTable" },
                  '@bottom_left' : { '@op' : "KeepTable" },
                  '@top_right' : { '@op' : "KeepTable" },
                  '@bottom_right' : { '@op' : "KeepTable" }
                }}]}
```

Application would take a table through the following transformation:

```
| 10 | 20 | 30 | A | B | C |
| 40 | 50 | 60 | D | E | F |
| 70 | 80 | 90 | G | H | I |
| X  | Y  | Z  | O | O | O |
| X  | Y  | Z  | O | O | O |
| X  | Y  | Z  | O | O | O |

=>

| 1  | 2  | 3  | A | B | C |
| 4  | 5  | 6  | D | E | F |
| 7  | 8  | 9  | G | H | I |
| X  | Y  | Z  | O | O | O |
| X  | Y  | Z  | O | O | O |
| X  | Y  | Z  | O | O | O |

```


###  Copy Table

```jsx
{ '@op' : "CopyTable"
  '@to_row' : To_Row,           % integer
  '@to_column' : To_Column,     % integer
  '@bottom_left' : Diff_BL,     % A Table Diff
  '@top_right' : Diff_TR,       % A Table Diff
  '@bottom_right' : Diff_BR     % A Table Diff
  }
```

### Swap Table

Swap isntructions will give a before table as a JSON list of lists for
both the before and after. These tables need not have the same
dimensions. This operation subsumes extension and drop of rows and
columns as well as full replacement.

```jsx
{ '@op' : "SwapTable",
  '@before' : Diff_Before,
  '@after' : Diff_After,
  '@bottom_left' : Diff_BL,
  '@top_right' : Diff_TR,
  '@bottom_right' : DIff_BR
  }
```

### Patch Table

Patch the table starting from the current point with the patch table in
`@patch`. The patch must be less than or equal to the length of the list.

```jsx
{ "@op" : "PatchTable",
  "@patch" : Patch,
  '@bottom_left' : Diff_BL,
  '@top_right' : Diff_TR,
  '@bottom_right' : DIff_BR
}
```


### Keep Table

`@keep` instructions are degenerate copies.

```
{ '@keep' : "Table" }
```

Examples:
---------

```
Diff := {
          '@id' : ID % ID of object to change.
          <prop1> : { '@op' : 'SwapValue',
                      '@before' : Obj_Old                      % Mandatory
                      '@after' : Obj_New },
          <prop2> : { '@op' : 'SwapValue',
                      '@before' : null                         % Add optional
                      '@after' : Obj_New },
          <prop2> : { '@op' : 'SwapValue',
                      '@before' : Obj_Old                      % Drop optional
                      '@after' : null },
          <prop3> : { '@id' : ID1,
                      <prop3_1> :                              % Deep swap [*must* be subdocuments]
                        { '@id' : ID2,
                           <prop3_2> : ...
                           { '@id' : ID3,
                              <prop3_n> : { '@op' : 'SwapValue',
                                            '@before' : Obj_Old,
                                            '@after' : Obj_New }
                        ... } } },
          <prop4> : { '@op' : 'CopyList',                      % Replace List
                      '@to' : 10,
                      '@rest' : { '@op' : 'SwapList',          % Replace List
                                  '@before' : [1,2,3],
                                  '@after' : [4,5,6],
                                  '@rest' : { '@keep' : "List" } } },
          <prop4> : { '@op' : "CopyTable"
                      '@to_row' : 3,
                      '@to_column' : 3,
                      '@bottom_left' : { '@keep' : "KeepTable" },
                      '@top_right' :  { '@keep' : "KeepTable" },
                      '@bottom_right' : { '@swap' : "SwapTable",
                                          '@before' : [[1,2,3]],
                                          '@after' : [[4,5,6]],
                                          '@bottom_left' : { '@keep' : "KeepTable" },
                                          '@top_right' : { '@keep' : "KeepTable" },
                                          '@bottom_right' : { '@keep' : "KeepTable" } }
                    },
          <prop5> : { '@op' : "SwapValue",
                      '@before' : { '@id' : ID },              % Replace element of a set
                      '@after' : { '@id' : ID }}
          <prop6> : { '@id' : ID,                            % Deep set replace
                       <prop6_1> : { '@op' : "SwapValue",
                                     '@before' : ...,
                                     '@after' : ... } },
          <prop6> : [{ '@id' : ID1,                          % Deep set replace 2
                        <prop6_1> : { '@op' : "SwapValue",
                                      '@before' : ...,
                                      '@after' : ... } },
                     { '@id' : ID2,                          % Deep set replace 2
                        <prop6_2> : { '@op' : "SwapValue",
                                      '@before' : ...,
                                      '@after' : ... } }],
          <prop6> : { '@op' : 'ForceValue',
                      '@after' : "Value" }                   % Ignore read state and force value

        }
```

Examples of Patch:

```jsx
var Original = {
        '@id': "EmployeesFromCSV/001",
        '@type': "EmployeesFromCSV",
        employee_id: "001",
        name: "Destiny Norris",
        team: "Marketing",
        title: "Marketing Manager"
      },
var Diff = {
        '@id': "EmployeesFromCSV/001",
        name: { '@op' : 'SwapValue', '@before' : "Destiny Norris", '@after' : "Destiny Morris" },
      },
var Final = {
        '@id': "EmployeesFromCSV/001",
        '@type': "EmployeesFromCSV",
        employee_id: "001",
        name: "Destiny Norris",
        team: "Marketing",
        title: "Marketing Manager"
      },
patch(Diff,Original,Final).

=> true
```

# Patch and Diff Endpoints

The Patch and Diff endpoints expose endpoints for clients to obtain
diffs or patches of data.

## Diff

The diff endpoint takes a POST of two JSON documents, *before*, and
*after*. This endpoint then returns a 200 and a patch which takes
*before* to *after* if applied using the patch inteface.

The payload is structured as a JSON document, with two keys,
`"before"` and `"after"`, pointing to the documents you would like to
diff.

An example of the payload:

```json
{ "before" : { "@id" : "Person/Jane", "@type" : Person", "name" : "Jane"},
  "after" :  { "@id" : "Person/Jane", "@type" : Person", "name" : "Janine"}}
```

Which would result in the following patch:

```json
{ "name" : { "@op" : "SwapValue", "@before" : "Jane", "@after": "Janine" }}
```

## Patch

Patch takes a POST with a *before* document and a *patch* and produces an *after*
document.

```json
{ "before" : { "@id" : "Person/Jane", "@type" : Person", "name" : "Jane"}
  "patch" : {"name" : { "@op" : "ValueSwap", "@before" : "Jane", "@after": "Janine" }}}
```

Resulting in the following document:

```json
{ "@id" : "Person/Jane", "@type" : Person", "name" : "Janine"}
```


*/

simple_patch(Diff,JSON_In,JSON_Out) :-
    is_dict(JSON_In),
    !,
    dict_keys(JSON_In,Doc_Keys),
    dict_keys(Diff,Diff_Keys),
    union(Doc_Keys,Diff_Keys,Keys),
    pairs_and_conflicts_from_keys(Keys,JSON_In,Diff,Pairs),
    dict_create(JSON_Out,_,Pairs).
simple_patch(Diff,Before,After) :-
    is_list(Diff),
    !,
    maplist([D,B,A]>>simple_patch(D,B,A), Diff, Before, After).
simple_patch(Diff,Before,After) :-
    diff_op(Diff,Op),
    simple_op_diff_value(Op, Diff, Before, After).
simple_patch(Before,Before,Before).

simple_patch_list(Diff,List_In,List_Out) :-
    is_list(List_In),
    !,
    maplist([Patch,Elt,Patched]>>simple_patch(Patch,Elt,Patched),
            Diff,List_In,List_Out).

simple_patch_table(Diff,Table_In,Table_Out) :-
    is_list(Table_In),
    !,
    row_length(Diff,Diff_Row_Length),
    column_length(Diff,Diff_Column_Length),
    row_length(Table_In,Row_Length),
    column_length(Table_In,Column_Length),
    Diff_Row_Length = Row_Length,
    Diff_Column_Length = Column_Length,
    maplist([Diff_Row,Row_In,Row_Out]>>
            maplist([Patch,Elt,Patched]>>
                    simple_patch(Patch,Elt,Patched),
                    Diff_Row,Row_In,Row_Out),
            Diff,Table_In,Table_Out).

%%% Table utils
row_length(T, Length) :-
    length(T, Length).

column_length([], 0).
column_length([R|_], Length) :-
    length(R, Length).

split(Index,List,Left,Right) :-
    length(Left, Index),
    append(Left,Right,List).

split_matrix(In, N, M, Top_Left, Right, Bottom, Bottom_Right) :-
    when((   nonvar(In)
         ;   nonvar(Rows_Top),
             nonvar(Rows_Bottom)),
         split_row(In, N, Rows_Top, Rows_Bottom)),
    split_column(Rows_Top, M, Top_Left, Right),
    split_column(Rows_Bottom, M, Bottom, Bottom_Right).

split_row(In, N, Top, Bottom) :-
    split(N, In, Top, Bottom).

split_column([], _, [], []) :-
    !.
split_column(L, 0, [], L) :-
    !.
split_column([R|Rows], N, [R|Rows], []) :-
    length(R,N),
    !.
split_column([R|Rows], N, [Left|Top], [Right|Bottom]) :-
    split(N, R, Left, Right),
    split_column(Rows, N, Top, Bottom).


%%% Conflict utils
pairs_and_conflicts_from_keys([], _, _, []).
pairs_and_conflicts_from_keys([Key|Keys], JSON, Diff, [Result|Values]) :-
    simple_patch_key_value(Key,JSON,Diff,Result),
    !,
    pairs_and_conflicts_from_keys(Keys, JSON, Diff, Values).


%%% Patch operations
simple_op_diff_value('ForceValue', Diff, _, After) :-
    get_dict('@after', Diff, After).
simple_op_diff_value('SwapValue', Diff, Before, After) :-
    get_dict('@before', Diff, Before),
    get_dict('@after', Diff, After).
simple_op_diff_value('CopyList', Diff, Before, After) :-
    get_dict('@to', Diff, To),
    get_dict('@rest', Diff, Next_Diff),
    nth_list_prefix_suffix(To,Before,Prefix,Suffix),
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op,Next_Diff,Suffix, Rest),
    append(Prefix,Rest,After).
simple_op_diff_value('SwapList', Diff, Before, After) :-
    get_dict('@before', Diff, Before_Prefix),
    get_dict('@after', Diff, After_Prefix),
    get_dict('@rest', Diff, Next_Diff),
    append(Before_Prefix,Before_Suffix,Before),
    append(After_Prefix,After_Suffix,After),
    diff_op(Next_Diff,Op),
    simple_op_diff_value(Op,Next_Diff, Before_Suffix, After_Suffix).
simple_op_diff_value('PatchList', Diff, Before, After) :-
    %% This should recover when the patch list length is smaller instead of failing.
    get_dict('@patch', Diff, Patch),
    get_dict('@rest', Diff, Rest),
    length(Patch, To),
    nth_list_prefix_suffix(To,Before,Prefix_Before,Suffix_Before),
    simple_patch_list(Patch,Prefix_Before,Prefix_After),
    diff_op(Rest,Op),
    simple_op_diff_value(Op,Rest,Suffix_Before,Suffix_After),
    append(Prefix_After,Suffix_After,After).
simple_op_diff_value('KeepList', _Diff, Same, Same).
simple_op_diff_value('CopyTable', Diff, Before, After) :-
    get_dict('@to_row', Diff, To_Row),
    get_dict('@to_column', Diff, To_Column),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    split_matrix(Before,To_Row,To_Column,Top_Left,Top_Right,Bottom_Left,Bottom_Right),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New),
    split_matrix(After, To_Row, To_Column, Top_Left, TR_New, BL_New, BR_New).
simple_op_diff_value('SwapTable', Diff, Before, After) :-
    get_dict('@before', Diff, Top_Left_Before),
    get_dict('@after', Diff, Top_Left_After),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    row_length(Top_Left_Before,To_Row_Before),
    column_length(Top_Left_Before,To_Column_Before),
    split_matrix(Before,To_Row_Before,To_Column_Before,
                 Top_Left_Before,Top_Right,Bottom_Left,Bottom_Right),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New),
    row_length(Top_Left_After,To_Row_After),
    column_length(Top_Left_After,To_Column_After),
    split_matrix(After, To_Row_After, To_Column_After, Top_Left_After, TR_New, BL_New, BR_New).
simple_op_diff_value('PatchTable', Diff, Before, After) :-
    get_dict('@top_left', Diff, TL_Diff),
    get_dict('@bottom_left', Diff, BL_Diff),
    get_dict('@top_right', Diff, TR_Diff),
    get_dict('@bottom_right', Diff, BR_Diff),
    row_length(TL_Diff,To_Row),
    column_length(TL_Diff,To_Column),
    split_matrix(Before,To_Row,To_Column,
                 Top_Left_Before,Top_Right,Bottom_Left,Bottom_Right),
    simple_patch_table(TL_Diff,Top_Left_Before, TL_New),
    diff_op(BL_Diff,BL_Op),
    simple_op_diff_value(BL_Op,BL_Diff,Bottom_Left,BL_New),
    diff_op(TR_Diff,TR_Op),
    simple_op_diff_value(TR_Op,TR_Diff,Top_Right,TR_New),
    diff_op(BR_Diff,BR_Op),
    simple_op_diff_value(BR_Op,BR_Diff,Bottom_Right,BR_New),
    row_length(Top_Left_After,To_Row_After),
    column_length(Top_Left_After,To_Column_After),
    split_matrix(After, To_Row_After, To_Column_After, TL_New, TR_New, BL_New, BR_New).
simple_op_diff_value('KeepTable', _Diff, Same, Same).

diff_op(Diff, Op) :-
    is_dict(Diff),
    get_dict('@op', Diff, Operation_String),
    atom_string(Op, Operation_String).

get_dict_or_null(Key,JSON,V) :-
    (   get_dict(Key,JSON,V)
    ->  true
    ;   V = null).

simple_patch_key_value(Key,JSON,Diff,Result) :-
    % If it's in the diff, we're changing it.
    get_dict(Key,Diff,Key_Diff),
    !,
    (   is_list(Key_Diff)
    ->  get_dict_or_null(Key,JSON,V),
        simple_patch_list(Key_Diff,V,Value),
        Result = Key-Value
    ;   diff_op(Key_Diff,Op)
    ->  get_dict_or_null(Key,JSON,V),
        simple_op_diff_value(Op,Key_Diff,V,Value),
        Result = Key-Value
    ;   get_dict_or_null(Key,JSON,V),
        simple_patch(Key_Diff,V,Value),
        Result = Key-Value
    ).
simple_patch_key_value(Key,JSON,_Diff,Key-Value) :-
    % This is an implicit copy instruction
    get_dict(Key,JSON,Value).

%%% List util
nth_list_prefix_suffix(0,List,[],List) :-
    !.
nth_list_prefix_suffix(N,[H|T],[H|Pr],Rest) :-
    M is N - 1,
    nth_list_prefix_suffix(M,T,Pr,Rest).


:- begin_tests(simple_patch).
:- use_module(core(util/test_utils)).

test(flat_patch, []) :-
    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },
    Patch = _{ name : _{ '@op' : "SwapValue",
                         '@before' : "jim",
                         '@after' : "james" }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "james",
                dob : "2009-08-03"
              },

    simple_patch(Patch,Before,After).

test(bad_read_state, []) :-
    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },
    Patch = _{ name : _{ '@op' : "SwapValue",
                         '@before' : "jake",
                         '@after' : "james" }
             },
    \+ simple_patch(Patch,Before,_After).

test(flat_patch_missing, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim"
              },
    Patch = _{ dob : _{ '@op' : "SwapValue",
                        '@before' : null,
                        '@after' :  "2009-08-03" }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },

    simple_patch(Patch,Before,After).

test(flat_drop_value, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob : "2009-08-03"
              },
    Patch = _{ dob : _{ '@op' : "SwapValue",
                        '@before' : "2009-08-03",
                        '@after' : null }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                dob: null
              },

    simple_patch(Patch,Before,After).


test(deep_swap_drop_value, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "Beethoven",
                dob : "1770-12-17",
                address :
                _{
                    address1 : "Probusgasse 6",
                    address2 : "Heiligenstadt",
                    city : "Vienna",
                    country : "Austria"
                }
              },
    Patch = _{ address :
               _{ address1 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Probusgasse 6",
                                 '@after' : "Mölker Bastei 8" },
                  address2 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Heiligenstadt",
                                 '@after' : null }
                }
             },
    After =  _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "Beethoven",
                dob : "1770-12-17",
                address :
                _{
                    address1 : "Mölker Bastei 8",
                    address2 : null,
                    city : "Vienna",
                    country : "Austria"
                }
              },

    simple_patch(Patch,Before,After).

test(deep_swap_wrong_value, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "Beethoven",
                dob : "1770-12-17",
                address :
                _{
                    address1 : "Probusgasse 6",
                    address2 : "Heiligenstadt",
                    city : "Vienna",
                    country : "Austria"
                }
              },
    Patch = _{ address :
               _{ address1 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Probusgasse 7",
                                 '@after' : "Mölker Bastei 8" },
                  address2 : _{
                                 '@op' : "SwapValue",
                                 '@before' : "Heiligenstadt",
                                 '@after' : null }
                }
             },

    \+ simple_patch(Patch,Before,_After).

test(flat_list_diff, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                friends : ["Person/2","Person/3","Person/4"]
              },
    Patch = _{ friends : _{ '@op' : "CopyList",
                            '@to' : 1,
                            '@rest' :
                            _{'@op' : "SwapList",
                              '@before' : ["Person/3"],
                              '@after' : [],
                              '@rest' : _{'@op':"KeepList"}
                             }
                          }
             },

    simple_patch(Patch,Before,After),
    After = _{ '@id' : "Person/1",
               '@type' : "Person",
               friends : ["Person/2","Person/4"],
               name : "jim"}.


test(flat_table_diff, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                table : [[1,2,3],
                         [4,5,6],
                         [7,8,9]]
              },
    Patch = _{ table : _{ '@op' : "CopyTable",
                          '@to_row' : 2,
                          '@to_column' : 2,
                          '@bottom_left' :
                          _{ '@op' : "KeepTable" },
                          '@top_right' :
                          _{ '@op' : "KeepTable" },
                          '@bottom_right' :
                           _{ '@op' : "SwapTable",
                              '@before' : [[9]],
                              '@after' : [[10]],
                              '@bottom_left' :
                              _{ '@op' : "KeepTable" },
                              '@top_right' :
                              _{ '@op' : "KeepTable" },
                              '@bottom_right' :
                              _{ '@op' : "KeepTable" }
                            }
                        }
             },

    simple_patch(Patch,Before,After),
    After = _{ '@id' : "Person/1",
               '@type' : "Person",
               name : "jim",
               table: [[1,2,3],
                       [4,5,6],
                       [7,8,10]]}.


test(flat_table_diff_conflict, []) :-

    Before = _{ '@id' : "Person/1",
                '@type' : "Person",
                name : "jim",
                table : [[1,2,3],
                         [4,5,6],
                         [7,8,9]]
              },
    Patch = _{ table : _{ '@op' : "CopyTable",
                          '@to_row' : 2,
                          '@to_column' : 2,
                          '@bottom_left' :
                          _{ '@op' : "KeepTable" },
                          '@top_right' :
                          _{ '@op' : "KeepTable" },
                          '@bottom_right' :
                           _{ '@op' : "SwapTable",
                              '@before' : [[8]],
                              '@after' : [[10]],
                              '@bottom_left' :
                              _{ '@op' : "KeepTable" },
                              '@top_right' :
                              _{ '@op' : "KeepTable" },
                              '@bottom_right' :
                              _{ '@op' : "KeepTable" }
                            }
                        }
             },

    \+ simple_patch(Patch,Before,_After).

test(deep_list_patch, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                addresses : [
                    _{ '@id' : "Person/jim/Address/addresses/1",
                       '@type' : "Address",
                       address1 : "Mölker Bastei 8",
                       address2 : null,
                       city : "Vienna",
                       country : "Austria"},
                    _{ '@id' : "Person/jim/Address/addresses/2",
                       '@type' : "Address",
                       address1 : "Probusgasse 7",
                       address2 : null,
                       city : "Vienna",
                                country : "Austria"}
                ]
              },
    Patch = _{ addresses :
               _{ '@op' : "CopyList",
                  '@to' : 1,
                  '@rest' :
                  _{ '@op' : "PatchList",
                     '@patch' : [
                         _{
                             address1
                             : _{ '@op' : "SwapValue",
                                  '@before' : "Probusgasse 6",
                                  '@after' : "Probusgasse 6" }
                         }],
                     '@rest' : _{ '@op' : "KeepList" }
                   }
                }
             },

    \+ simple_patch(Patch,Before,_After).

:- use_module(library(http/json)).

test(deep_table_patch, []) :-

    Before = _{ '@type': "Excel",
                'Name': "testdoc.xlsx",
                'Sheets': [
                     _{
                         '@type': "Sheet",
                         'Name': "Sheet1",
                         'Cells': [[ _{ '@type': "Cell",
                                        'Value': "1.0",
                                        'Text': "1" },
                                      _{ '@type': "Cell",
                                        'Value': "2.0",
                                        'Text': "2" },
                                      _{ '@type': "Cell",
                                        'Value': "3.0",
                                        'Text': "3" }],
                                   [ _{ '@type': "Cell",
                                        'Value': "4.0",
                                        'Text': "4" },
                                     _{ '@type': "Cell",
                                        'Value': "5.0",
                                        'Text': "5" },
                                     _{ '@type': "Cell",
                                        'Value': "6.0",
                                        'Text': "6" }],
                                   [ _{ '@type': "Cell",
                                        'Value': "7.0",
                                        'Text': "7" },
                                     _{ '@type': "Cell",
                                        'Value': "8.0",
                                        'Text': "8" },
                                     _{ '@type': "Cell",
                                        'Value': "9.0",
                                        'Text': "9" }]]
                     }
                ]},

    Patch = _{ 'Sheets' :
               _{ '@op' : "PatchList",
                  '@patch' : [
                      _{ 'Cells' :
                         _{ '@op' : "PatchTable",
                            '@top_left' :
                            [[ _{ 'Value' : _{ '@op' : "SwapValue",
                                               '@before' : "1.0",
                                               '@after' : "2.0" },
                                  'Text' : _{ '@op' : "SwapValue",
                                              '@before' : "1",
                                              '@after' : "2" }},
                               _{ 'Value' : _{ '@op' : "SwapValue",
                                               '@before' : "2.0",
                                               '@after' : "3.0" },
                                  'Text' : _{ '@op' : "SwapValue",
                                              '@before' : "2",
                                              '@after' : "3" }},
                               _{ 'Value' : _{ '@op' : "SwapValue",
                                               '@before' : "3.0",
                                               '@after' : "4.0" },
                                  'Text' : _{ '@op' : "SwapValue",
                                              '@before' : "3",
                                              '@after' : "4" }}]],
                            '@bottom_left' : _{ '@op' : "KeepTable" },
                            '@top_right' : _{ '@op' : "KeepTable" },
                            '@bottom_right' : _{ '@op' : "KeepTable" }}}
                  ],
                  '@rest' : _{ '@op' : "KeepList" }}},

    simple_patch(Patch,Before,After),

    After = _{'@type':"Excel",
              'Name':"testdoc.xlsx",
              'Sheets':[_{'@type':"Sheet",
                          'Cells':[[_{'@type':"Cell",'Text':"2",'Value':"2.0"},
                                    _{'@type':"Cell",'Text':"3",'Value':"3.0"},
                                    _{'@type':"Cell",'Text':"4",'Value':"4.0"}],
                                   [_{'@type':"Cell",'Text':"4",'Value':"4.0"},
                                    _{'@type':"Cell",'Text':"5",'Value':"5.0"},
                                    _{'@type':"Cell",'Text':"6",'Value':"6.0"}],
                                   [_{'@type':"Cell",'Text':"7",'Value':"7.0"},
                                    _{'@type':"Cell",'Text':"8",'Value':"8.0"},
                                    _{'@type':"Cell",'Text':"9",'Value':"9.0"}]],
                          'Name':"Sheet1"}]}.

test(read_state, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig"
              },
    Patch = _{ '@id' : "Person/Ludwig",
               name : _{ '@op' : "SwapValue",
                         '@before' : "Ludwig",
                         '@after' : "Ludo" }
             },
    simple_patch(Patch,Before,After),

    After = _{ '@id' : "Person/Ludwig",
               '@type' : "Person",
               name : "Ludo"
             }.

test(deep_read_state, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                             '@type' : "Address",
                             address1 : "Mölker Bastei 8",
                             address2 : null,
                             city : "Vienna",
                             country : "Austria"}
              },
    Patch = _{ name:_{'@after':"Ludo",
                      '@before':"Ludwig",
                      '@op':"SwapValue"},
               address: _{ city : "Vienna" }
             },
    simple_patch(Patch,Before,After),
    After = _{'@id':"Person/Ludwig",
              '@type':"Person",
              name:"Ludo",
              address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                           '@type' : "Address",
                           address1 : "Mölker Bastei 8",
                           address2 : null,
                           city : "Vienna",
                           country : "Austria"}
             }.

test(deep_read_state_failure, []) :-

    Before = _{ '@id' : "Person/Ludwig",
                '@type' : "Person",
                name : "Ludwig",
                address : _{ '@id' : "Person/Ludwig/Address/addresses/1",
                             '@type' : "Address",
                             address1 : "Mölker Bastei 8",
                             address2 : null,
                             city : "Vienna",
                             country : "Austria"}
              },
    Patch = _{ name:_{'@after':"Ludo",
                      '@before':"Ludwig",
                      '@op':"SwapValue"},
               address: _{ city : "Timbuktu" }
             },
    \+ simple_patch(Patch,Before,_After).


:- end_tests(simple_patch).

