:- module('document/patch',
          []).

:- use_module(core(util)).

/*

# Patch and Diff

Patch applies a diff to obtain a new object. We will need to be able
to infer Diffs from objects, or from between diff different data
products.

A patch can be applied either singularly or in bulk to a patch
endpoint which will apply the patch to the specified data product
resource.

We specify this patch using the following:

## Copy Diff

Copy is implicit

All properties which are not specifically mentioned will be considered
part of an implicit copy. This will make patches more compressed and
easier to specify by hand.

## Mandatory Diff

@before/@after instructions contain objects specified as tightly as
required to obtain ids, or as ids.

```jsx
{ '@id' : "Person/jim",
  'date_of_birth' : { '@op' : 'SwapValue',
                      '@before' : "1928-03-05",
                      '@after' : "1938-03-05"
                    }}
```

## Optional Diff

Optionals also contain @before/@after designations, but potentially
`null` fields to describe missing elements.

```jsx
{ '@id' : "Object/my_object",
  'name' : { '@op' : 'SwapValue',
             '@before' : null,
             '@after' : "Jim" }}
```

## Set Diff / Cardinality Diff

Set requires the ability to explicitly remove or add elements - we can do this by maintaining a @@before/@after with a list of those which exist *only* on the left, and *only* on the right.


## List Diff

The list diff requires swaps at a position.  We use, @copy, @swap and @keep.

### Copy

Copy the previous list from `From_Position` to `To_Position.

```jsx
{ "@op" : "CopyList",
  "@from" : From_Position,
  "@to" : To_Position,
  "@rest" : Diff }
```

### Swap

Swap out the list starting from the current point from `Previous` to `Next`

```jsx
{ "@op" : "SwapList",
  "@before" : Previous,
  "@after" : Next,
  "@rest" : Diff }
```

### Example:

```jsx
var Patch =
{ '@id' : "TaskList/my_tasks",
  'tasks' : { '@op' : "CopyList",                      % Replace List
              '@from' : 0,
              '@to' : 2,
              '@rest' : { '@op' : "SwapList",
                          '@before' : ["Task/shopping","Task/cleaning","Task/fishing"],
                          '@after' : ["Task/climbing","Task/dining","Task/travelling"],
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
                  '@from_row' : 0,
                  '@to_row' : 3,
                  '@from_column' : 0,
                  '@to_column' : 3,
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


###  Copy

```jsx
{ '@op' : "CopyTable"
  '@from_row' : From_Row,       % integer
  '@from_column' : From_Column, % integer
  '@to_row' : To_Row,           % integer
  '@to_column' : To_Column,     % integer
  '@bottom_left' : Diff_BL,     % A Table Diff
  '@top_right' : Diff_TR,       % A Table Diff
  '@bottom_right' : Diff_BR     % A Table Diff
  }
```

### Swap

Swap isntructions will give a before table as a JSON list of lists for
both the before and after. These tables need not have the same
dimensions.

```jsx
{ '@op' : "SwapTable",
  '@from_row' : From_Row,       % integer
  '@from_column' : From_Column, % integer
  '@to_row' : To_Row,           % integer
  '@to_column' : To_Column,     % integer
  '@before' : Diff_Before,
  '@after' : Diff_After,
  '@bottom_left' : Diff_BL,
  '@top_right' : Diff_TR,
  '@bottom_right' : DIff_BR
  }
```

### Keep

@keep instructions are degenerate copies.

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
                      '@from' : 0,
                      '@to' : 10,
                      '@rest' : { '@op' : 'SwapList',          % Replace List
                                  '@before' : [1,2,3],
                                  '@after' : [4,5,6],
                                  '@rest' : { '@keep' : "List" } } },
          <prop4> : { '@op' : "CopyTable"
                      '@from_row' : 0,
                      '@from_column' : 0,
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
        name: { '@op' : 'ValueSwap', '@before' : "Destiny Norris", '@after' : "Destiny Morris" },
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

*/

simple_patch(Diff,JSON_In,JSON_Out,Patch) :-
    is_dict(Diff),
    !,
    dict_keys(JSON_In,Doc_Keys),
    dict_keys(Diff,Diff_Keys),
    union(Doc_Keys,Diff_Keys,Keys),
    pairs_and_conflicts_from_keys(Keys,JSON_In,Diff,Pairs,Conflicts),
    dict_create(JSON_Out,_,Pairs),
    dict_create(Patch,_,Conflicts).

pairs_and_conflicts_from_keys([], _, _, [], []).
pairs_and_conflicts_from_keys([Key|Keys], JSON, Diff, [Result|Values], Conflicts_Out) :-
    simple_patch_key_value(Key,JSON,Diff,Result,Conflict),
    !,
    (   Conflict = _{}
    ->  Conflicts_Out = Conflicts
    ;   Conflicts_Out = [Conflict|Conflicts]
    ),
    pairs_and_conflicts_from_keys(Keys, JSON, Diff, Values, Conflicts).
pairs_and_conflicts_from_keys([Key|Keys], JSON, Diff, [Key-Final|Values],
                              [Key-Patch|Conflicts]) :-
    get_dict(Key,Diff,Key_Diff),
    get_dict('@before', Key_Diff, Expected),
    get_dict('@after', Key_Diff, Final),
    get_dict_or_null(Key, JSON, Actual),
    Patch = _{ '@op' : "SwapValue",
               '@before' : Expected,
               '@after' : Actual },
    !,
    pairs_and_conflicts_from_keys(Keys, JSON, Diff, Values, Conflicts).

simple_op_diff_value('SwapValue', _Key, Diff, Before, After) :-
    get_dict('@before', Diff, Before),
    get_dict('@after', Diff, After).

get_dict_or_null(Key,JSON,V) :-
    (   get_dict(Key,JSON,V)
    ->  true
    ;   V = null).

simple_patch_key_value(Key,JSON,Diff,Result,Conflict) :-
    % If it's in the diff, we're changing it.
    get_dict(Key,Diff,Key_Diff),
    !,
    (   get_dict('@op', Key_Diff, Operation_String)
    ->  atom_string(Op, Operation_String),
        get_dict_or_null(Key,JSON,V),
        simple_op_diff_value(Op,Key,Key_Diff,V,Value),
        Result = Key-Value,
        Conflict = _{}
    ;   get_dict_or_null(Key,JSON,V),
        simple_patch(Key_Diff,V,Value,Sub_Conflict),
        Result = Key-Value,
        (   Sub_Conflict = _{}
        ->  Conflict = _{}
        ;   Conflict = Key-Sub_Conflict)
    ).
simple_patch_key_value(Key,JSON,_Diff,Key-Value,_{}) :-
    % This is an implicit copy instruction
    get_dict(Key,JSON,Value).

:- begin_tests(simple_patch).
:- use_module(core(util/test_utils)).

test(simple_flat_patch, []) :-
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

    simple_patch(Patch,Before,After,_{}).

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
    simple_patch(Patch,Before,After,Conflict),
    After = _{ '@id' : "Person/1",
               '@type' : "Person",
               dob : "2009-08-03",
               name : "james"
             },
    Conflict = _{ name: _{ '@op' : "SwapValue",
                           '@after' : "jim",
                           '@before': "jake"}
                }.

test(simple_flat_patch_missing, []) :-

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

    simple_patch(Patch,Before,After,_{}).

test(simple_flat_drop_value, []) :-

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

    simple_patch(Patch,Before,After,_{}).


test(simple_deep_swap_drop_value, []) :-

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

    simple_patch(Patch,Before,After,_{}).

test(simple_deep_swap_wrong_value, []) :-

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

    simple_patch(Patch,Before,After,Conflict),
    After =
    _{ '@id' : "Person/1",
       '@type' : "Person",
       address:
       _{ address1 : "Mölker Bastei 8",
          address2 : null,
          city : "Vienna",
          country : "Austria"},
       dob:"1770-12-17",
       name:"Beethoven"},
    Conflict =
    _{ address:
       _{ address1:
          _{
              '@op' : "SwapValue",
              '@after' : "Probusgasse 6",
              '@before' : "Probusgasse 7"
          }
        }
     }.

:- end_tests(simple_patch).

/*


:- begin_tests(patch).

:- use_module(core(util/test_utils)).

schema1('
    {
        "@base": "terminusdb:///data/",
        "@schema": "terminusdb:///schema#",
        "@type": "@context"
    }
    {
        "@id": "Employee",
        "@type": "Class",
        "@key" : { "@type" : "Lexical",
                   "@fields" : [ "employee_id" ] },
        "employee_id": "xsd:string",
        "manager": {
            "@class": "Employee",
            "@type": "Optional"
        },
        "name": {
            "@class": "xsd:string",
            "@type": "Optional"
        },
        "team": {
            "@class": "xsd:string",
            "@type": "Optional"
        },
        "title": {
            "@class": "xsd:string",
            "@type": "Optional"
        }
    }
').

test(all_class_frames, [
         setup(
             (   setup_temp_store(State),
                 test_document_label_descriptor(Desc),
                 write_schema(schema1,Desc)
             )),
         cleanup(
             teardown_temp_store(State)
         )]) :-

    open_descriptor(Desc, DB),
    all_class_frames(DB,  Frames),

    Original =
    _{
        '@id': "Employee/001",
        '@type': "Employee",
        employee_id: "001",
        name: "Destiny Norris",
        team: "Marketing",
        title: "Marketing Manager"
    },
    run_insert_document(Desc, commit_info{ author: "Me",
                                           message: "foo" },
                        Original, Id),

    Diff =
    _{
        '@id': Id,
        name: _{ '@before' : "Destiny Norris", '@after' : "Destiny Morris" }
    },

    open_descriptor(Desc, DB),
    json_elaborate_diff(DB, Diff, Elaborated_Diff),

    create_context(DB, _{ author : "me", message : "Have you tried dogecoin?" }, Context),
    with_transaction(
        Context,
        patch_(Context,Elaborated_Diff),
        _
    ),

    Final =
    _{
        '@id': "Employee/001",
        '@type': "Employee",
        employee_id: "001",
        name: "Destiny Morris",
        team: "Marketing",
        title: "Marketing Manager"
    },

    get_document(Desc, Id, Final).

:- end_tests(patch).

*/
