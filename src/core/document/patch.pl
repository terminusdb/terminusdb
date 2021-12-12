:- module('document/patch',
          [patch/3]).

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
  'date_of_birth' : { '@before' : "1928-03-05",
                      '@after' : "1938-03-05" }}
```

## Optional Diff

Optionals also contain @before/@after designations, but potentially
`null` fields to describe missing elements.

```jsx
{ '@id' : "Object/my_object",
  'name' : { '@before' : null,
             '@after' : "Jim" }}
```

## Set Diff / Cardinality Diff

Set requires the ability to explicitly remove or add elements - we can do this by maintaining a @@before/@after with a list of those which exist *only* on the left, and *only* on the right.


## List Diff

The list diff requires swaps at a position.  We use, @copy, @swap and @keep.

### Copy

Copy the previous list from `From_Position` to `To_Position.

```jsx
{ "@copy" : "List",
  "@from" : From_Position,
  "@to" : To_Position,
  "@rest" : Diff }
```

### Swap

Swap out the list starting from the current point from `Previous` to `Next`

```jsx
{ "@swap" : "List",
  "@before" : Previous,
  "@after" : Next,
  "@rest" : Diff }
```

### Example:

```jsx
var Patch =
{ '@id' : "TaskList/my_tasks",
  'tasks' : { '@copy' : "List",                      % Replace List
              '@from' : 0,
              '@to' : 2,
              '@rest' : { '@swap' : "List",
                          '@before' : ["Task/shopping","Task/cleaning","Task/fishing"],
                          '@after' : ["Task/climbing","Task/dining","Task/travelling"],
                          '@rest' : { '@keep' : "List" } } }
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
  'name' : { '@force' : "Jake" }}
```

## Table Diff

A Table diff requires swaps at two positions and subdivision of each patch into squares: Top-Left (in which we make the patch) Top-Right, Bottom-Left and Bottom-Right, each of which will be computed with the help of an additional Diff.

We use @copy, @swap and @keep.

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
                { '@swap' : "Table",
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
                  '@after' : { '@keep' : "Table" },
                  '@bottom_left' : { '@keep' : "Table" },
                  '@top_right' : { '@keep' : "Table" },
                  '@bottom_right' : { '@keep' : "Table" }
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
{ '@copy' : "Table"
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
{ '@swap' : "Table",
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
          <prop1> : { '@before' : Obj_Old                      % Mandatory
                      '@after' : Obj_New },
          <prop2> : { '@before' : null                         % Add optional
                      '@after' : Obj_New },
          <prop2> : { '@before' : Obj_Old                      % Drop optional
                      '@after' : null },
          <prop3> : { <prop3_1> :                            % Deep swap [*must* be subdocuments]
                        { <prop3_2> : ...
                           { <prop3_n> : { '@before' : Obj_Old,
                                           '@after' : Obj_New }
                        ... } } },
          <prop4> : { '@copy' : "List",                      % Replace List
                      '@from' : 0,
                      '@to' : 10,
                      '@rest' : { '@swap' : "List",
                                  '@before' : [1,2,3],
                                  '@after' : [4,5,6],
                                  '@rest' : { '@keep' : "List" } } },
          <prop4> : { '@copy' : "Table"
                      '@from_row' : 0,
                      '@from_column' : 0,
                      '@to_row' : 3,
                      '@to_column' : 3,
                      '@bottom_left' : { '@keep' : "Table" },
                      '@top_right' :  { '@keep' : "Table" },
                      '@bottom_right' : { '@swap' : "Table",
                                          '@before' : [[1,2,3]],
                                          '@after' : [[4,5,6]],
                                          '@bottom_left' : { '@keep' : "Table" },
                                          '@top_right' : { '@keep' : "Table" },
                                          '@bottom_right' : { '@keep' : "Table" } }
                    },
          <prop5> : { '@before' : { '@id' : ID },              % Replace element of a set
                      '@after' : { '@id' : ID }}
          <prop6> : { '@id' : ID,                            % Deep set replace
                       <prop6_1> : { '@before' : ...,
                                     '@after' : ... } },
          <prop6> : [{ '@id' : ID1,                          % Deep set replace 2
                        <prop6_1> : { '@before' : ...,
                                      '@after' : ... } },
                     { '@id' : ID2,                          % Deep set replace 2
                        <prop6_2> : { '@before' : ...,
                                      '@after' : ... } }],
          <prop6> : { '@force' : "Value" }                   % Ignore read state and force value

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
        name: { '@before' : "Destiny Norris", '@after' : "Destiny Morris" },
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

patch(DB,Diff) :-
    elaborate_diff(DB,Diff,Elaborated_Diff),
    patch_(DB,Elaborated_Diff).

patch_(DB,Elaborated_Diff) :-
    database_prefixes(DB,Context),
    patch_(DB,Context,Elaborated_Diff).

elaborate_diff(DB,Diff,Elaborated_Diff) :-
    is_dict(Dict),
    !,
    get_dict('@id', Diff, ID),

    dict_keys(Diff,Keys),
    findall(
        P-V,
        (   member(Key,Keys),
            get_dict(Key,Dict,Value),
            \+ member(Key, '@id'),
            patch_property(Key,Value,Context,ID,DocLeft,P,DocRight)
        ),
        PVs),
    dict_pairs(DocRight, _, PVs).

patch_(Diff,DB,Context,Elaborated_Diff) :-
    is_dict(Dict),
    !,
    get_dict('@id', Diff, ID),

    dict_keys(Diff,Keys),
    findall(
        P-V,
        (   member(Key,Keys),
            get_dict(Key,Dict,Value),
            patch_property(Key,Value,Context,ID,DocLeft,P,DocRight)
        ),
        PVs),
    dict_pairs(DocRight, _, PVs).

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
')

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
        name: _{ '@before' : "Destiny Norris", '@after' : "Destiny Morris" },
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
