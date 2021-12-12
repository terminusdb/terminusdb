:- module('document/patch',
          [patch/3]).

:- use_module(core(util)).

/*

Patch definition:

For the defintion of patch on trees we will take the following
approach:

1. Copy is implicit

All properties which are not specifically mentioned will be considered
part of an implicit copy. This will make patches more constrained.

2. Mandatory and optional properties will use @left / @right swap
instructions.

a) @left/@right instructions contain objects specified as tightly as
required to obtain ids, or as ids.

b) optionals also contain @left/@right designations, but potentially 'null's

3. Set requires the ability to explicitly remove or add elements - we can do this by maintaining a @@left/@right with a list of those which exist *only* on the left, and *only* on the right.


4. List requires swaps at a position - we will use, @copy, @swap and @keep.

5. Arrays will allow index swaping or "shrink" and "grow".

6. '@force' used to drop a value irrespective of current value.

7. Table requires swaps at two positions - we will use @copy, @swap and @keep.

Diff := {
          '@id' : ID % ID of object to change.
          <prop1> : { '@left' : Obj_Old                      % Mandatory
                      '@right' : Obj_New },
          <prop2> : { '@left' : null                         % Add optional
                      '@right' : Obj_New },
          <prop2> : { '@left' : Obj_Old                      % Drop optional
                      '@right' : null },
          <prop3> : { <prop3_1> :                            % Deep swap [*must* be subdocuments]
                        { <prop3_2> : ...
                           { <prop3_n> : { '@left' : Obj_Old,
                                           '@right' : Obj_New }
                        ... } } },
          <prop4> : { '@copy' : "List",                      % Replace List
                      '@from' : 0,
                      '@to' : 10,
                      '@rest' : { '@swap' : "List",
                                  '@left' : [1,2,3],
                                  '@right' : [4,5,6],
                                  '@then' : { '@keep' : "List" } } },
          <prop4> : { '@copy' : "Table"
                      '@from_row' : 0,
                      '@from_column' : 0,
                      '@to_row' : 3,
                      '@to_column' : 3,
                      '@bottom_left' : { '@keep' : "Table" },
                      '@top_right' :  { '@keep' : "Table" },
                      '@bottom_right' : { '@swap' : "List",
                                          '@left' : [[1,2,3]],
                                          '@right' : [[4,5,6]],
                                          '@bottom_left' : { '@keep' : "Table" },
                                          '@top_right' : { '@keep' : "Table" },
                                          '@bottom_right' : { '@keep' : "Table" } }
                    },
          <prop5> : { '@left' : { '@id' : ID },              % Replace element of a set
                      '@right' : { '@id' : ID }}
          <prop6> : { '@id' : ID,                            % Deep set replace
                       <prop6_1> : { '@left' : ...,
                                     '@right' : ... } },
          <prop6> : [{ '@id' : ID1,                          % Deep set replace 2
                        <prop6_1> : { '@left' : ...,
                                      '@right' : ... } },
                     { '@id' : ID2,                          % Deep set replace 2
                        <prop6_2> : { '@left' : ...,
                                      '@right' : ... } }],
          <prop6> : { '@force' : "Value" }                   % Ignore read state and force value

        }


Examples of Patch:

Original = _{
        '@id': "EmployeesFromCSV/001",
        '@type': "EmployeesFromCSV",
        employee_id: "001",
        name: "Destiny Norris",
        team: "Marketing",
        title: "Marketing Manager"
      },
Diff = _{
        '@id': "EmployeesFromCSV/001",
        name: _{ '@left' : "Destiny Norris", '@right' : "Destiny Morris" },
      },
Final = _{
        '@id': "EmployeesFromCSV/001",
        '@type': "EmployeesFromCSV",
        employee_id: "001",
        name: "Destiny Norris",
        team: "Marketing",
        title: "Marketing Manager"
      },
patch(Diff,Original,Final).

=> true

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
        name: _{ '@left' : "Destiny Norris", '@right' : "Destiny Morris" },
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
