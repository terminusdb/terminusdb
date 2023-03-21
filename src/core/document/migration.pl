:- module('document/migration', [
              perform_instance_migration/4,
              perform_instance_migration_on_transaction/3
          ]).

:- use_module(instance).
:- use_module(schema).
:- use_module(json).

:- use_module(library(assoc)).
:- use_module(library(pcre)).
:- use_module(library(uri)).
:- use_module(library(crypto)).
:- use_module(library(when)).
:- use_module(library(option)).

% performance
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(library(terminus_store)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(solution_sequences)).
:- use_module(library(random)).
:- use_module(library(plunit)).

:- use_module(core(api)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(util/tables)).
:- use_module(core(api/api_document)).

/*
Operations Language:

Default_Or_Error := error
                 |  default(Default)
Op := delete_class(Name)
    | create_class(ClassDocument)
    | move_class(Old_Name,New_Name)
    | delete_class_property(Class,Property)
    | create_class_property(Class,Property,Type)  % for option or set
    | create_class_property(Class,Property,Type,Default)
    | move_class_property(Class, Old_Property, New_Property)
    | upcast_class_property(Class, Property, New_Type)
    | cast_class_property(Class, Property, New_Type, Default_Or_Error)
    | move_key(Class, KeyType, [Property1, ..., PropertyN])
    | change_parents(Class,
           [Parent1,...ParentN],
           [default_for_property(Property1,Default1),
            ...,
            default_for_property(PropertyN,DefaultN)])


Generic Upgrade Workflow

1. Generate an upgrade plan from a schema version, resulting in a before/after.
2. Tag the new schema version
3. Send data product with Before schema (check by Hash) to after schema version into
   perform_migration/3

Questions:

1. Where do we store the upgrades? How do we attach the exact before
and after state which the upgrade was designed for?
2. How do we mark a data product as a "schema" data product?

*/

/* delete_class(Name) */
delete_class(Name, Before, After) :-
    atom_string(Name_Key, Name),
    del_dict(Name_Key, Before, _, After).

/* create_class(Class_Document) */
create_class(Class_Document, Before, After) :-
    get_dict('@id', Class_Document, Name),
    atom_string(Name_Key, Name),
    put_dict(Name_Key, Before, Class_Document, After).

/* move_class(Old_Name, New_Name) */
move_class_in_complex_target_type(Before_Class, After_Class, Before_Target_Type, After_Target_Type) :-
    get_dict('@class', Before_Target_Type, Old_Class),
    move_class_in_required_target_type(Before_Class, After_Class, Old_Class, New_Class),
    put_dict(_{'@class' : New_Class}, Before_Target_Type, After_Target_Type).

move_class_in_required_target_type(Before_Class, After_Class, Before_Class, After_Class) :-
    !.
move_class_in_required_target_type(_Before_Class, _After_Class, Target_Type, Target_Type).

move_class_in_target_type('@id', Before_Class, After_Class, Before_Target_Type, After_Target_Type) :-
    !,
    (   atom_string(Before_Target_Type, Before_Class)
    ->  After_Target_Type = After_Class
    ;   After_Target_Type = Before_Target_Type
    ).
move_class_in_target_type('@inherits', Before_Class, After_Class, Before_Target_Type, After_Target_Type) :-
    !,
    (   select(Before_Class, Before_Target_Type, After_Class, After_Target_Type)
    ->  true
    ;   Before_Target_Type = After_Target_Type
    ).
move_class_in_target_type(Key, _Before_Class, _After_Class, Target_Type, Target_Type) :-
    has_at(Key),
    !.
move_class_in_target_type(_Key, Before_Class, After_Class, Before_Target_Type, After_Target_Type) :-
    (   is_dict(Before_Target_Type)
    ->  move_class_in_complex_target_type(Before_Class, After_Class, Before_Target_Type, After_Target_Type)
    ;   move_class_in_required_target_type(Before_Class, After_Class, Before_Target_Type, After_Target_Type)
    ).

move_class_in_document(Before_Class, After_Class, Before_Document, After_Document) :-
    findall(
        Property-After_Target_Type,
        (   get_dict(Property, Before_Document, Before_Target_Type),
            move_class_in_target_type(Property, Before_Class, After_Class,
                                      Before_Target_Type, After_Target_Type)),
        Pairs),
    dict_create(After_Document, json, Pairs).

move_class(Before_Class, After_Class, Before, After) :-
    findall(
        Class-After_Document,
        (   get_dict(Name,Before,Before_Document),
            (   atom_string(Name,Before_Class)
            ->  atom_string(Class,After_Class)
            ;   atom_string(Class,Name)
            ),
            move_class_in_document(Before_Class, After_Class, Before_Document, After_Document)
        ),
        Pairs),
    dict_create(After, json, Pairs).

/* delete_class_property(Class,Property) */
delete_class_property(Class, Property, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),
    get_dict(Class_Key, Before, Class_Document),
    del_dict(Property_Key, Class_Document, _, Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).

/* create_class_property(Class,Property) */
create_class_property(Class, Property, Type, _Default, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),
    get_dict(Class_Key, Before, Class_Document),
    \+ get_dict(Property_Key, Class_Document, _),
    put_dict(Property_Key, Class_Document, Type, Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).

/* move_class_property(Class, Old_Property, New_Property) */
move_class_property(Class, Old_Property, New_Property, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Old_Property_Key, Old_Property),
    atom_string(New_Property_Key, New_Property),
    get_dict(Class_Key, Before, Before_Class_Document),
    del_dict(Old_Property_Key, Before_Class_Document, Type_Definition, Class_Document0),
    put_dict(New_Property_Key, Class_Document0, Type_Definition, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After).

family_weaken("List","List").
family_weaken("Set","Set").
family_weaken("Optional", "Optional").
family_weaken("Cardinality", "Set").
family_weaken("Optional", "Set").

class_weaken(Class, Class) :-
    !.
class_weaken(ClassA, ClassB) :-
    basetype_subsumption_of(ClassA, ClassB).

type_weaken(Type1, Type2) :-
    is_dict(Type1),
    is_dict(Type2),
    !,
    get_dict('@type', Type1, Family1),
    get_dict('@type', Type2, Family2),
    (   family_weaken(Family1, Family2)
    ->  true
    ;   Family1 = "Cardinality",
        Family2 = "Cardinality"
    ->  get_dict('@min_cardinality', Type1, Min1),
        get_dict('@max_cardinality', Type1, Max1),
        get_dict('@min_cardinality', Type2, Min2),
        get_dict('@max_cardinality', Type2, Max2),
        Min2 =< Min1,
        Max2 >= Max1
    ;   Family1 = "Array",
        Family2 = "Array"
    ->  get_dict('@dimensions', Type1, Dim1),
        get_dict('@dimensions', Type2, Dim2),
        Dim1 = Dim2
    ),
    get_dict('@class', Type1, Class1Text),
    get_dict('@class', Type2, Class2Text),
    atom_string(Class1, Class1Text),
    atom_string(Class2, Class2Text),
    class_weaken(Class1, Class2).
type_weaken(Type1Text, Type2) :-
    is_dict(Type2),
    atom_string(Type1, Type1Text),
    !,
    get_dict('@type', Type2, Family),
    (   memberchk(Family, ["Set", "Optional"])
    ->  true
    ;   Family = "Cardinality"
    ->  get_dict('@min_cardinality', Type2, Min),
        get_dict('@max_cardinality', Type2, Max),
        Min =< 1,
        Max >= 1
    ),
    get_dict('@class', Type2, Class2Text),
    atom_string(Class2, Class2Text),
    class_weaken(Type1, Class2).
type_weaken(Type1Text, Type2Text) :-
    atom_string(Type, Type1Text),
    atom_string(Type, Type2Text).

/* upcast_class_property(Class, Property, New_Type) */
upcast_class_property(Class, Property, New_Type, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),

    get_dict(Class_Key, Before, Before_Class_Document),
    get_dict(Property_Key, Before_Class_Document, Type_Definition),
    type_weaken(Type_Definition, New_Type),
    put_dict(Property_Key, Before_Class_Document, New_Type, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After).

/* cast_class_property(Class, Property, New_Type, Default_Or_Error) */
cast_class_property(Class, Property, New_Type, _Default_Or_Error, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),

    get_dict(Class_Key, Before, Before_Class_Document),
    get_dict(Property_Key, Before_Class_Document, Type_Definition),
    \+ type_weaken(Type_Definition, New_Type),
    put_dict(Property_Key, Before_Class_Document, New_Type, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After).

/***********************************
 *                                 *
 *  The schema update interpreter  *
 *                                 *
 ***********************************/
interpret_schema_operation(Op, Before, After) :-
    Op =.. [OpName|Args],
    append(Args, [Before, After], Args1),
    Pred =.. [OpName|Args1],
    call(Pred),
    !.
interpret_schema_operation(Op, Before, _After) :-
    throw(error(schema_operation_failed(Op, Before), _)).

interpret_schema_operations([], Schema, Schema).
interpret_schema_operations([Op|Operations], Before_Schema, After_Schema) :-
    interpret_schema_operation(Op, Before_Schema, Middle_Schema),
    interpret_schema_operations(Operations, Middle_Schema, After_Schema).

/*************************************
 *                                   *
 *  The instance update interpreter  *
 *                                   *
 *************************************/
/*

We will use a shared instance, but a different schema

Schema1   Instance   Schema2
       \  /       \   /
     Before,      After


TODO: We need to pay attempt to the schema type, as we need to know if we are an Array, or List.

*/


extract_simple_type(Type, Simple) :-
    is_dict(Type),
    !,
    get_dict('@type', Type, Family),
    memberchk(Family, ["Set", "Optional", "Cardinality"]),
    get_dict('@class', Type, Simple_Unexpanded),
    default_prefixes(Prefixes),
    prefix_expand_schema(Simple_Unexpanded, Prefixes, Simple).
extract_simple_type(_, Type, Type). % implict \+ is_dict(Type)


% This has to do something with subdocument ids, maybe it has to re-elaborate, re-assign?
rewrite_document_ids(_Before, _After, Document, New_Class, New_Document) :-
    get_dict('@type', Document, Type_A),
    get_dict('@id', Document, Id_A),
    atom_concat(Type_A, Id_Suffix, Id_A),
    atom_concat(New_Class, Id_Suffix, Id_B),
    put_dict(_{ '@type' : New_Class,
                '@id' : Id_B }, Document, New_Document).

interpret_instance_operation(delete_class(Class), Before, After, Count) :-
    count_solutions(
        (   get_document_by_type(Before, Class, Uri),
            delete_document(After, Uri)
        ),
        Count
    ).
interpret_instance_operation(create_class(_), _Before, _After, 0).
interpret_instance_operation(move_class(Old_Class, New_Class), Before, After, Count) :-
    database_prefixes(Before, Prefixes),
    prefix_expand_schema(Old_Class, Prefixes, Old_Class_Ex),
    count_solutions(
        (   ask(Before,
                t(Uri, rdf:type, Old_Class_Ex)),
            once(
                (   get_document(Before, Uri, Document),
                    delete_document(Before, Uri),
                    rewrite_document_ids(Before, After, Document, New_Class, Final_Document),
                    insert_document(After, Final_Document, New_Uri),
                    forall(
                        ask(After,
                            (   t(X, P, Uri),
                                delete(X, P, Uri),
                                insert(X, P, New_Uri))),
                        true)
                )
            )
        ),
        Count
    ).
interpret_instance_operation(delete_class_property(Class, Property), Before, _After, Count) :-
    % Todo: Array / List
    count_solutions(
        ask(Before,
            (   t(X, rdf:type, Class),
                t(X, Property, Value),
                delete(X, Property, Value))),
        Count
    ).
interpret_instance_operation(upcast_class_property(Class, Property, New_Type), Before, _After, Count) :-
    (   extract_simple_type(New_Type, Simple_Type)
    ->  count_solutions(
            ask(Before,
                    (   t(X, rdf:type, Class),
                        t(X, Property, Old_Value),
                        delete(X, Property, Old_Value),
                        typecast(Old_Value, Simple_Type, [], Cast),
                        insert(X, Property, Cast)
                    )
               ),
            Count
        )
    ;   % Todo: Array / List
        throw(error(not_implemented, _))
    ).
interpret_instance_operation(cast_class_property(Class, Property, New_Type, Default_or_Error), Before, After, Count) :-
    (   extract_simple_type(New_Type, Simple_Type)
    ->  count_solutions(
            (   ask(Before,
                    (   t(X, rdf:type, Class),
                        t(X, Property, Value),
                        delete(X, Property, Value)
                    )),
                once(
                    (   (   typecast(Value, Simple_Type, [], Cast)
                        ->  true
                        ;   Default_or_Error = default(Default)
                        ->  Cast = Default^^Simple_Type
                        ;   Value = _^^Old_Type,
                            throw(error(bad_cast_in_schema_migration(Class,Property,Old_Type,New_Type), _))
                        ),
                        ask(After,
                            (   insert(X, Property, Cast)))
                    ))
            ),
            Count
        )
    ;   % Todo: Array / List
        throw(error(not_implemented, _))
    ).
interpret_instance_operation(change_parents(_Class,_Parents,_Property_Defaults), _Before, _After,_Count) :-
    throw(error(unimplemented, _)).
interpret_instance_operation(Op, _Before, _After, _Count) :-
    throw(error(instance_operation_failed(Op), _)).

interpret_instance_operations([], _Before, _After, Count, Count).
interpret_instance_operations([Instance_Operation|Instance_Operations], Before, After, Count_In, Count_Out) :-
    interpret_instance_operation(Instance_Operation, Before, After, Count),
    Count1 is Count + Count_In,
    interpret_instance_operations(Instance_Operations, Before, After, Count1, Count_Out).

interpret_instance_operations(Ops, Before, After, Count) :-
    interpret_instance_operations(Ops, Before, After, 0, Count).

/*
 * A convenient intermediate form using a dictionary:
 * { Class1 : Class_Description1, ... ClassN : Class_DescriptionN }
 *
 */
create_class_dictionary(Transaction, Dictionary) :-
    Config = config{
                 skip: 0,
                 count: unlimited,
                 as_list: false,
                 compress: true,
                 unfold: true,
                 minimized: true
             },
    findall(
        Class-Class_Document,
        (   api_document:api_get_documents(Transaction, schema, Config, Class_Document),
            get_dict('@id',Class_Document, Class)
        ),
        Pairs
    ),
    dict_create(Dictionary, json, Pairs).

class_dictionary_to_schema(Dictionary, Schema) :-
    dict_pairs(Dictionary, _, Pairs),
    maplist([_-Class,Class]>>true, Pairs, Schema).

calculate_schema_migration(Transaction, Ops, Schema) :-
    create_class_dictionary(Transaction, Dictionary),
    interpret_schema_operations(Ops, Dictionary, After),
    class_dictionary_to_schema(After, Schema).

/*
 * Actually perform the upgrade
 */

replace_schema(Before_Transaction, Schema, After_Transaction) :-
    api_full_replace_schema(Before_Transaction, Schema),
    get_dict(schema_objects,Before_Transaction,[Schema_RW_Obj]),
    get_dict(write,Schema_RW_Obj,Builder),
    nb_commit(Builder, Layer),
    put_dict(_{write:_, read:Layer, force_write: true}, Schema_RW_Obj, New_Schema_RW_Obj),
    put_dict(_{schema_objects: [New_Schema_RW_Obj]}, Before_Transaction, After_Transaction).

perform_instance_migration(Descriptor, Commit_Info, Operations, Result) :-
    % restart logic here
    max_transaction_retries(Max),
    between(0, Max, _),

    do_or_die(open_descriptor(Descriptor, Commit_Info, Transaction),
              something),
    perform_instance_migration_on_transaction(Transaction, Operations, Result),
    !.
perform_instance_migration(_, _, _, _) :-
    throw(error(transaction_retry_exceeded, _)).

perform_instance_migration_on_transaction(Before_Transaction, Operations, Result) :-
    calculate_schema_migration(Before_Transaction, Operations, Schema),
    replace_schema(Before_Transaction, Schema, After_Transaction),
    interpret_instance_operations(Operations, Before_Transaction, After_Transaction, Count),
    % run logic here.
    run_transactions([After_Transaction], false, _),
    length(Operations, Op_Count),
    Result = metadata{ schema_operations: Op_Count,
                       instance_operations: Count }.

:- begin_tests(migration).

:- use_module(core(util/test_utils)).

before1('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}


{ "@type" : "Class",
  "@id" : "A",
  "a" : "xsd:string" }

').

test(add_remove_classes,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Before),
             write_schema(before1,Before)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Ops = [
        delete_class("A"),
        create_class(_{ '@type' : "Class",
                        '@id' : "C",
                        c : "xsd:string" })
    ],
    open_descriptor(Before, Transaction),
    create_class_dictionary(Transaction, Dictionary),
    Dictionary = json{'A':json{'@id':'A','@type':'Class',a:'xsd:string'}},

    interpret_schema_operations(Ops, Dictionary, After),

    After = json{'C':_{'@id':"C",'@type':"Class",c:"xsd:string"}}.


before2('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}


{ "@type" : "Class",
  "@id" : "A",
  "a" : "xsd:string" }

').

test(move_and_weaken,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Before),
             write_schema(before2,Before)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Ops = [
        move_class("A", "B"),
        upcast_class_property("B", "a", _{ '@type' : "Optional", '@class' : "xsd:string"})
    ],
    open_descriptor(Before, Transaction),
    create_class_dictionary(Transaction, Dictionary),
    Dictionary = json{'A':json{'@id':'A','@type':'Class',a:'xsd:string'}},

    interpret_schema_operations(Ops, Dictionary, After),

    After = json{ 'B': json{ '@id':"B",
						     '@type':'Class',
						     a:_{ '@class':"xsd:string",
							      '@type':"Optional"
							    }
						   }
				}.

test(move_and_weaken_with_instance_data,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Descriptor,
        C1,
        (   insert_document(C1,
                            _{ '@id' : 'A/1', a : "foo" },
                            _),
            insert_document(C1,
                            _{ '@id' : 'A/2', a : "bar" },
                            _)
        )
    ),

    Ops = [
        move_class("A", "B"),
        upcast_class_property("B", "a", _{ '@type' : "Optional", '@class' : "xsd:string"})
    ],

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops),

    findall(
        DocA,
        get_document_by_type(Descriptor, "A", DocA),
        A_Docs),
    A_Docs = [],
    findall(
        DocB,
        get_document_by_type(Descriptor, "B", DocB),
        B_Docs),

    B_Docs = [ json{'@id':'B/1','@type':'B',a:"foo"},
	           json{'@id':'B/2','@type':'B',a:"bar"}
	         ].

:- end_tests(migration).
