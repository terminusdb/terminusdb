:- module('document/migration', [
              perform_instance_migration/5,
              perform_instance_migration_on_transaction/4,
              infer_weakening_migration/2,
              infer_arbitrary_migration/2
          ]).

:- use_module(instance).
:- use_module(schema).
:- use_module(json).

:- use_module(library(assoc)).
:- use_module(library(pcre)).
:- use_module(library(uri)).
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
:- use_module(library(ordsets)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(http/json)).

:- use_module(config(terminus_config)).

:- use_module(core(api)).
:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(util/tables)).
:- use_module(core(api/api_document)).

:- use_module(migration_dict).

/*
Operations Language:

Default_Or_Error := error
                 |  default(Default)
Op := delete_class(Name)
    | create_class(ClassDocument)
    | move_class(Old_Name,New_Name)
    | replace_class_metadata(Class,Metadata)
    | replace_class_documentation(Class,Documentation)
    | delete_class_property(Class,Property)
    | create_class_property(Class,Property,Type)  % for option or set
    | create_class_property(Class,Property,Type,Default)
    | move_class_property(Class, Old_Property, New_Property)
    | upcast_class_property(Class, Property, New_Type)
    | cast_class_property(Class, Property, New_Type, Default_Or_Error)
    | change_key(Class, KeyType, [Property1, ..., PropertyN])
    | change_parents(Class,
           [Parent1,...ParentN],
           [property_default(Property1,Default1),
            ...,
            property_default(PropertyN,DefaultN)])

*/

operation_string(Term,String) :-
    migration_dict_to_ast(Dict, Term),
    atom_json_dict(String, Dict, []).

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
    atom_string(After_Name, After_Class),
    die_if(
        get_dict(After_Name,Before,_),
        error(class_already_exists(After_Class), _)
    ),
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

/* replace_class_metadata(Class,Metadata) */
replace_class_metadata(Class, Metadata, Before, After) :-
    atom_string(Class_Key, Class),
    get_dict(Class_Key, Before, Class_Document),
    put_dict('@metadata', Class_Document, Metadata, Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).

/* replace_class_documentation(Class,Documentation) */
replace_class_documentation(Class, Documentation, Before, After) :-
    atom_string(Class_Key, Class),
    get_dict(Class_Key, Before, Class_Document),
    put_dict('@documentation', Class_Document, Documentation, Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).

/* delete_class_property(Class,Property) */
delete_class_property(Class, Property, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),
    get_dict(Class_Key, Before, Class_Document),
    del_dict(Property_Key, Class_Document, _, Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).


/* create_class_property(Class,Property,Type) */
create_class_property(Class, Property, Type, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),
    get_dict(Class_Key, Before, Class_Document),
    \+ get_dict(Property_Key, Class_Document, _),
    put_dict(Property_Key, Class_Document, Type, Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).

/* create_class_property(Class,Property,Type,Default) */
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
    text(Type1Text),
    !,
    atom_string(Type1, Type1Text),
    type_is_optional(Type2),
    get_dict('@class', Type2, Class2Text),
    atom_string(Class2, Class2Text),
    class_weaken(Type1, Class2).
type_weaken(Type1, Type2) :-
    is_dict(Type1),
    text(Type2),
    !,
    fail.
type_weaken(Type1Text, Type2Text) :-
    atom_string(Type, Type1Text),
    atom_string(Type, Type2Text).

type_is_optional(Type) :-
    get_dict('@type', Type, Family),
    (   memberchk(Family, ["Set", "Optional"])
    ->  true
    ;   Family = "Cardinality"
    ->  get_dict('@min_cardinality', Type, Min),
        get_dict('@max_cardinality', Type, Max),
        Min =< 1,
        Max >= 1
    ).

/* Failure is no-op, success is operation, and no clause head is an error

class_property_weakened(+,+,+,-) is semidet  + error
*/
class_property_weakened(Property, Original, Weakening, _Class, _Operation),
memberchk(Property,['@type','@key','@subdocument','@inherits','@id','@unfoldable']),
get_dict(Property,Weakening,New_Value),
get_dict(Property,Original,Old_Value),
New_Value = Old_Value =>
    fail.
class_property_weakened('@metadata', _Original, Weakening, Class, Operation) =>
    get_dict('@metadata',Weakening, New_Metadata),
    Operation = replace_class_metadata(Class,New_Metadata).
class_property_weakened('@documentation', _Original, Weakening, Class, Operation) =>
    get_dict('@documentation',Weakening, New_Docs),
    Operation = replace_class_documentation(Class,New_Docs).
class_property_weakened(Property, Original, Weakening, _Class, _Operation),
get_dict(Property,Weakening,New_Value),
get_dict(Property,Original,Old_Value),
New_Value = Old_Value =>
    fail.
class_property_weakened(Property, Original, Weakening, Class, Operation),
get_dict(Property, Original, Original_Type),
get_dict(Property, Weakening, Weakening_Type) =>
    do_or_die(type_weaken(Original_Type, Weakening_Type),
              error(weakening_failure(json{ reason: class_property_change_not_a_weakening,
                                            message: "The class property was changed to a type which was not weaker",
                                            class: Class,
                                            property: Property,
                                            original: Original,
                                            candidate: Weakening}),_)),
    Operation = upcast_class_property(Class,Property,Weakening).
class_property_weakened(Property, Value, Weakening, Class, _Operation) =>
    throw(error(weakening_failure(json{ reason: class_definition_not_a_weakening,
                                        message: "The class definition was not a weakening of the original",
                                        class: Class,
                                        property: Property,
                                        value: Value,
                                        candidate: Weakening}),_)).

class_property_optional(Property,Weakening,Class,Operation) :-
    get_dict(Property,Weakening,Type),
    do_or_die(
        type_is_optional(Type),
        error(weakening_failure(json{ reason: class_property_addition_not_optional,
                                      message: "The class property which was not added, did not have an optional type",
                                      class: Class,
                                      property: Property,
                                      candidate: Weakening}),_)),
    Operation = add_class_property(Class,Property,Type).

class_weakened(Class, Definition, Weakening, Operations) :-
    dict_keys(Weakening, New),
    dict_keys(Definition, Old),
    ord_subtract(Old,New,Dropped),
    % deleted
    do_or_die(
        Dropped = [],
        error(weakening_failure(json{ reason: class_property_deletion_not_a_weakening,
                                      message: "Deletion of a class property is never a weakening",
                                      dropped: Dropped,
                                      definition: Definition,
                                      candidate: Weakening}),_)
    ),
    % added
    ord_subtract(New,Old,Added),
    findall(Operation0,
            (   member(Property, Added),
                class_property_optional(Property,Weakening,Class,Operation0)
            ),
            Operations0),
    % shared
    ord_intersection(New,Old,Shared),
    findall(Operation1,
            (   member(Property,Shared),
                class_property_weakened(Property,Definition,Weakening,Class,Operation1)
            ),
            Operations1),
    append(Operations0,Operations1,Operations).

schema_weakening(Schema,Weakened,Operations) :-
    dict_keys(Schema,Old),
    dict_keys(Weakened,New),
    ord_subtract(Old,New,Deleted),
    do_or_die(
        Deleted = [],
        error(weakening_failure(json{ reason: not_a_weakening_class_definitions_deleted,
                                      message: "The specified class(es) were deleted, violating the weakening conditions",
                                      deleted: Deleted}),_)),
    ord_subtract(New,Old,Added),
    maplist({Weakened}/[Class,Operation]>>(
                get_dict(Class, Weakened, Definition),
                Operation = create_class(Definition)),
            Added,
            Operations0),
    ord_intersection(Old,New,Shared),
    findall(Intermediate_Operations,
            (   member(Key, Shared),
                get_dict(Key,Schema,Old_Class),
                get_dict(Key,Weakened,New_Class),
                class_weakened(Key,Old_Class,New_Class,Intermediate_Operations)
            ),
            Operations_List),
    append(Operations_List, Operations1),
    append(Operations0,Operations1,Operations).

schema_strengthening(Schema,Weakened,Operations) :-
    dict_keys(Schema,Old),
    dict_keys(Weakened,New),
    ord_subtract(Old,New,Deleted),
    maplist([Deleted,delete_class(Deleted)]>>true, Deleted, Operations0),
    ord_subtract(New,Old,Added),
    maplist({Weakened}/[Class,Operation]>>(
                get_dict(Class, Weakened, Definition),
                Operation = create_class(Definition)),
            Added,
            Operations1),
    ord_intersection(Old,New,Shared),
    % We can do more here! Dropped properties should be accepted
    findall(Intermediate_Operations,
            (   member(Key, Shared),
                get_dict(Key,Schema,Old_Class),
                get_dict(Key,Weakened,New_Class),
                class_weakened(Key,Old_Class,New_Class,Intermediate_Operations)
            ),
            Operations_List),
    append(Operations_List, Operations2),
    append([Operations0,Operations1,Operations2],Operations).

schema_inference_rule(weakening, Before, After, Operations) :-
    catch(
        schema_weakening(Before, After, Operations),
        error(weakening_failure(_),_),
        fail
    ).
schema_inference_rule(strengthening, Before, After, Operations) :-
    schema_strengthening(Before, After, Operations).

perform_migration_rule(weakening,_Before, After, Operations_List, Validation_Out) :-
    transaction_objects_to_validation_objects([After], [Validation]),
    (   Operations_List = []
    ->  Validation = Validation_Out
    ;   get_dict(schema_objects, Validation, [Schema_Object]),
        put_dict(_{changed:true}, Schema_Object, Schema_Object_Changed),
        put_dict(_{schema_objects:[Schema_Object_Changed]}, Validation, Validation_Out)
    ).
perform_migration_rule(strengthening, Before, After, Operations_List, Validation_Out) :-
    interpret_instance_operations(Operations_List, Before, After, Count),
    transaction_objects_to_validation_objects([After], [Validation]),
    (   Operations_List = []
    ->  Validation = Validation0
    ;   get_dict(schema_objects, Validation, [Schema_Object]),
        put_dict(_{changed:true}, Schema_Object, Schema_Object_Changed),
        put_dict(_{schema_objects:[Schema_Object_Changed]}, Validation, Validation0)
    ),
    (   Count > 0
    ->  Validation0 = Validation_Out
    ;   get_dict(schema_objects, Validation0, [Schema_Object]),
        put_dict(_{changed:true}, Schema_Object, Schema_Object_Changed),
        put_dict(_{schema_objects:[Schema_Object_Changed]}, Validation0, Validation_Out)
    ).

infer_migration(Rule, [Validation], [New_Validation]) :-
    validation_objects_to_transaction_objects([Validation],[After_Transaction]),
    Descriptor = (Validation.descriptor),
    open_descriptor(Descriptor, Before_Transaction),
    create_class_dictionary(Before_Transaction, Before),
    create_class_dictionary(After_Transaction, After),
    schema_inference_rule(Rule, Before, After, Operations),
    migration_list_to_ast_list(Operations_List,Operations),
    !,
    perform_migration_rule(Rule, Before_Transaction, After_Transaction, Operations_List, Validation0),
    atom_json_dict(Migration, Operations_List, [default_tag(json), width(0)]),
    (   get_dict(commit_info, Validation, Commit_Info)
    ->  true
    ;   Commit_Info = commit_info{}),
    put_dict(_{ migration: Migration }, Commit_Info, Commit_Info0),
    put_dict(_{commit_info: Commit_Info0}, Validation0, New_Validation).

infer_weakening_migration(Validations,New_Validations) :-
    infer_migration(weakening, Validations, New_Validations).

infer_arbitrary_migration(Validations,New_Validations) :-
    infer_migration(strengthening, Validations, New_Validations).

/* upcast_class_property(Class, Property, New_Type) */
upcast_class_property(Class, Property, New_Type, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),

    get_dict(Class_Key, Before, Before_Class_Document),
    get_dict(Property_Key, Before_Class_Document, Type_Definition),
    do_or_die(
        type_weaken(Type_Definition, New_Type),
        error(not_an_irrefutable_weakening_operation(
                  upcast_class_property,
                  Class,
                  Property,
                  New_Type),
              _)
    ),
    put_dict(Property_Key, Before_Class_Document, New_Type, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After).

/* cast_class_property(Class, Property, New_Type, Default_Or_Error) */
cast_class_property(Class, Property, New_Type, _Default_Or_Error, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),

    get_dict(Class_Key, Before, Before_Class_Document),
    get_dict(Property_Key, Before_Class_Document, _Type_Definition),
    put_dict(Property_Key, Before_Class_Document, New_Type, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After).

/***********************************
 *                                 *
 *  The schema update interpreter  *
 *                                 *
 ***********************************/
interpret_schema_operation_(Op, Before, After) :-
    Op =.. [OpName|Args],
    append(Args, [Before, After], Args1),
    Pred =.. [OpName|Args1],
    catch(
        call(Pred),
        error(existence_error(procedure,_), _),
        (   operation_string(Op, Op_String),
            throw(error(schema_operation_failed(Op_String, Before), _))
        )
    ),
    !.
interpret_schema_operation_(Op, Before, _After) :-
    operation_string(Op, Op_String),
    throw(error(schema_operation_failed(Op_String, Before), _)).

interpret_schema_operations([], Schema, Schema).
interpret_schema_operations([Op|Operations], Before_Schema, After_Schema) :-
    interpret_schema_operation_(Op, Before_Schema, Middle_Schema),
    interpret_schema_operations(Operations, Middle_Schema, After_Schema).

interpret_schema_operation(Operation, Before, After) :-
    calculate_schema_migration(Before, [Operation], Schema),
    replace_schema(Before, Schema, After).

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
extract_simple_type(Type, Type). % implict \+ is_dict(Type)


rewrite_document_key_value('@id',Id_A,Type_A,Type_B,Key,Id_B) =>
    atom_concat(Type_A, Id_Suffix, Id_A),
    atom_concat(Type_B, Id_Suffix, Id_B),
    Key = '@id'.
rewrite_document_key_value('@type',Type,Type_A,Type_B,Key,Value),
atom_string(Type,Type_A) =>
    atom_string(Value,Type_B),
    Key = '@type'.
rewrite_document_key_value(P,V,Type_A,Type_B,Key,Value) =>
    P = Key,
    rewrite_document_ids(V,Type_A,Type_B,Value).

rewrite_document_ids(Document, Class_A, Class_B, New_Document),
is_dict(Document) =>
    findall(
        Key-Value,
        (   get_dict(K, Document, V),
            rewrite_document_key_value(K,V,Class_A,Class_B,Key,Value)
        ),
        Pairs
    ),
    dict_create(New_Document, json, Pairs).
rewrite_document_ids(Document, Class_A, Class_B, New_Document),
is_list(Document) =>
    maplist({Class_A,Class_B}/[D0,D1]>>rewrite_document_ids(D0,Class_A,Class_B,D1),
            Document, New_Document).
rewrite_document_ids(Value, _Class_A, _Class_B, New_Value) =>
    Value = New_Value.


delete_value(base_class(_),_,_) => true.
delete_value(unit,_,_) => true.
delete_value(enum(_,_),_,_) => true.
delete_value(_, Before, Value) =>
    database_prefixes(Before, Prefixes),
    delete_subdocument(Before, Prefixes, Value).

interpret_instance_operation_(delete_class(Class), Before, After, Count) :-
    count_solutions(
        (   get_document_uri_by_type(Before, Class, Uri),
            delete_document(After, Uri)
        ),
        Count
    ).
interpret_instance_operation_(create_class(_), _Before, _After, 0).
interpret_instance_operation_(replace_class_metadata(_,_), _Before, _After, 0).
interpret_instance_operation_(replace_class_documentation(_,_), _Before, _After, 0).
interpret_instance_operation_(move_class(Old_Class, New_Class), Before, After, Count) :-
    database_prefixes(Before, Prefixes),
    prefix_expand_schema(Old_Class, Prefixes, Old_Class_Ex),
    count_solutions(
        (   ask(Before,
                t(Uri, rdf:type, Old_Class_Ex)),
            once(
                (   get_document(Before, Uri, Document),
                    delete_document(Before, Uri),
                    rewrite_document_ids(Document, Old_Class, New_Class, Final_Document),
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
interpret_instance_operation_(delete_class_property(Class, Property), Before, _After, Count) :-
    % Todo: Tagged Union
    database_prefixes(Before, Prefixes),
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    prefix_expand_schema(Property, Prefixes, P),
    once(class_predicate_type(Before,Class_Ex, P, Type)),
    count_solutions(
        (   ask(Before,
                (   isa(X, Class_Ex),
                    t(X, P, Value),
                    delete(X, P, Value))),
            delete_value(Type,Before,Value)
        ),
        Count
    ).
interpret_instance_operation_(create_class_property(Class, Property, Type, Default), _Before, After, Count) :-
    (   is_dict(Type),
        get_dict('@type', Type, "List")
    ->  database_prefixes(After, Prefixes),
        get_dict('@class', Type, List_Class),
        prefix_expand_schema(List_Class, Prefixes, List_Class_Ex),
        prefix_expand_schema(Property, Prefixes, P),
        count_solutions(
            (   ask(After,
                    isa(X, Class)),
                forall(
                    'document/json':list_id_key_context_triple(
                        Default,
                        object{'@type' : List_Class_Ex},
                        X,
                        P,
                        Prefixes,
                        t(ListS,ListP,ListO)),
                    ask(After,
                        insert(ListS, ListP, ListO)))),
            Count
        )
    ;   is_dict(Type),
        get_dict('@type', Type, "Array")
    ->  get_dict('@dimensions', Type, Dimensions),
        get_dict('@class', Type, Array_Class),
        prefix_expand_schema(Array_Class, Prefixes, Array_Class_Ex),
        database_prefixes(After, Prefixes),
        prefix_expand_schema(Property, Prefixes, P),
        count_solutions(
            (   ask(After,
                    isa(X, Class)),
                forall(
                    'document/json':array_id_key_context_triple(
                        Default,
                        object{'@type' : Array_Class_Ex},
                        Dimensions,
                        X,
                        P,
                        Prefixes,
                        t(ListS,ListP,ListO)),
                    ask(After,
                        insert(ListS, ListP, ListO)))),
            Count
        )
    ;   is_dict(Type),
        get_dict('@type', Type, Family),
        memberchk(Family, ["Set", "Optional", "Cardinality"])
    ->  get_dict('@class', Type, Value_Type),
        default_prefixes(Prefixes),
        prefix_expand_schema(Value_Type, Prefixes, Value_Type_Atom),
        (   Family = "Optional"
        ->  Default_List = [Default]
        ;   Default_List = Default),
        count_solutions(
            (   ask(After,
                    isa(X, Class)),
                forall(
                    member(V, Default_List),
                    ask(After,
                        insert(X, Property, V^^Value_Type_Atom))
                )
            ),
            Count
        )
    ;   text(Type)
    ->  default_prefixes(Prefixes),
        prefix_expand_schema(Type, Prefixes, Value_Type_Atom),
        count_solutions(
            ask(After,
                (   isa(X, Class),
                    insert(X, Property, Default^^Value_Type_Atom))),
            Count)
    ;   throw(
            error(
                not_irrefutable_property_creation(Class,Property,Type)))
    ).
interpret_instance_operation_(create_class_property(Class, Property, Type), _Before, After, Count) :-
    (   is_dict(Type),
        get_dict('@type', Type, "List")
    ->  count_solutions(
            ask(After,
                (   isa(X, Class),
                    insert(X, Property, rdf:nil))),
            Count
        )
    ;   is_dict(Type),
        type_is_optional(Type)
    ->  Count = 0
    ;   throw(
            error(
                unknown_irrefutable_property_creation(Class,Property,Type)))
    ).
interpret_instance_operation_(upcast_class_property(Class, Property, New_Type), _Before, After, Count) :-
    (   extract_simple_type(New_Type, Simple_Type)
    ->  count_solutions(
            (   default_prefixes(Prefixes),
                prefix_expand_schema(Simple_Type, Prefixes, Simple_Type_Ex),
                ask(After,
                    (   isa(X, Class),
                        t(X, Property, Old_Value),
                        delete(X, Property, Old_Value),
                        typecast(Old_Value, Simple_Type_Ex, [], Cast),
                        insert(X, Property, Cast)
                    )
                   )
            ),
            Count
        )
    ;   % Todo: Array / List
        operation_string(upcast_class_property(Class,Property,New_Type), Op_String),
        throw(error(not_implemented(Op_String), _))
    ).
interpret_instance_operation_(cast_class_property(Class, Property, New_Type, Default_or_Error), Before, After, Count) :-
    (   extract_simple_type(New_Type, Simple_Type)
    ->  count_solutions(
            (   default_prefixes(Prefixes),
                prefix_expand_schema(Simple_Type, Prefixes, Simple_Type_Ex),
                ask(Before,
                    (   t(X, rdf:type, '@schema':Class),
                        t(X, Property, Value),
                        delete(X, Property, Value)
                    )),
                once(
                    (   Value = V^^Old_Type,
                        global_prefix_expand(Old_Type, Old_Type_Ex),
                        (   typecast(V^^Old_Type_Ex, Simple_Type_Ex, [], Cast)
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
        operation_string(cast_class_property(Class, Property, New_Type, Default_or_Error), Op_String),
        throw(error(not_implemented(Op_String), _))
    ).
interpret_instance_operation_(change_parents(Class,Parents,Property_Defaults), _Before, _After,_Count) :-
    operation_string(change_parents(Class,Parents,Property_Defaults), Op_String),
    throw(error(not_implemented(Op_String), _)).

interpret_instance_operation(Op, Before, After, Count) :-
    interpret_instance_operation_(Op, Before, After, Count),
    !.
interpret_instance_operation(Op, _Before, _After, _Count) :-
    operation_string(Op, Op_String),
    throw(error(instance_operation_failed(Op_String), _)).

object_type(instance,instance_objects).
object_type(schema,schema_objects).

cycle_layer(Type, Before_Transaction, After_Transaction) :-
    object_type(Type,OT),
    get_dict(OT, Before_Transaction, [RWO]),
    get_dict(write, RWO, Builder),
    (   var(Builder)
    ->  Before_Transaction = After_Transaction
    ;   nb_commit(Builder, Layer),
        put_dict(_{read: Layer, write: _}, RWO, New_RWO),
        put_dict(OT, Before_Transaction, [New_RWO], After_Transaction)
    ),
    ensure_transaction_has_builder(Type, After_Transaction).

interpret_instance_operations([], _Before, _After, Instance_Count, Instance_Count).
interpret_instance_operations([Operation|Operations], Before, After, Instance_Count_In, Instance_Count_Out) :-
    interpret_instance_operation(Operation, Before, After, Instance_Count),
    Instance_Count1 is Instance_Count + Instance_Count_In,

    interpret_instance_operations(Operations, Before, After, Instance_Count1, Instance_Count_Out).

interpret_instance_operations(JSONOps, Before, After, Instance_Count) :-
    migration_list_to_ast_list(JSONOps, Ops),
    interpret_instance_operations(Ops, Before, After, 0, Instance_Count).

interpret_operations([], After_Transaction, After_Transaction, Instance_Count, Instance_Count).
interpret_operations([Operation|Operations], Before, After, Instance_Count_In, Instance_Count_Out) :-
    interpret_schema_operation(Operation, Before, Intermediate0),
    cycle_layer(schema,Intermediate0,Intermediate1),

    interpret_instance_operation(Operation, Before, Intermediate1, Instance_Count),
    Instance_Count1 is Instance_Count + Instance_Count_In,
    cycle_layer(instance,Intermediate1, Intermediate2),

    interpret_operations(Operations, Intermediate2, After, Instance_Count1, Instance_Count_Out).

interpret_operations(JSONOps, Before, After, Instance_Count) :-
    migration_list_to_ast_list(JSONOps, Ops),
    % Preflight to test that everything is ok...
    calculate_schema_migration(Before, Ops, _),
    interpret_operations(Ops, Before, Intermediate, 0, Instance_Count),
    squash_layer(Before, Intermediate, After).

squash_layer(Before,Intermediate,After) :-
    squash_layer(instance, Before, Intermediate, Intermediate0),
    squash_layer(schema, Before, Intermediate0, After).

squash_layer(Type,Before,Intermediate,After) :-
    % squash the result
    object_type(Type,OT),
    get_dict(OT, Before, [Before_RWO]),
    get_dict(read, Before_RWO, Before_Layer),

    get_dict(OT, Intermediate, [Intermediate_RWO]),
    get_dict(read, Intermediate_RWO, Intermediate_Layer),

    (   ground(Intermediate_Layer)
    ->  (   ground(Before_Layer)
        ->  squash_upto(Intermediate_Layer, Before_Layer, After_Layer)
        ;   After_Layer = Intermediate_Layer % first write to DB
        ),
        (   layer_addition_count(After_Layer, Additions),
            layer_removal_count(After_Layer, Removals),
            (   Additions \= 0
            ;   Removals \= 0)
        ->  Force_Write = true
        ;   Force_Write = false
        )
    ;   Force_Write = false % no data ever written to db
    ),

    put_dict(_{read: After_Layer, write: _, force_write: Force_Write}, Intermediate_RWO, After_RWO),
    put_dict(OT, Intermediate, [After_RWO], After).

/*
 * A convenient intermediate form using a dictionary:
 * { Class1 : Class_Description1, ... ClassN : Class_DescriptionN }
 *
 */
create_class_dictionary(Transaction, Dictionary) :-
    Schema_Objects = (Transaction.schema_objects),
    forall(
        member(Schema_Object,Schema_Objects),
        (   get_dict(read, Schema_Object, Read),
            var(Read))
    ),
    !,
    Dictionary = json{}.
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

perform_instance_migration(Descriptor, Commit_Info, Operations, Result, Options) :-
    (   option(dry_run(true), Options)
    ->  do_or_die(
            open_descriptor(Descriptor, Commit_Info, Transaction),
            error(unresolvable_absolute_descriptor(Descriptor),_)
        ),
        perform_instance_migration_on_transaction(Transaction, Operations, Result, Options)
    ;   perform_instance_migration_retry(Descriptor, Commit_Info, Operations, Result, Options)
    ).

perform_instance_migration_retry(Descriptor, Commit_Info, Operations, Result, Options) :-
    % restart logic here
    max_transaction_retries(Max),
    between(0, Max, _),
    do_or_die(
        open_descriptor(Descriptor, Commit_Info, Transaction),
        error(unresolvable_absolute_descriptor(Descriptor),_)
    ),
    perform_instance_migration_on_transaction(Transaction, Operations, Result, Options),
    !.
perform_instance_migration_retry(_, _, _, _, _) :-
    throw(error(transaction_retry_exceeded, _)).

perform_instance_migration_on_transaction(Before_Transaction, Operations, Result, Options) :-
    perform_instance_migration_on_transaction(Before_Transaction, Operations, _, Result, Options).

perform_instance_migration_on_transaction(Before_Transaction, Operations, After_Transaction, Result, Options) :-
    ensure_transaction_has_builder(instance, Before_Transaction),
    ensure_transaction_has_builder(schema, Before_Transaction),
    interpret_operations(Operations,Before_Transaction,After_Transaction,Count),
    length(Operations, Op_Count),
    % run logic here.
    (   option(dry_run(true), Options)
    ->  true
    ;   run_transactions([After_Transaction], false, _)
    ),

    (   option(verbose(true), Options)
    ->  create_class_dictionary(After_Transaction, Dictionary),
        class_dictionary_to_schema(Dictionary, Schema),
        Result = metadata{ schema_operations: Op_Count,
                           schema: Schema,
                           instance_operations: Count }
    ;   Result = metadata{ schema_operations: Op_Count,
                           instance_operations: Count }
    ).

:- begin_tests(migration).

:- use_module(core(util/test_utils)).

test(property_weakening, []) :-
    \+ class_property_weakened(
           d,
           json{'@id':'D', '@type':'Class', d:json{'@class':'xsd:string', '@type':'List'}},
           json{'@id':'D', '@type':'Class', d:json{'@class':'xsd:string', '@type':'List'}},
           'D', _Weakened).

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

{ "@type" : "Class",
  "@id" : "Super",
  "sub" : "Sub" }

{ "@type" : "Class",
  "@id" : "Sub",
  "@subdocument" : [],
  "@key" : { "@type" : "Lexical", "@fields" : ["value"] },
  "value" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "C",
  "c" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "D",
  "d" : { "@type" : "List",
          "@class" : "xsd:string" }}

{ "@type" : "Class",
  "@id" : "E",
  "e" : { "@type" : "List",
          "@class" : "Sub" }}

{ "@type" : "Class",
  "@id" : "F",
  "f" : { "@type" : "Optional", "@class" : "xsd:float" }}

').

test(move_and_weaken,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(Before),
             write_schema(before2,Before)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Term_Ops = [
        move_class("A", "B"),
        upcast_class_property("B", "a", _{ '@type' : "Optional", '@class' : "xsd:string"})
    ],

    open_descriptor(Before, Transaction),
    create_class_dictionary(Transaction, Dictionary),

    json{'A':json{'@id':'A','@type':'Class',a:'xsd:string'}} :< Dictionary,

    interpret_schema_operations(Term_Ops, Dictionary, After),

    json{ 'B': json{ '@id':"B",
					 '@type':'Class',
					 a:_{ '@class':"xsd:string",
						  '@type':"Optional"
						}
				   }
		} :< After.

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

    Term_Ops = [
        move_class("A", "B"),
        upcast_class_property("B", "a", _{ '@type' : "Optional", '@class' : "xsd:string"})
    ],
    migration_list_to_ast_list(Ops,Term_Ops),
    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),
    Result = metadata{instance_operations:4,schema_operations:2},
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


test(delete_class_property,
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

    Term_Ops = [
        delete_class_property("A", "a")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),
    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),

    Result = metadata{instance_operations:2,schema_operations:1},
    findall(
        DocA,
        get_document_by_type(Descriptor, "A", DocA),
        A_Docs),

    A_Docs = [ json{ '@id':'A/1',
					 '@type':'A'
				   },
			   json{ '@id':'A/2',
					 '@type':'A'
				   }
			 ].

test(rewrite_subdocument_ids, []) :-
    rewrite_document_ids(
        json{'@id':'Super/1', '@type':'Super',
             sub:json{'@id':'Super/1/sub/Sub/asdf',
                      '@type':'Sub', value:"asdf"}},
        "Super",
        "Duper",
        Result),
    Result =
    json{'@id':'Duper/1', '@type':'Duper',
         sub:json{'@id':'Duper/1/sub/Sub/asdf',
                  '@type':'Sub', value:"asdf"}}.

test(subdocument_move_class,
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
                            _{ '@id' : 'Super/1', sub : _{ '@id' : "Super/1/sub/Sub/asdf",
                                                           value : "asdf" }},
                            _),
            insert_document(C1,
                            _{ '@id' : 'Super/2', sub : _{ '@id' : "Super/2/sub/Sub/fdsa",
                                                           value: "fdsa" }},
                            _)
        )
    ),

    Term_Ops = [
        move_class("Super", "Duper")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),
    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),

    Result = metadata{instance_operations:2,schema_operations:1},
    findall(
        DocDuper,
        get_document_by_type(Descriptor, "Duper", DocDuper),
        Duper_Docs),
    Duper_Docs = [
        json{ '@id':'Duper/1',
			  '@type':'Duper',
			  sub:json{ '@id':'Duper/1/sub/Sub/asdf',
						'@type':'Sub',
						value:"asdf"
					  }
			},
		json{ '@id':'Duper/2',
			  '@type':'Duper',
			  sub:json{ '@id':'Duper/2/sub/Sub/fdsa',
						'@type':'Sub',
						value:"fdsa"
					  }
			}
	].

test(move_to_existing_fails,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(class_already_exists("C"),_)
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

    Term_Ops = [
        move_class("A", "C")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),
    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),

    Result = metadata{instance_operations:2,schema_operations:1},
    findall(
        DocDuper,
        get_document_by_type(Descriptor, "Duper", DocDuper),
        Duper_Docs),
    Duper_Docs = [
        json{ '@id':'Duper/1',
			  '@type':'Duper',
			  sub:json{ '@id':'Duper/1/sub/Sub/asdf',
						'@type':'Sub',
						value:"asdf"
					  }
			},
		json{ '@id':'Duper/2',
			  '@type':'Duper',
			  sub:json{ '@id':'Duper/2/sub/Sub/fdsa',
						'@type':'Sub',
						value:"fdsa"
					  }
			}
	].


test(delete_list_property,
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
                            _{ '@id' : 'D/1', d : ["foo","bar"] },
                            _),
            insert_document(C1,
                            _{ '@id' : 'D/2', d : ["baz","qux"] },
                            _)
        )
    ),

    Term_Ops = [
        delete_class_property("D", "d")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),
    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),

    Result = metadata{instance_operations:2,schema_operations:1},
    findall(
        DocD,
        get_document_by_type(Descriptor, "D", DocD),
        D_Docs),

    D_Docs = [ json{ '@id':'D/1',
					 '@type':'D'
				   },
			   json{ '@id':'D/2',
					 '@type':'D'
				   }
			 ].

test(delete_list_of_subdocument_property,
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
                            _{ '@id' : 'E/1',
                               e :
                               [
                                   json{ '@type':'Sub',
						                 value:"asdf"
					                   },
                                   json{ '@type':'Sub',
						                 value:"fdsa"
					                   }
                               ] },
                            _),
            insert_document(C1,
                            _{ '@id' : 'E/2',
                               e :
                               [
                                   json{ '@type':'Sub',
						                 value:"boo"
					                   },
                                   json{ '@type':'Sub',
						                 value:"oob"
					                   }
                               ] },
                            _)
        )
    ),

    Term_Ops = [
        delete_class_property("E", "e")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),

    Result = metadata{instance_operations:2,schema_operations:1},
    findall(
        DocE,
        get_document_by_type(Descriptor, "E", DocE),
        E_Docs),

    E_Docs = [ json{ '@id':'E/1',
					 '@type':'E'
				   },
			   json{ '@id':'E/2',
					 '@type':'E'
				   }
			 ].


test(delete_and_create_class_property,
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

    Term_Ops = [
        delete_class_property("A", "a"),
        create_class_property("A", "a",
                              _{'@type' : "Optional",
                                '@class': "xsd:integer"})
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),
    Result = metadata{instance_operations:2,schema_operations:2},
    findall(
        DocA,
        get_document_by_type(Descriptor, "A", DocA),
        A_Docs),

    A_Docs = [ json{ '@id':'A/1',
					 '@type':'A'
				   },
			   json{ '@id':'A/2',
					 '@type':'A'
				   }
			 ].

test(float_to_string,
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
                            _{ '@id' : 'F/1', f : 33.4 },
                            _),
            insert_document(C1,
                            _{ '@id' : 'F/2', f : 44.3 },
                            _)
        )
    ),

    Term_Ops = [
        cast_class_property("F", "f",_{ '@type' : "Optional",
                                        '@class' : "xsd:string"},error)
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),

    Result = metadata{instance_operations:2,schema_operations:1},
    findall(
        DocF,
        get_document_by_type(Descriptor, "F", DocF),
        F_Docs),

    F_Docs = [ json{'@id':'F/1','@type':'F',f:"33.400001525878906"},
			   json{'@id':'F/2','@type':'F',f:"44.29999923706055"}
			 ].

test(cast_to_required_fails,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(not_an_irrefutable_weakening_operation(
                upcast_class_property,"F","f","xsd:string"
            ),_)
     ]) :-


    with_test_transaction(
        Descriptor,
        C1,
        (   insert_document(C1,
                            _{ '@id' : 'F/1', f : 33.4 },
                            _),
            insert_document(C1,
                            _{ '@id' : 'F/2', f : 44.3 },
                            _)
        )
    ),

    Term_Ops = [
        upcast_class_property("F", "f","xsd:string")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               _Result,
                               []).

test(cast_to_required_or_error,
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
                            _{ '@id' : 'F/1', f : 33.4 },
                            _),
            insert_document(C1,
                            _{ '@id' : 'F/2', f : 44.3 },
                            _)
        )
    ),

    Term_Ops = [
        cast_class_property("F", "f", "xsd:string", error)
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),
    Result = metadata{ instance_operations:2,
					   schema_operations:1
					 },
    findall(
        DocF,
        get_document_by_type(Descriptor, "F", DocF),
        F_Docs),

    F_Docs = [ json{'@id':'F/1','@type':'F',f:"33.400001525878906"},
			   json{'@id':'F/2','@type':'F',f:"44.29999923706055"}
			 ].


test(garbage_op,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(unknown_schema_migration_operation(foo),_)
     ]) :-

    Ops = [
        foo
    ],

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               _Result,
                               []).

test(replace_metadata,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Term_Ops = [
        replace_class_metadata("F", _{ asdf : "fdsa" })
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),

    get_schema_document(Descriptor, "F", F),
    F = json{ '@id':'F',
              '@metadata':json{asdf:"fdsa"},
              '@type':'Class',
              f:json{'@class':'xsd:float','@type':'Optional'}
            },
    Result = metadata{instance_operations:0,schema_operations:1}.

test(weakening_inference,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before1,Descriptor)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-
    create_class_dictionary(Descriptor, Before),
    After = json{'A':json{'@id':'A','@type':'Class', a:json{ '@type' : "Optional",
                                                             '@class' : 'xsd:string'}},
                 'B':json{'@id': 'B', '@type':'Class', b: 'xsd:integer'}},
    schema_weakening(Before, After, Operations),

    Operations = [ create_class(json{ '@id':'B',
							          '@type':'Class',
							          b:'xsd:integer'
							        }),
				   upcast_class_property('A',
								         a,
								         json{ '@id':'A',
								               '@type':'Class',
								               a:json{ '@class':'xsd:string',
									                   '@type':"Optional"
									                 }
								             })
				 ].

test(weakening_inference_class_missing,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before1,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(weakening_failure(json{deleted:['A'],
                                   message:"The specified class(es) were deleted, violating the weakening conditions",
                                   reason:not_a_weakening_class_definitions_deleted}),_)
     ]) :-

    create_class_dictionary(Descriptor, Before),
    After = json{},
    schema_weakening(Before, After, _Operations).


test(weakening_inference_property_missing,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before1,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(weakening_failure(json{definition:json{'@id':'A','@type':'Class',a:'xsd:string'},
                                   dropped:[a],
                                   message:"Deletion of a class property is never a weakening",
                                   reason:class_property_deletion_not_a_weakening,
                                   candidate:json{'@id':'A','@type':'Class'}}),_)
     ]) :-

    create_class_dictionary(Descriptor, Before),
    After = json{'A':json{'@id':'A','@type':'Class'}},
    schema_weakening(Before, After, _Operations).

test(dry_run,
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
                            _{ '@id' : 'F/1', f : 33.4 },
                            _),
            insert_document(C1,
                            _{ '@id' : 'F/2', f : 44.3 },
                            _)
        )
    ),

    Term_Ops = [
        cast_class_property("F", "f", "xsd:string", error)
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               [dry_run(true), verbose(true)]),

    Result = metadata{ instance_operations:2,
					   schema_operations:1,
                       schema: Schema
					 },

    memberchk(json{'@id':'F','@type':'Class',f:'xsd:string'}, Schema),

    findall(
        DocF,
        get_document_by_type(Descriptor, "F", DocF),
        F_Docs),

    F_Docs = [ json{'@id':'F/1','@type':'F',f:33.400001525878906},
			   json{'@id':'F/2','@type':'F',f:44.29999923706055}
			 ].

test(verbose,
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
                            _{ '@id' : 'F/1', f : 33.4 },
                            _),
            insert_document(C1,
                            _{ '@id' : 'F/2', f : 44.3 },
                            _)
        )
    ),

    Term_Ops = [
        cast_class_property("F", "f", "xsd:string", error)
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               [verbose(true)]),

    Result = metadata{ instance_operations:2,
					   schema_operations:1,
                       schema: Schema
					 },

    memberchk(json{'@id':'F','@type':'Class',f:'xsd:string'}, Schema),

    findall(
        DocF,
        get_document_by_type(Descriptor, "F", DocF),
        F_Docs),

    F_Docs = [ json{'@id':'F/1','@type':'F',f:"33.400001525878906"},
			   json{'@id':'F/2','@type':'F',f:"44.29999923706055"}
			 ].


test(infer_destructive_migration,
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
                            _{ '@id' : 'F/1', f : 33.4 },
                            _),
            insert_document(C1,
                            _{ '@id' : 'F/2', f : 44.3 },
                            _)
        )
    ),

    create_context(Descriptor, commit_info{author:"me",
                                           message:"yes"}, Context),

    with_transaction(
        Context,
        delete_schema_document(Context, "F"),
        _,
        [require_migration(true), allow_destructive_migration(true)]
    ),

    findall(
        Doc_Id,
        get_document_uri(Descriptor, false, Doc_Id),
        Docs
    ),
    Docs = ['terminusdb:///data/A/1'],

    \+ ask(Descriptor,
           t('@schema':'F', rdf:type, sys:'Class', schema)
          ).


:- end_tests(migration).
