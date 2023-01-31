:- module('document/migration', []).

:- use_module(instance).
:- use_module(schema).

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

/*
Default_Or_Error := error
                 |  default(Default)
Op := delete_class(Name)
    | create_class(ClassDocument)
    | move_class(Old_Name,New_Name)
    | delete_class_property(Class,Property)
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

/******************************
 *                            *
 *  The interpreter           *
 *                            *
 ******************************/
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

:- end_tests(migration).
