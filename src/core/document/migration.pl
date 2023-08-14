:- module('document/migration', [
              perform_instance_migration/5,
              perform_instance_migration_on_transaction/4,
              infer_weakening_migration/3,
              infer_arbitrary_migration/3,
              operations_are_weakening/1,
              type_weaken/3
          ]).

:- use_module(instance).
:- use_module(schema).
:- use_module(json).
:- use_module(core(triple/base_type)).

:- use_module(library(assoc)).
:- use_module(library(pcre)).
:- use_module(library(uri)).
:- use_module(library(when)).
:- use_module(library(option)).
:- use_module(library(lists)).

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
:- use_module(library(ordsets)).

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
    | replace_context(Context)
    | create_class(ClassDocument)
    | expand_enum(Class,Values)
    | replace_class_metadata(Class,Metadata)
    | replace_class_documentation(Class,Documentation)
    | delete_class_property(Class,Property)
    | create_class_property(Class,Property,Type)  % for option or set
    | create_class_property(Class,Property,Type,Default)
    | move_class_property(Class, Old_Property, New_Property)
    | upcast_class_property(Class, Property, New_Type)
    | cast_class_property(Class, Property, New_Type, Default_Or_Error)
    | change_parents(Class,
           [Parent1,...ParentN],
           [property_default(Property1,Default1),
            ...,
            property_default(PropertyN,DefaultN)])
    | move_class(Old_Name,New_Name)
    | change_key(Class, KeyType, [Property1, ..., PropertyN])
    | unfoldable(Class)
    | not_unfoldable(Class)
    | abstract(Class)
    | not_abstract(Class)
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

change_key(Class, Key_Type, Properties, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Key_Type_Key, Key_Type),
    do_or_die(
        get_dict(Class_Key, Before, Before_Class_Document),
        error(class_does_not_exist(Class), _)
    ),
    (   memberchk(Key_Type_Key, ['Random', 'ValueHash'])
    ->  put_dict('@key', Before_Class_Document,
                 _{ '@type' : Key_Type }, After_Class_Document)
    ;   memberchk(Key_Type_Key, ['Lexical', 'Hash'])
    ->  put_dict('@key', Before_Class_Document,
                 _{ '@type' : Key_Type,
                    '@fields' : Properties }, After_Class_Document)
    ;   throw(error(bad_key_type(Key_Type), _))
    ),
    put_dict(Class_Key, Before, After_Class_Document, After).

expand_enum(Class, Values, Before, After) :-
    atom_string(Class_Key, Class),
    get_dict(Class_Key, Before, Class_Document),
    get_dict('@value', Class_Document, Old_Values),
    list_to_ord_set(Old_Values, Old_Set),
    list_to_ord_set(Values, New_Set),
    union(Old_Set,New_Set, New_Values),
    put_dict(json{ '@value' : New_Values}, Class_Document, After_Document),
    put_dict(Class_Key, Before, After_Document, After).

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

/* replace_class_documentation(Class,Documentation) */
abstract(Class, Before, After) :-
    atom_string(Class_Key, Class),
    get_dict(Class_Key, Before, Class_Document),
    put_dict('@abstract', Class_Document, [], Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).

/* replace_class_documentation(Class,Documentation) */
not_abstract(Class, Before, After) :-
    atom_string(Class_Key, Class),
    get_dict(Class_Key, Before, Class_Document),
    put_dict('@abstrct', Class_Document, [], Final_Document),
    put_dict(Class_Key, Before, Final_Document, After).


/* replace_context(Context) */
replace_context(Context, Before, After) :-
    throw(error(not_implemented('replace_context'), _)),
    put_dict(_{'@context' : Context}, Before, After).


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
    put_dict(Class_Key, Before, Final_Document, After),
    class_children_valid_properties(Class_Key, After).

/* create_class_property(Class,Property,Type,Default) */
create_class_property(Class, Property, Type, _Default, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),
    get_dict(Class_Key, Before, Class_Document),
    \+ get_dict(Property_Key, Class_Document, _),
    put_dict(Property_Key, Class_Document, Type, Final_Document),
    put_dict(Class_Key, Before, Final_Document, After),
    class_children_valid_properties(Class_Key, After).

/* move_class_property(Class, Old_Property, New_Property) */
move_class_property(Class, Old_Property, New_Property, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Old_Property_Key, Old_Property),
    atom_string(New_Property_Key, New_Property),
    get_dict(Class_Key, Before, Before_Class_Document),
    del_dict(Old_Property_Key, Before_Class_Document, Type_Definition, Class_Document0),
    put_dict(New_Property_Key, Class_Document0, Type_Definition, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After),
    class_children_valid_properties(Class_Key, After),
    ensure_properties_not_defined_elsewhere(Class_Key, Old_Property_Key, After).

ensure_properties_not_defined_elsewhere(Class_Key, Old_Property, After) :-
    frame_supermap(After, Supermap),
    invert_supermap(Supermap, Childmap),
    ensure_properties_not_defined_elsewhere(Class_Key, Old_Property, Supermap, Childmap, After).

ensure_properties_not_defined_elsewhere(Class_Key, Old_Property, Supermap, Childmap, After) :-
    class_properties_valid_properties(Class_Key, Supermap, After, Properties),
    die_if(
        get_dict(Old_Property,Properties, _),
        error(immovable_property(Old_Property, Class_Key), _)
    ),
    (   get_dict(Class_Key, Childmap, Children)
    ->  maplist({Old_Property, Supermap, Childmap, After}/[Child]>>(
                    atom_string(Child_Key, Child),
                    ensure_properties_not_defined_elsewhere(Child_Key, Old_Property, Supermap, Childmap, After)
                ), Children)
    ;   true
    ).

family_weaken('List','List').
family_weaken('Set','Set').
family_weaken('Optional', 'Optional').
family_weaken('Cardinality', 'Set').
family_weaken('Optional', 'Set').

class_weaken(Class, Class, _Supermap) :-
    !.
class_weaken(ClassA, ClassB, _Supermap) :-
    base_type(ClassA),
    !,
    base_type(ClassB),
    basetype_subsumption_of(ClassA, ClassB).
class_weaken(ClassA, ClassB, Supermap) :-
    atom_string(ClassA_Atom,ClassA),
    atom_string(ClassB_Atom,ClassB),
    get_dict(ClassA_Atom, Supermap, Supers),
    memberchk(ClassB_Atom, Supers).

type_weaken(Type1, Type2, SuperMap) :-
    is_dict(Type1),
    is_dict(Type2),
    !,
    get_dict('@type', Type1, FamilyString1),
    get_dict('@type', Type2, FamilyString2),
    atom_string(Family1,FamilyString1),
    atom_string(Family2,FamilyString2),
    (   family_weaken(Family1, Family2)
    ->  true
    ;   Family1 = 'Cardinality',
        Family2 = 'Cardinality'
    ->  get_dict('@min_cardinality', Type1, Min1),
        get_dict('@max_cardinality', Type1, Max1),
        get_dict('@min_cardinality', Type2, Min2),
        get_dict('@max_cardinality', Type2, Max2),
        Min2 =< Min1,
        Max2 >= Max1
    ;   Family1 = 'Array',
        Family2 = 'Array'
    ->  get_dict('@dimensions', Type1, Dim1),
        get_dict('@dimensions', Type2, Dim2),
        Dim1 = Dim2
    ),
    get_dict('@class', Type1, Class1Text),
    get_dict('@class', Type2, Class2Text),
    atom_string(Class1, Class1Text),
    atom_string(Class2, Class2Text),
    class_weaken(Class1, Class2, SuperMap).
type_weaken(Type1Text, Type2, SuperMap) :-
    is_dict(Type2),
    text(Type1Text),
    !,
    atom_string(Type1, Type1Text),
    type_is_optional(Type2),
    get_dict('@class', Type2, Class2Text),
    atom_string(Class2, Class2Text),
    class_weaken(Type1, Class2, SuperMap).
type_weaken(Type1Text, Type2Text, Supermap) :-
    text(Type1Text),
    text(Type2Text),
    !,
    atom_string(Type1, Type1Text),
    atom_string(Type2, Type2Text),
    supermap_class_subsumes(Supermap, Type1, Type2).
type_weaken(Type1, Type2, _) :-
    is_dict(Type1),
    text(Type2),
    !,
    fail.

supermap_class_subsumes(_, Type,Type) :-
    !.
supermap_class_subsumes(Supermap, Type1, Type2) :-
    get_dict(Type1, Supermap, Supers),
    memberchk(Type2, Supers).

type_is_optional(Type) :-
    is_dict(Type),
    get_dict('@type', Type, FamilyElt),
    atom_string(Family, FamilyElt),
    (   memberchk(Family, ['Set', 'Optional'])
    ->  true
    ;   Family = 'Cardinality'
    ->  get_dict('@min_cardinality', Type, Min),
        get_dict('@max_cardinality', Type, Max),
        Min =< 1,
        Max >= 1
    ).

one_of_subsumed(OriginalOneOf, WeakeningOneOf) :-
    forall(
        member(Spec, OriginalOneOf),
        \+ forall(
               member(Spec2, WeakeningOneOf),
               \+ Spec :< Spec2
           )
    ).

/* Failure is no-op, success is operation, and no clause head is an error

class_property_weakened(+,+,+,-) is semidet  + error
*/
class_property_weakened(Property, Original, Weakening, _Class, _SuperMap, _Operation),
memberchk(Property,['@type','@key','@subdocument','@inherits','@id','@unfoldable']),
get_dict(Property,Weakening,New_Value),
get_dict(Property,Original,Old_Value),
New_Value = Old_Value =>
    fail.
class_property_weakened('@oneOf', Original, Weakening, Class, _SuperMap, _Operation) =>
    get_dict('@oneOf', Original, OriginalOneOf),
    get_dict('@oneOf', Weakening, WeakeningOneOf),
    do_or_die(
        one_of_subsumed(OriginalOneOf,WeakeningOneOf),
        error(weakening_failure(json{reason: class_enum_value_change_not_a_weakening,
                                     message: "An enum was changed to include a @oneOf specification which did not subsume the original",
                                     class: Class,
                                     old: OriginalOneOf,
                                     new: WeakeningOneOf}), _)
    ),
    fail.
class_property_weakened('@metadata', Original, Weakening, Class, _SuperMap, Operation) =>
    get_dict('@metadata',Original, Old_Metadata),
    get_dict('@metadata',Weakening, New_Metadata),
    \+ Old_Metadata = New_Metadata,
    Operation = replace_class_metadata(Class,New_Metadata).
class_property_weakened('@documentation', Original, Weakening, Class, _SuperMap, Operation) =>
    get_dict('@documentation',Original, Old_Docs),
    get_dict('@documentation',Weakening, New_Docs),
    \+ Old_Docs = New_Docs,
    Operation = replace_class_documentation(Class,New_Docs).
class_property_weakened(Property, Original, Weakening, _Class, _SuperMap, _Operation),
get_dict(Property,Weakening,New_Value),
get_dict(Property,Original,Old_Value),
New_Value = Old_Value =>
    fail.
class_property_weakened('@value', Original, Weakening, Class, _SuperMap, Operation),
get_dict('@value',Weakening,New_Value),
get_dict('@value',Original,Old_Value) =>
    list_to_ord_set(New_Value,New_Set),
    list_to_ord_set(Old_Value,Old_Set),
    (   ord_subset(Old_Set, New_Set)
    ->  subtract(New_Set, Old_Set, Difference),
        Operation = expand_enum(Class, Difference)
    ;   throw(error(weakening_failure(json{reason: class_enum_value_change_not_a_weakening,
                                           message: "An enum was changed to include a set of values which is not a superset",
                                           class: Class,
                                           old: Old_Set,
                                           new: New_Set}), _))
    ).
class_property_weakened(Property, Original, Weakening, Class, SuperMap, Operation),
get_dict(Property, Original, Original_Type),
get_dict(Property, Weakening, Weakening_Type) =>
    do_or_die(type_weaken(Original_Type, Weakening_Type, SuperMap),
              error(weakening_failure(json{ reason: class_property_change_not_a_weakening,
                                            message: "The class property was changed to a type which was not weaker",
                                            class: Class,
                                            property: Property,
                                            original: Original,
                                            candidate: Weakening}),_)),
    Operation = upcast_class_property(Class,Property,Weakening_Type).
class_property_weakened(Property, Value, Weakening, Class, _SuperMap, _Operation) =>
    throw(error(weakening_failure(json{ reason: class_definition_not_a_weakening,
                                        message: "The class definition was not a weakening of the original",
                                        class: Class,
                                        property: Property,
                                        value: Value,
                                        candidate: Weakening}),_)).

class_property_optional('@oneOf', Weakening, Class, _) :-
    throw(error(weakening_failure(json{ reason: class_property_addition_not_optional,
                                        message: "@oneOf can not include optionals",
                                        class: Class,
                                        property: '@oneOf',
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
    Operation = create_class_property(Class,Property,Type).

class_weakened(Class, Definition, Weakening, Supermap, Operations) :-
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
                class_property_weakened(Property,Definition,Weakening,Class,Supermap,Operation1)
            ),
            Operations1),
    append(Operations0,Operations1,Operations).

immediate_class_supers(Class, Schema, Immediate) :-
    get_dict(Class, Schema, Record),
    (   get_dict('@inherits', Record, Immediate_Parents)
    ->  maplist(atom_string, Immediate, Immediate_Parents)
    ;   Immediate = []
    ).

class_supers(Class, Schema, Supers) :-
    immediate_class_supers(Class, Schema, Immediate),
    maplist({Schema}/[I,Sups]>>class_supers(I,Schema,Sups), Immediate, Transitive),
    append(Transitive, Upper),
    append(Immediate, Upper, Supers).

frame_supermap(Schema,Supermap) :-
    dict_keys(Schema, Classes),
    findall(
        Class_Key-Supers,
        (   member(Class, Classes),
            atom_string(Class_Key, Class),
            class_supers(Class_Key, Schema, Supers)),
        Class_Supers),
    dict_create(Supermap, supermap, Class_Supers).

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
                (   Class = '@context'
                ->  Operation = replace_context(Definition)
                ;   get_dict(Class, Weakened, Definition),
                    Operation = create_class(Definition)
                )
            ),
            Added,
            Operations0),
    ord_intersection(Old,New,Shared),
    (   Shared \= []
    ->  frame_supermap(Schema,Supermap)
    ;   Supermap = supermap{}
    ),
    findall(Intermediate_Operations,
            (   member(Key, Shared),
                get_dict(Key,Schema,Old_Class),
                get_dict(Key,Weakened,New_Class),
                (   Key = '@context'
                ->  (   Old_Class = New_Class % no change
                    ->  fail
                    ;   throw(error(
                                  weakening_failure(json{ reason: not_a_weakening_context_changed,
                                                          message: "The change of context may cause changes of instance data"}), _)))
                ;   class_weakened(Key,Old_Class,New_Class,Supermap,Intermediate_Operations)
                )
            ),
            Operations_List),
    append(Operations_List, Operations1),
    append(Operations0,Operations1,Operations).

class_strengthened('@context',Old_Class,New_Class,_Supermap,Operations) :-
    !,
    \+ Old_Class = New_Class,
    Operations = [replace_context(New_Class)].
class_strengthened(Key,Old_Class,New_Class,Supermap,Operations) :-
    class_weakened(Key,Old_Class,New_Class,Supermap,Operations).

schema_strengthening(Schema,Weakened,Operations) :-
    dict_keys(Schema,Old),
    dict_keys(Weakened,New),
    ord_subtract(Old,New,Deleted),
    maplist([Deleted,delete_class(Deleted)]>>true, Deleted, Operations0),
    ord_subtract(New,Old,Added),
    maplist({Weakened}/[Class,Operation]>>(
                (   Class = '@context'
                ->  get_dict(Class, Weakened, Definition),
                    Operation = replace_context(Definition)
                ;   get_dict(Class, Weakened, Definition),
                    Operation = create_class(Definition)
                )
            ),
            Added,
            Operations1),
    ord_intersection(Old,New,Shared),
    (   Shared \= []
    ->  frame_supermap(Schema,Supermap)
    ;   Supermap = supermap{}
    ),
    % We can do more here! Dropped properties should be accepted
    findall(Intermediate_Operations,
            (   member(Key, Shared),
                get_dict(Key,Schema,Old_Class),
                get_dict(Key,Weakened,New_Class),
                class_strengthened(Key,Old_Class,New_Class,Supermap,Intermediate_Operations)
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

perform_migration_rule(weakening,_Before, After, Operations_List, Validation_Out, Meta_Data) :-
    transaction_objects_to_validation_objects([After], [Validation]),
    length(Operations_List, Schema_Operations),
    Meta_Data = _{ instance_operations:0,
				   schema_operations: Schema_Operations
                 },
    (   Operations_List = []
    ->  Validation = Validation_Out
    ;   get_dict(schema_objects, Validation, [Schema_Object]),
        put_dict(_{changed:true}, Schema_Object, Schema_Object_Changed),
        put_dict(_{schema_objects:[Schema_Object_Changed]}, Validation, Validation_Out)
    ).
perform_migration_rule(strengthening, Before, After, Operations_List, Validation_Out, Meta_Data) :-
    interpret_instance_operations(Operations_List, Before, After, Count),
    transaction_objects_to_validation_objects([After], [Validation]),
    length(Operations_List, Schema_Operations),
    Meta_Data = _{ instance_operations: Count,
				   schema_operations: Schema_Operations
                 },
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

infer_migration(Rule, [Validation], [New_Validation], Meta_Data) :-
    validation_objects_to_transaction_objects([Validation],[After_Transaction]),
    Descriptor = (Validation.descriptor),
    open_descriptor(Descriptor, Before_Transaction),
    create_class_dictionary(Before_Transaction, Before),
    create_class_dictionary(After_Transaction, After),
    schema_inference_rule(Rule, Before, After, Operations),
    migration_list_to_ast_list(Operations_List,Operations),
    !,
    perform_migration_rule(Rule, Before_Transaction, After_Transaction, Operations_List, Validation0, Meta_Data),
    atom_json_dict(Migration, Operations_List, [default_tag(json), width(0)]),
    (   get_dict(commit_info, Validation, Commit_Info)
    ->  true
    ;   Commit_Info = commit_info{}),
    put_dict(_{ migration: Migration }, Commit_Info, Commit_Info0),
    put_dict(_{commit_info: Commit_Info0}, Validation0, New_Validation),
    copy_validation_change_status(Validation, New_Validation).

copy_validation_change_status(Old_Validation,New_Validation) :-
    Old_Schemas = (Old_Validation.schema_objects),
    sort(descriptor, @<, Old_Schemas, Old_Schemas_Sorted),
    New_Schemas = (New_Validation.schema_objects),
    sort(descriptor, @<, New_Schemas, New_Schemas_Sorted),
    maplist(
        [Old_Schema, New_Schema]>>(
            get_dict(changed, Old_Schema, Changed),
            nb_set_dict(changed, New_Schema, Changed)
        ),
        Old_Schemas_Sorted,
        New_Schemas_Sorted
    ).

infer_weakening_migration(Validations,New_Validations, Meta_Data) :-
    infer_migration(weakening, Validations, New_Validations, Meta_Data).

infer_arbitrary_migration(Validations,New_Validations, Meta_Data) :-
    infer_migration(strengthening, Validations, New_Validations, Meta_Data).

/* upcast_class_property(Class, Property, New_Type) */
upcast_class_property(Class, Property, New_Type_Candidate, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),

    % Backward compatible fix for bug https://github.com/terminusdb/terminusdb/issues/1853
    (   is_dict(New_Type_Candidate),
        get_dict('@type', New_Type_Candidate, 'Class')
    ->  get_dict(Property_Key, New_Type_Candidate, New_Type)
    ;   New_Type = New_Type_Candidate
    ),

    get_dict(Class_Key, Before, Before_Class_Document),
    get_dict(Property_Key, Before_Class_Document, Type_Definition),
    frame_supermap(Before, Supermap),
    do_or_die(
        type_weaken(Type_Definition, New_Type, Supermap),
        error(not_an_irrefutable_weakening_operation(
                  upcast_class_property,
                  Class,
                  Property,
                  New_Type),
              _)
    ),
    put_dict(Property_Key, Before_Class_Document, New_Type, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After),
    class_children_valid_properties(Class_Key, After).

/* cast_class_property(Class, Property, New_Type, Default_Or_Error) */
cast_class_property(Class, Property, New_Type, _Default_Or_Error, Before, After) :-
    atom_string(Class_Key, Class),
    atom_string(Property_Key, Property),

    get_dict(Class_Key, Before, Before_Class_Document),
    get_dict(Property_Key, Before_Class_Document, _Type_Definition),
    put_dict(Property_Key, Before_Class_Document, New_Type, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After),
    class_children_valid_properties(Class_Key, After).

/* change_parents(Class, [Parent1,...ParentN], [property_default(Property1, Default1)])
 */
change_parents(Class, Parent_List, Property_Defaults, Before, After) :-
    atom_string(Class_Key, Class),
    get_dict(Class_Key, Before, Before_Class_Document),
    put_dict(_{ '@inherits' : Parent_List}, Before_Class_Document, After_Class_Document),
    put_dict(Class_Key, Before, After_Class_Document, After),
    check_children_property_defaults(Before, After, Class_Key, Property_Defaults).

check_children_property_defaults(Before, After, Class_Key, Property_Defaults) :-
    frame_supermap(After, After_Supermap),
    frame_supermap(Before, Before_Supermap),
    invert_supermap(After_Supermap, Childmap),
    check_children_property_defaults(Before, After, Class_Key, Before_Supermap, After_Supermap, Childmap, Property_Defaults).

check_children_property_defaults(Before, After, Class_Key, Before_Supermap, After_Supermap, Childmap, Property_Defaults) :-
    class_properties_valid_properties(Class_Key, After_Supermap, After, After_Properties),
    class_properties_valid_properties(Class_Key, Before_Supermap, Before, Before_Properties),
    findall(
        Property-Type,
        (   get_dict(Property, After_Properties, Type),
            \+ get_dict(Property, Before_Properties, _)
        ),
        Added
    ),
    forall(
        member(Property-Type, Added),
        (   type_is_optional(Type)
        ;   is_dict(Type),
            get_dict('@type', Type, "List")
        ->  true
        ;   atom_string(Property, Property_String),
            do_or_die(
                member(property_default(Property_String, _), Property_Defaults),
                error(required_property_has_no_default(Class_Key,Property_String), _)
            )
        )
    ),
    forall(
        (   get_dict(Property, After_Properties, Type2),
            get_dict(Property, Before_Properties, Type1)
        ),
        (   do_or_die(
                Type1 = Type2,
                error(property_type_changed_due_to_parent_change(Class_Key, Property, Type1, Type2), _)
            )
        )
    ),
    forall(
        member(property_default(Property_String, _), Property_Defaults),
        (   atom_string(Property, Property_String),
            do_or_die(
                memberchk(Property-_, Added),
                error(unused_property_default(Class_Key, Property), _)
            )
        )
    ),
    (   get_dict(Class_Key, Childmap, Child_Classes)
    ->  forall(
            member(Child, Child_Classes),
            (   atom_string(Child_Key, Child),
                check_children_property_defaults(Before, After, Child_Key, Before_Supermap, After_Supermap, Childmap, Property_Defaults)
            )
        )
    ;   true
    ).

check_property_defaults([], _Properties).
check_property_defaults([property_default(Property, _Default)|Rest], Properties) :-
    atom_string(Prop_Key, Property),
    % The actual data should probably be checked to be of the appropriate type here.
    get_dict(Prop_Key, Properties, _),
    check_property_defaults(Rest, Properties).

invert_supermap(Supermap, Childmap) :-
    findall(
        Parent-Child,
        (   get_dict(Child, Supermap, Parents),
            member(Parent, Parents)
        ),
        Pairs
    ),
    sort(Pairs, Sorted_Pairs),
    create_childmap(Sorted_Pairs, Childmap).

create_childmap(Pairs, Childmap) :-
    maplist([Key-Value,Key-[Value]]>>true, Pairs, List_Pairs),
    create_child_lists(List_Pairs, Lists),
    dict_create(Childmap, children, Lists).

create_child_lists([], []).
create_child_lists([Key-Value], [Key-Value]).
create_child_lists([Key-Value1, Key-Value2|Rest], Result) :-
    !,
    append(Value1,Value2,Value),
    create_child_lists([Key-Value|Rest], Result).
create_child_lists([Key-Value|Rest], [Key-Value|Result]) :-
    create_child_lists(Rest, Result).

class_children_valid_properties(Class_Key, After) :-
    class_children_valid_properties(Class_Key, After, _).

class_children_valid_properties(Class_Key, After, Properties) :-
    frame_supermap(After, Supermap),
    invert_supermap(Supermap, Childmap),
    class_children_valid_properties(Class_Key, Supermap, Childmap, After, Properties).

class_children_valid_properties(Class_Key, Supermap, Childmap, After, Properties) :-
    class_properties_valid_properties(Class_Key, Supermap, After, Properties),
    (   get_dict(Class_Key, Childmap, Children)
    ->  maplist({Supermap, Childmap, After}/[Child]>>(
                    atom_string(Child_Key, Child),
                    class_children_valid_properties(Child_Key, Supermap, Childmap, After, _)
                ), Children)
    ;   true
    ).

class_properties_valid_properties(Class_Key, Supermap, After, Valid) :-
    get_dict(Class_Key, Supermap, Parents),
    findall(
        Property-Range,
        (   (   Current_Class = Class_Key
            ;   member(Parent, Parents),
                atom_string(Current_Class, Parent)
            ),
            get_dict(Current_Class, After, Class_Document),
            get_dict(Property, Class_Document, Range),
            \+ has_at(Property)
        ),
        Pairs
    ),
    pairs_satisfying_diamond_property(Pairs, Class_Key, Supermap, Collapsed_Pairs),
    get_dict(Class_Key, After, Class_Document),
    check_class_document_pairs(Class_Document, Collapsed_Pairs),
    dict_create(Valid, frame, Collapsed_Pairs).

check_class_document_pairs(Class_Document, Pairs) :-
    get_dict('@id', Class_Document, Class),
    forall(
        (   get_dict(Property, Class_Document, Range),
            \+ has_at(Property)
        ),
        do_or_die(
            member(Property-Range, Pairs),
            error(violation_of_diamond_property(Class, Property), _)
        )
    ).

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

rewrite_uri(Old_Context, New_Context, URI, New_URI) :-
    atom(URI),
    get_dict('@base', Old_Context, Old_Base),
    string_length(Old_Base, Base_Length),
    sub_atom(URI, 0, Base_Length, _, Old_Base),
    !,
    sub_atom(URI, Base_Length, _, 0, Branch),
    get_dict('@base', New_Context, New_Base),
    atomic_concat(New_Base, Branch, New_URI).
rewrite_uri(Old_Context, New_Context, URI, New_URI) :-
    atom(URI),
    get_dict('@schema', Old_Context, Old_Schema),
    string_length(Old_Schema, Schema_Length),
    sub_atom(URI, 0, Schema_Length, _, Old_Schema),
    !,
    sub_atom(URI, Schema_Length, _, 0, Branch),
    get_dict('@schema', New_Context, New_Schema),
    atomic_concat(New_Schema, Branch, New_URI).
rewrite_uri(_Old_Context, _New_Context, URI, URI).

rewrite_triple(Old_Context,New_Context,S,P,O,NS,NP,NO) :-
    rewrite_uri(Old_Context,New_Context,S,NS),
    rewrite_uri(Old_Context,New_Context,P,NP),
    rewrite_uri(Old_Context,New_Context,O,NO).

delete_document_ids_key_value('@id',_,_,_) =>
    fail.
delete_document_ids_key_value(P,V,Key,Value) =>
    P = Key,
    rewrite_document_ids(V,Value).

delete_document_ids(Document, New_Document),
is_dict(Document) =>
    findall(
        Key-Value,
        (   get_dict(K, Document, V),
            delete_document_ids_key_value(K,V,Key,Value)
        ),
        Pairs
    ),
    dict_create(New_Document, json, Pairs).
delete_document_ids(Document, New_Document),
is_list(Document) =>
    maplist([D0,D1]>>delete_document_ids(D0,D1),
            Document, New_Document).
rewrite_document_ids(Value, New_Value) =>
    Value = New_Value.

delete_value(base_class(_),_,_) => true.
delete_value(unit,_,_) => true.
delete_value(enum(_,_),_,_) => true.
delete_value(_, Before, Value) =>
    database_prefixes(Before, Prefixes),
    delete_subdocument(Before, Prefixes, Value).


create_class_property_default_operation(Class, Property, Type, Default, After, Count) :-
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

create_class_property_operation(Class, Property, Type, After, Count) :-
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

delete_class_property_operation(Before, Class, Property, Type, Count) :-
    count_solutions(
        (   ask(Before,
                (   isa(X, Class),
                    t(X, Property, Value),
                    delete(X, Property, Value))),
            delete_value(Type,Before,Value)
        ),
        Count
    ).

class_properties_deleted_and_added(Before, After, C, Deleted, Added) :-
    class_frame(Before, C, Before_Frame),
    class_frame(After, C, After_Frame),
    findall(
        Prop-Type,
        (   get_dict(Prop, Before_Frame, Type),
            \+ has_at(Prop),
            \+ get_dict(Prop, After_Frame, _)
        ),
        Deleted
    ),
    findall(
        Prop-Type,
        (   get_dict(Prop, After_Frame, Type),
            \+ has_at(Prop),
            \+ get_dict(Prop, Before_Frame, _)
        ),
        Added
    ).

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
interpret_instance_operation_(replace_context(New_Context), Before, After, Count) :-
    (   database_context_object(Before,Old_Context)
    ->  count_solutions(
            (   ask(Before,
                    (   t(S, P, O),
                        delete(S, P, O)),
                    [compress_prefixes(false)]),
                rewrite_triple(Old_Context,New_Context,S,P,O,NS,NP,NO),
                ask(After,
                    insert(NS,NP,NO))),
            Count
        )
    ;   Count = 0
    ).
interpret_instance_operation_(expand_enum(_Class, _Values), _Before, _After, 0).
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
interpret_instance_operation_(change_key(Class, _Type, _Properties), Before, After, Count) :-
    database_prefixes(Before, Prefixes),
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    findall(
        New_Uri,
        (   ask(Before,
                t(Uri, rdf:type, Class_Ex)),
            once(
                (   get_document(Before, Uri, Document),
                    delete_document(Before, Uri),
                    delete_document_ids(Document, Final_Document),
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
        Uris
    ),

    (   has_duplicates(Uris, Duplicates)
    ->  throw(error(id_overlap_after_key_change(Duplicates)))
    ;   true
    ),

    length(Uris, Count).

interpret_instance_operation_(delete_class_property(Class, Property), Before, _After, Count) :-
    % Todo: Tagged Union
    database_prefixes(Before, Prefixes),
    prefix_expand_schema(Class, Prefixes, Class_Ex),
    prefix_expand_schema(Property, Prefixes, P),
    once(class_predicate_type(Before,Class_Ex, P, Type)),
    delete_class_property_operation(Before, Class_Ex, P, Type, Count).
interpret_instance_operation_(create_class_property(Class, Property, Type, Default), _Before, After, Count) :-
    create_class_property_default_operation(Class, Property, Type, Default, After, Count).
interpret_instance_operation_(create_class_property(Class, Property, Type), _Before, After, Count) :-
    create_class_property_operation(Class, Property, Type, After, Count).
interpret_instance_operation_(upcast_class_property(Class, Property, New_Type_Candidate), _Before, After, Count) :-
    % Backward compatible fix for bug https://github.com/terminusdb/terminusdb/issues/1853
    (   is_dict(New_Type_Candidate),
        get_dict('@type', New_Type_Candidate, 'Class'),
        atom_string(Property_Key, Property)
    ->  get_dict(Property_Key, New_Type_Candidate, New_Type)
    ;   New_Type = New_Type_Candidate
    ),

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
interpret_instance_operation_(move_class_property(Class,Old_Property,New_Property), _Before, After, Count) :-
    count_solutions(
        ask(After,
            (   isa(X, Class),
                t(X, Old_Property, Y),
                delete(X, Old_Property, Y),
                insert(X, New_Property, Y))),
        Count
    ).
interpret_instance_operation_(change_parents(Class,_,Property_Defaults), Before, After, Count) :-
    findall(C,
            class_subsumed(Before, C, Class),
            Before_Classes),
    findall(C,
            class_subsumed(After, C, Class),
            After_Classes),
    append(Before_Classes, After_Classes, Changed_Classes_Unsorted),
    sort(Changed_Classes_Unsorted, Changed_Classes),
    findall(
        Count,
        (   member(C, Changed_Classes),
            class_properties_deleted_and_added(Before, After, C, Deleted, Added),
            (   member(P-T, Deleted),
                delete_class_property_operation(Before, C, P, T, Count)
            ;   member(P-T, Added),
                atom_string(P, PString),
                (   memberchk(property_default(PString,Default), Property_Defaults)
                ->  create_class_property_default_operation(C, P, T, Default, After, Count)
                ;   create_class_property_operation(C, P, T, After, Count)
                )
            )
        ),
        Counts
    ),
    sum_list(Counts, Count).

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
    is_transaction(Transaction),
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
    %test_utils:print_all_triples(Transaction, schema),
    database_context_object(Transaction, Context),
    findall(
        Class-Class_Document,
        (   api_document:api_get_documents(Transaction, schema, Config, Class_Document),
            get_dict('@id',Class_Document, Class_String),
            atom_string(Class,Class_String)
        ),
        Pairs
    ),
    dict_create(Dictionary, json, ['@context'-Context|Pairs]).

class_dictionary_to_schema(Dictionary, [Context|Schema]) :-
    del_dict('@context', Dictionary, Context0, Class_Dictionary),
    put_dict(_{ '@type' : "@context"}, Context0, Context),
    dict_pairs(Class_Dictionary, _, Pairs),
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
    ;   run_transactions([After_Transaction], false, _, [inside_migration(true)])
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


operation_is_weakening(create_class(_)).
operation_is_weakening(create_class_property(_,_,_)).
operation_is_weakening(upcast_class_property(_,_,_)).

operations_are_weakening(L) :-
    maplist(operation_is_weakening, L).

:- begin_tests(migration).

:- use_module(core(util/test_utils)).

test(property_weakening_optional, [
         error(
             weakening_failure(
                 json{ reason: class_property_addition_not_optional,
                       message: "The class property which was not added, did not have an optional type",
                       class: 'Squash',
                       property: is_a_pumpkin,
                       candidate: _Weakening}),
             _)]) :-

    class_property_optional(
        is_a_pumpkin,
        json{'@id':'Squash', '@key':json{'@fields':[genus, species], '@type':"Hash"}, '@type':'Class', colour:'xsd:string', genus:'xsd:string', is_a_pumpkin:'xsd:boolean', name:'xsd:string', shape:'xsd:string', species:'xsd:string'},
        'Squash', _Optional).

test(property_weakening, []) :-
    \+ class_property_weakened(
           d,
           json{'@id':'D', '@type':'Class', d:json{'@class':'xsd:string', '@type':'List'}},
           json{'@id':'D', '@type':'Class', d:json{'@class':'xsd:string', '@type':'List'}},
           'D',
           json{'D' : []},
           _Weakened).

test(weaken_enum_failure, [
         error(weakening_failure(json{class:'Team',message:"An enum was changed to include a set of values which is not a superset",new:['Amazing Marketing','Information Technology'],old:['IT','Marketing'],reason:class_enum_value_change_not_a_weakening}), _)
     ]) :-
    schema_weakening(json{'@context':_1124{'@base':"http://i/",
                                           '@schema':"http://s/",
                                           '@type':'Context'},
                      'Team':json{'@id':'Team', '@type':'Enum',
                                  '@value':['IT', 'Marketing']}},
                     json{'@context':_1580{'@base':"http://i/",
                                           '@schema':"http://s/",
                                           '@type':'Context'},
                      'Team':json{'@id':'Team', '@type':'Enum',
                                  '@value':['Information Technology', 'Amazing Marketing']}}, _8856).

test(weaken_enum_success, []) :-
    schema_weakening(json{'@context':_1124{'@base':"http://i/",
                                           '@schema':"http://s/",
                                           '@type':'Context'},
                          'Team':json{'@id':'Team', '@type':'Enum',
                                      '@value':['IT', 'Marketing']}},
                     json{'@context':_1580{'@base':"http://i/",
                                           '@schema':"http://s/",
                                           '@type':'Context'},
                          'Team':json{'@id':'Team', '@type':'Enum',
                                      '@value':['IT', 'Marketing', 'Finance']}}, Operations),

    Operations = [  expand_enum('Team',['Finance']) ].

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
    Dictionary = json{'@context':json{'@base':"terminusdb:///data/",
                                      '@schema':"terminusdb:///schema#",
                                      '@type':'Context'},
                      'A':json{'@id':'A', '@type':'Class', a:'xsd:string'}},

    interpret_schema_operations(Ops, Dictionary, After),

    After = json{'@context':json{'@base':"terminusdb:///data/",
                                 '@schema':"terminusdb:///schema#",
                                 '@type':'Context'},
                 'C':json{'@id':"C", '@type':"Class", c:"xsd:string"}}.


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

test(move_and_weaken_with_deformed_upcast,
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
        upcast_class_property("B", "a", json{ '@id' : "B",
                                              '@type' : 'Class',
                                              a:json{ '@type' : "Optional",
                                                      '@class' : "xsd:string"}})
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
    After = json{'@context':_{ '@base':"terminusdb:///data/",
							   '@schema':"terminusdb:///schema#",
							   '@type':'Context'
						     },
                 'A':json{'@id':'A','@type':'Class', a:json{ '@type' : "Optional",
                                                             '@class' : 'xsd:string'}},
                 'B':json{'@id': 'B', '@type':'Class', b: 'xsd:integer'}},
    schema_weakening(Before, After, Operations),

    Operations = [ create_class(json{ '@id':'B',
							          '@type':'Class',
							          b:'xsd:integer'
							        }),
				   upcast_class_property('A',
								         a,
								         json{ '@class':'xsd:string',
									           '@type':"Optional"
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
    After = json{
                '@context':_{ '@base':"terminusdb:///data/",
							  '@schema':"terminusdb:///schema#",
							  '@type':'Context'
							}
			},
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
    After = json{ '@context':_{ '@base':"terminusdb:///data/",
							    '@schema':"terminusdb:///schema#",
							    '@type':'Context'
							  },
                  'A':json{'@id':'A','@type':'Class'}},
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

test(change_context,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      blocked('Not yet implemented')
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
    create_context(Descriptor, commit_info{author:"me",
                                           message:"yes"}, C2),
    with_transaction(
        C2,
        (   replace_context_document(C2,
                                     _{ '@type' : "@context",
                                        '@base' : "http://a/",
                                        '@schema' : "http://a#"
                                      })
        ),
        _Meta_Data,
        [require_migration(true), allow_destructive_migration(true)]
    ).

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
        Meta_Data,
        [require_migration(true), allow_destructive_migration(true)]
    ),
    Meta_Data = meta_data{
                    data_versions:[],
					deletes:0,
					inserts:0,
					instance_operations:2,
					schema_operations:1,
					transaction_retry_count:0
				},
    findall(
        Doc_Id,
        get_document_uri(Descriptor, false, Doc_Id),
        Docs
    ),
    Docs = ['terminusdb:///data/A/1'],

    \+ ask(Descriptor,
           t('@schema':'F', rdf:type, sys:'Class', schema)
          ).

test(infer_class_property_weakening,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    create_context(Descriptor, commit_info{author:"me",
                                           message:"yes"}, C1),
    with_transaction(
        C1,
        replace_schema_document(C1,
                                _{ '@type' : "Class",
                                   '@id' : "A",
                                   a : "xsd:string",
                                   b : _{ '@type' : "Optional",
                                          '@class' : "xsd:boolean"}
                                 }),
        Meta_Data
    ),
    get_dict(schema_operations, Meta_Data, 1).

test(subsumes_oneof, []) :-
    New = [json{parameters:json{'@class':'Parameter','@type':'List'},
                receives:json{'@class':'Parameter','@type':'List'}},
           json{returns:'Returns',
                returns_multiple:json{'@class':'Returns','@type':'List'},
                yields:'Returns'}],
    Old = [json{parameters:json{'@class':'Parameter','@type':'List'},
                receives:json{'@class':'Parameter','@type':'List'}},
           json{returns:'Returns',
                returns_multiple:json{'@class':'Returns','@type':'List'},
                void:'sys:Unit',yields:'Returns'}],
    one_of_subsumed(New, Old),
    \+ one_of_subsumed(Old, New).

test(weaken_oneof, [
         error(weakening_failure(json{class:'Definition',message:"An enum was changed to include a @oneOf specification which did not subsume the original",new:[json{parameters:json{'@class':'Parameter','@type':'List'},receives:json{'@class':'Parameter','@type':'List'}},json{returns:'Returns',returns_multiple:json{'@class':'Returns','@type':'List'},yields:'Returns'}],old:[json{parameters:json{'@class':'Parameter','@type':'List'},receives:json{'@class':'Parameter','@type':'List'}},json{returns:'Returns',returns_multiple:json{'@class':'Returns','@type':'List'},void:'sys:Unit',yields:'Returns'}],reason:class_enum_value_change_not_a_weakening}), _)
     ]) :-
    Before = json{'@id':'Definition', '@inherits':'Documented', '@oneOf':[json{parameters:json{'@class':'Parameter', '@type':'List'}, receives:json{'@class':'Parameter', '@type':'List'}}, json{returns:'Returns', returns_multiple:json{'@class':'Returns', '@type':'List'}, void:'sys:Unit', yields:'Returns'}], '@type':'Class', examples:json{'@class':'xsd:string', '@dimensions':1, '@type':'Array'}, extendedSummary:json{'@class':'xsd:string', '@type':'Optional'}, index:json{'@class':'xsd:integer', '@type':'Optional'}, notes:json{'@class':'xsd:string', '@type':'Optional'}, raises:json{'@class':'Exception', '@type':'Set'}, references:json{'@class':'xsd:string', '@type':'Optional'}, section:json{'@class':'xsd:string', '@type':'Optional'}, seeAlso:json{'@class':'Definition', '@type':'Set'}, signature:json{'@class':'xsd:string', '@type':'Optional'}},
    After = json{'@id':'Definition', '@inherits':'Documented', '@oneOf':[json{parameters:json{'@class':'Parameter', '@type':'List'}, receives:json{'@class':'Parameter', '@type':'List'}}, json{returns:'Returns', returns_multiple:json{'@class':'Returns', '@type':'List'}, yields:'Returns'}], '@type':'Class', examples:json{'@class':'xsd:string', '@dimensions':1, '@type':'Array'}, extendedSummary:json{'@class':'xsd:string', '@type':'Optional'}, index:json{'@class':'xsd:integer', '@type':'Optional'}, notes:json{'@class':'xsd:string', '@type':'Optional'}, raises:json{'@class':'Exception', '@type':'Set'}, references:json{'@class':'xsd:string', '@type':'Optional'}, section:json{'@class':'xsd:string', '@type':'Optional'}, seeAlso:json{'@class':'Definition', '@type':'Set'}, signature:json{'@class':'xsd:string', '@type':'Optional'}},
    class_weakened('Definition', Before, After, supermap{}, _5762).

test(weaken_subsumed_range, []) :-
    Before = json{'@id':'A', '@type':'Class', b : 'B1'},
    After = json{'@id':'A', '@type':'Class', b : 'B2'},
    class_weakened('A', Before, After, supermap{'B1' : ['B2']}, Result),
    Result = [ upcast_class_property('A',b,'B2')].

test(double_type_weaken, []) :-
    type_weaken('B1', json{'@type':'Optional', '@class':'B2'}, supermap{'B1':['B2']}).

test(weaken_subsumed_and_optional_range, []) :-
    Before = json{'@id':'A', '@type':'Class', b : 'B1'},
    After = json{'@id':'A', '@type':'Class', b : json{'@type' : 'Optional', '@class' : 'B2'}},
    class_weakened('A', Before, After, supermap{'B1' : ['B2']}, Result),
    Result = [ upcast_class_property('A',
									 b,
									 json{ '@class':'B2',
										   '@type':'Optional'
										 })
			 ].


test(same_metadata,
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
                               _Result,
                               []),

    get_schema_document(Descriptor, "F", F),
    F = json{ '@id':'F',
              '@metadata':json{asdf:"fdsa"},
              '@type':'Class',
              f:json{'@class':'xsd:float','@type':'Optional'}
            },

    create_class_dictionary(Descriptor, Dictionary),

    schema_inference_rule(
        weakening,
        Dictionary,
        Dictionary,
        Operations
    ),
    Operations = [].

test(change_a_parent,
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
                            _{ '@id' : 'D/1', d : ["foo"] },
                            _),
            insert_document(C1,
                            _{ '@id' : 'D/2', d : ["bar"] },
                            _)
        )
    ),

    Term_Ops = [
        change_parents("D", ["A"], [property_default("a", "Some data")])
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),
    Result = metadata{
                 instance_operations:2,
				 schema_operations:1
			 },

    findall(
        DocD,
        get_document_by_type(Descriptor, "D", DocD),
        D_Docs),
    D_Docs = [ json{ '@id':'D/1',
					 '@type':'D',
					 a:"Some data",
					 d:["foo"]
				   },
			   json{ '@id':'D/2',
					 '@type':'D',
					 a:"Some data",
					 d:["bar"]
				   }
			 ].

test(change_a_parent_with_optional,
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
                            _{ '@id' : 'C/1', c : "foo" },
                            _),
            insert_document(C1,
                            _{ '@id' : 'C/2', c : "bar" },
                            _)
        )
    ),

    Term_Ops = [
        change_parents("C", ["F"], [])
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Result,
                               []),
    Result = metadata{
                 instance_operations:0,
				 schema_operations:1
			 },

    findall(
        DocC,
        get_document_by_type(Descriptor, "C", DocC),
        C_Docs),
    C_Docs = [ json{ '@id':'C/1',
					 '@type':'C',
					 c:"foo"
				   },
			   json{ '@id':'C/2',
					 '@type':'C',
					 c:"bar"
				   }
			 ].

test(change_a_parent_with_unused_default,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(unused_property_default('C',d), _)
     ]) :-

    with_test_transaction(
        Descriptor,
        C1,
        (   insert_document(C1,
                            _{ '@id' : 'C/1', c : "foo" },
                            _),
            insert_document(C1,
                            _{ '@id' : 'C/2', c : "bar" },
                            _)
        )
    ),

    Term_Ops = [
        change_parents("C", ["F"], [property_default("d", "foo")])
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               _Result,
                               []).

test(change_a_parent_with_default_for_existing_property,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(unused_property_default('C',c), _)
     ]) :-

    with_test_transaction(
        Descriptor,
        C1,
        (   insert_document(C1,
                            _{ '@id' : 'C/1', c : "foo" },
                            _),
            insert_document(C1,
                            _{ '@id' : 'C/2', c : "bar" },
                            _)
        )
    ),

    Term_Ops = [
        change_parents("C", ["F"], [property_default("c", "baz")])
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               _Result,
                               []).

test(move_class_property,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before2,Descriptor)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    Term_Ops = [
        move_class_property("A", "a", "b")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               _Result,
                               []).

before3('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type" : "Class",
  "@id" : "A",
  "a" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "B",
  "a" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "Sub",
  "@inherits" : ["A", "B"] }
').

test(move_class_property_inheritance_conflict,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before3,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(immovable_property(a,'Sub'), _)
     ]) :-

    Term_Ops = [
        move_class_property("B", "a", "b")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               _,
                               []).

before4('
{ "@base": "terminusdb:///data/",
  "@schema": "terminusdb:///schema#",
  "@type": "@context"}

{ "@type" : "Class",
  "@id" : "A",
  "a" : "xsd:string" }

{ "@type" : "Class",
  "@id" : "B",
  "@inherits" : "A" }
').

test(move_class_property_inheritance_no_conflict,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before4,Descriptor)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Descriptor,
        C1,
        (   insert_document(C1,
                            _{ '@type' : "A", '@id' : 'A/1', a : "foo" },
                            _),
            insert_document(C1,
                            _{ '@type' : "B", '@id' : 'B/2', a : "bar" },
                            _)
        )
    ),


    Term_Ops = [
        move_class_property("A", "a", "b")
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
        DocA,
        get_document_by_type(Descriptor, "A", DocA),
        A_Docs),

    A_Docs = [ json{ '@id':'A/1',
					 '@type':'A',
					 b:"foo"
				   },
			   json{ '@id':'B/2',
					 '@type':'B',
					 b:"bar"
				   }
			 ].


test(move_class_property_which_doesnt_exist,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before4,Descriptor)
            )),
      cleanup(teardown_temp_store(State)),
      error(schema_operation_failed(
                '{"@type":"MoveClassProperty", "class":"A", "from":"b", "to":"c"}',
                json{'@context':_{'@base':"terminusdb:///data/",'@schema':"terminusdb:///schema#",'@type':'Context'},
                     'A':json{'@id':'A','@type':'Class',a:'xsd:string'},
                     'B':json{'@id':'B','@inherits':'A','@type':'Class'}}), _)
     ]) :-

    Term_Ops = [
        move_class_property("A", "b", "c")
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               _,
                               []).

test(change_key,
     [setup((setup_temp_store(State),
             test_document_label_descriptor(database,Descriptor),
             write_schema(before4,Descriptor)
            )),
      cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Descriptor,
        C1,
        (   insert_document(C1,
                            _{ '@type' : "A", '@id' : 'A/1', a : "foo" },
                            _),
            insert_document(C1,
                            _{ '@type' : "B", '@id' : 'B/2', a : "bar" },
                            _)
        )
    ),

    Term_Ops = [
        change_key("B", "Lexical", ["a"])
    ],
    migration_list_to_ast_list(Ops,Term_Ops),

    perform_instance_migration(Descriptor, commit_info{ author: "me",
                                                        message: "Fancy" },
                               Ops,
                               Results,
                               []),

    Results = metadata{instance_operations:1,schema_operations:1},

    findall(
        DocA,
        get_document_by_type(Descriptor, "A", DocA),
        A_Docs),

    A_Docs = [ json{ '@id':'A/1',
					 '@type':'A',
					 a:"foo"
				   },
			   json{ '@id':'B/bar',
					 '@type':'B',
					 a:"bar"
				   }
			 ].

:- end_tests(migration).
