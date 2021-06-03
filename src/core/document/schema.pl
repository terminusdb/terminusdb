:- module('document/schema', [
              refute_schema/2,
              is_enum/2,
              is_simple_class/2,
              is_base_type/1,
              is_built_in/1,
              refute_class/3,
              class_predicate_type/4,
              type_descriptor/3,
              class_subsumed/3,
              key_descriptor/3,
              type_family_constructor/1
          ]).

/*
 * Schema language for JSON as a graph
 *
 */

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(query), [has_at/1, compress_uri/4]).

:- use_module(json). % This feels very circular.

check_schema :-
    (   schema_not_wf(Witness)
    ->  throw(error(schema_check_failure(Witness), _))
    ;   true).

is_unit(Class) :-
    global_prefix_expand(sys:'Unit', Class).

is_enum(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, rdf:type, sys:'Enum').

is_tagged_union(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, rdf:type, sys:'TaggedUnion').

is_simple_class(_Validation_Object,Class) :-
    prefix_list(
        [
            sys:'Class',
            sys:'TaggedUnion',
            sys:'Enum',
            sys:'Unit'
        ], List),
    memberchk(Class,List),
    !.
is_simple_class(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class, rdf:type, C),
    is_simple_class(Validation_Object,C).

is_base_type(Type) :-
    prefix_list(
        [
            xsd:integer, % List all here...
            xsd:string,
            xsd:date,
            xsd:dateTime
        ],
        List),
    atom_string(Atom,Type),
    memberchk(Atom, List).

class_subsumed(_Validation_Object,Class,Class).
class_subsumed(Validation_Object,Class,Subsumed) :-
    class_super(Validation_Object,Class,Super),
    class_subsumed(Validation_Object,Super, Subsumed).

class_super(Validation_Object,Class,Super) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Class, sys:inherits, Super).
class_super(Validation_Object,Class,Super) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:inherits, Intermediate),
    class_super(Validation_Object,Intermediate,Super).

class_predicate_type(Validation_Object,Class,Predicate,Type) :-
    is_simple_class(Validation_Object,Class),
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class,Predicate,Range),
    \+ is_built_in(Predicate),
    do_or_die(
        \+ has_at(Predicate),
        throw(error(not_a_valid_keyword(Predicate)))),
    type_descriptor(Validation_Object,Range,Type).
class_predicate_type(Validation_Object,Class,Predicate,Type) :-
    class_super(Validation_Object,Class,Super),
    class_predicate_type(Validation_Object,Super,Predicate,Type).

refute_schema(Validation_Object,Witness) :-
    is_simple_class(Validation_Object,Class),
    refute_class_definition(Validation_Object,Class,Witness).
refute_schema(Validation_Object,Witness) :-
    is_circular_hasse_diagram(Validation_Object,Witness).

/* O(n^3)
 */
is_circular_hasse_diagram(Validation_Object,Witness) :-
    is_simple_class(Validation_Object, Class),
    transitive_sub_class_of(Validation_Object, Class, Subclass, Path),
    repeats(Path),
    Witness = witness{
                  '@type': cycle_in_class,
                  from_class : Class,
                  to_class : Subclass,
                  path : Path
              }.

sub_class_of(Validation_Object,Class,Subclass) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class,rdfs:subClassOf,Subclass).

repeats([X|T]) :-
    member(X,T),
    !.
repeats([_|T]) :-
    repeats(T).

/* Needs to check 'seen' classes so as not to loop infinitely */
transitive_sub_class_of(Validation_Object,Class, Subclass, [Subclass,Class]) :-
    sub_class_of(Validation_Object,Class,Subclass).
transitive_sub_class_of(Validation_Object,Class, Subsubclass, [Subclass,Class|Path]) :-
    sub_class_of(Validation_Object,Class,Subclass),
    transitive_sub_class_of(Validation_Object,Subclass, Subsubclass, Path).

refute_class_definition(Validation_Object,Class,Witness) :-
    refute_property(Validation_Object,Class, Witness).

% TODO: Add goal expansion magic here
is_built_in(P) :-
    prefix_list(
        [
            rdf:type,
            sys:comment,
            sys:inherits,
            sys:key,
            sys:base,
            sys:value,
            sys:class,
            sys:index
        ],
        List),
    memberchk(P,List).

type_family_constructor(Type) :-
    prefix_list(
        [
            sys:'Set',
            sys:'List',
            sys:'Array',
            sys:'Cardinality',
            sys:'Optional'
        ],
        List),
    memberchk(Type,List).

is_type_family(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class,rdf:type,Type_Constructor),
    type_family_constructor(Type_Constructor).

refute_property(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, rdf:type, sys:'Class'),
    xrdf(Schema, Class, P, Range),
    \+ is_built_in(P),
    (   is_type_family(Validation_Object, Range)
    ->  refute_type(Validation_Object, Range, Witness)
    ;   refute_class_or_base_type(Validation_Object, Range, Witness)
    ).

refute_class_or_base_type(Validation_Object,Class,Witness) :-
    refute_base_type(Class,_Witness1),
    refute_class(Validation_Object,Class,_Witness2),
    Witness = witness{
                 '@type': not_a_class_or_base_type,
                  class: Class
              }.

refute_base_type(Type,_Witness) :-
    is_base_type(Type),
    !,
    fail.
refute_base_type(Type,Witness) :-
    Witness = witness{ '@type': not_a_base_type,
                       type: Type }.

is_rdf_list(Validation_Object,Type) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:first, _Car),
    xrdf(Schema, Type, rdf:rest, _Cdr).

refute_list(_Validation_Object,rdf:nil,_Witness) :-
    !,
    fail.
refute_list(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    \+ xrdf(Schema, Type, rdf:first, _Car),
    Witness = witness{ '@type': list_has_no_first,
                       'type': Type }.
refute_list(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    \+ xrdf(Schema, Type, rdf:rest, _Cdr),
    Witness = witness{ '@type' : list_has_no_rest,
                       type: Type }.
refute_list(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:first, _Car),
    xrdf(Schema, Type, rdf:rest, Cdr),
    refute_list(Cdr, Witness).

refute_simple_class(Validation_Object,Class,_Witness) :-
    is_simple_class(Validation_Object,Class),
    !,
    fail.
refute_simple_class(_Validation_Object,Class,Witness) :-
    Witness = witness{ '@type' : class_not_defined,
                       class: Class }.

refute_class(Validation_Object,Class, Witness) :-
    is_type_family(Validation_Object,Class),
    !,
    refute_type(Validation_Object,Class,Witness).
refute_class(Validation_Object,Class,Witness) :-
    refute_simple_class(Validation_Object,Class,Witness).

rdf_list(_,P,[]) :-
    prefix_list([rdf:nil],List),
    memberchk(P,List),
    !.
rdf_list(Validation_Object,Cell,[First|Rest]) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Cell, rdf:first, First),
    xrdf(Schema, Cell, rdf:rest, Next_Cell),
    rdf_list(Validation_Object, Next_Cell,Rest).

refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Enum'),
    (   \+ xrdf(Schema, Type, sys:value, _),
        Witness = json{ '@type' : enum_has_no_elements,
                        type : Type }
    ;   xrdf(Schema, Type, sys:value, Cons),
        refute_list(Validation_Object,Cons,List_Witness),
        Witness = (List_Witness.put({'@type' : enum_has_bad_enumeration,
                                     list_error : (List_Witness.'@type')}))
    ).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'TaggedUnion'),
    xrdf(Schema, Type, P, Class),
    \+ is_built_in(P),
    refute_type(Validation_Object,Class,Class_Witness),
    (Witness.put({'@type' : tagged_union_has_bad_class,
                  class_error : (Class_Witness.'@type')
                 })).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Set'),
    \+  xrdf(Schema, Type, sys:class, _),
    Witness = json{ '@type' : set_has_no_class,
                    type : Type }.
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, sys:'List'),
    \+ xrdf(Schema, Type, sys:class, _),
    Witness = json{ '@type' : list_has_no_class,
                    type : Type }.
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Optional'),
    \+ xrdf(Schema, Type, sys:class, _),
    Witness = json{ '@type' : optional_has_no_class,
                    type : Type }.
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Array'),
    \+ xrdf(Schema, Type, sys:class, _),
    Witness = json{ '@type' : array_has_no_class,
                    type : Type }.
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Cardinality'),
    (   \+ xrdf(Schema, Type, sys:class, _)
    ->  Witness = json{ '@type' : cardinality_has_no_class,
                        type : Type }
    ;   \+ xrdf(Schema, Type, sys:cardinality, _)
    ->  Witness = json{ '@type' : cardinality_has_no_bound,
                        type : Type }
    ).

type_descriptor(_Validation_Object, Class, unit) :-
    is_unit(Class),
    !.
type_descriptor(Validation_Object, Class, tagged_union(Class,Map)) :-
    is_tagged_union(Validation_Object, Class),
    !,
    database_schema(Validation_Object,Schema),
    findall(P-C,
            (
                distinct(P,(
                             xrdf(Schema, Class, P, C),
                             \+ is_built_in(P)
                         ))
            ),
            Data),
    dict_create(Map,tagged_union,Data).
type_descriptor(Validation_Object, Class, enum(Class,List)) :-
    is_enum(Validation_Object,Class),
    !,
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:value, Cons),
    rdf_list(Validation_Object, Cons, List).
type_descriptor(_Validation_Object, Class, base_class(Class)) :-
    % Not sure these should be conflated...
    is_base_type(Class),
    !.
type_descriptor(Validation_Object, Class, class(Class)) :-
    % Not sure these should be conflated...
    is_simple_class(Validation_Object, Class),
    !.
type_descriptor(Validation_Object, Type, set(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Set'),
    !,
    xrdf(Schema, Type, sys:class, Class).
type_descriptor(Validation_Object, Type, list(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'List'),
    !,
    xrdf(Schema, Type, sys:class, Class).
type_descriptor(Validation_Object, Type, array(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Array'),
    !,
    xrdf(Schema, Type, sys:class, Class).
type_descriptor(Validation_Object, Type, card(Class,N)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Cardinality'),
    !,
    xrdf(Schema, Type, sys:class, Class),
    xrdf(Schema, Type, sys:cardinality, N^^xsd:positiveInteger).
type_descriptor(Validation_Object, Type, optional(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Optional'),
    !,
    xrdf(Schema, Type, sys:class, Class).

key_base(Validation_Object, Type, Base) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, sys:base, Base^^xsd:string),
    !.
key_base(Validation_Object, Type, Base) :-
    database_context(Validation_Object,Context),
    get_dict('@schema',Context,Schema),
    compress_uri(Type,'@base',Schema,Type_Compressed),
    atomic_list_concat([Type_Compressed,'_'],Base).

% should refactor to do key lookup once.
key_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type,sys:key,Obj),
    key_descriptor_(Validation_Object,Type,Obj,Descriptor).

key_descriptor_(Validation_Object, Type, Obj, lexical(Base,Fields)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Obj,rdf:type, sys:'Lexical'),
    xrdf(Schema, Obj, sys:fields, L),
    rdf_list(Validation_Object,L,Fields),
    key_base(Validation_Object,Type,Base).
key_descriptor_(Validation_Object, Type, Obj, hash(Base,Fields)) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj, rdf:type, sys:'Hash'),
    xrdf(Schema, Obj, sys:fields, L),
    rdf_list(Validation_Object,L,Fields),
    key_base(Validation_Object,Type,Base).
key_descriptor_(Validation_Object, Type, Obj, value_hash(Base)) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj, rdf:type, sys:'ValueHash'),
    key_base(Validation_Object,Type,Base).
key_descriptor_(Validation_Object, Type, Obj, random(Base)) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj, rdf:type, sys:'Random'),
    key_base(Validation_Object,Type,Base).

:- begin_tests(schema_checker).
:- use_module(core(util/test_utils)).

test(check_for_cycles_good, [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema(admin,test),
                 resolve_absolute_string_descriptor('admin/test',Desc)

             )),
         cleanup(
             teardown_temp_store(State)
         )
     ]) :-

    create_context(Desc,commit{
                            author : "me",
                            message : "none"}, Context),

    with_transaction(
        Context,
        (   % Schema
            ask(Context,
                (   insert(person, rdf:type, 'Class'),
                    insert(person, name, type:string),
                    insert(person, birthdate, type:date),
                    insert(person, friends, set_person),
                    insert(set_person, rdf:type, 'Set'),
                    insert(set_person, sys:class, person),
                    insert(employee, rdf:type, 'Class'),
                    insert(employee, rdfs:subClassOf, person),
                    insert(employee, employee_number, type:integer)
                )
               )
        ),
        _Meta).

test(check_for_cycles_bad, [
         setup(
             (   setup_temp_store(State),
                 create_db_with_empty_schema(admin,test),
                 resolve_absolute_string_descriptor('admin/test',Desc)

             )),
         cleanup(
             teardown_temp_store(State)
         )

     ]) :-

    create_context(Desc,commit{
                            author : "me",
                            message : "none"}, Context),

    with_transaction(
        Context,
        (   % Schema
            ask(Context,
                (
                    insert(person, rdf:type, 'Class'),
                    insert(person, sys:inheritsFrom, engineer),
                    insert(person, name, type:string),
                    insert(person, birthdate, type:date),
                    insert(person, friends, set_person),
                    insert(set_person, rdf:type, 'Set'),
                    insert(set_person, sys:class, person),

                    insert(employee, rdf:type, 'Class'),
                    insert(employee, sys:inheritsFrom, person),
                    insert(employee, employee_number, type:integer),

                    insert(engineer, rdf:type, 'Class'),
                    insert(engineer, sys:inheritsFrom, employee)
                )
               )
        ),
        _Meta_Data
    ).

%% TODO: Check for diamond properties!

:- end_tests(schema_checker).

