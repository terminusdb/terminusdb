:- module('document/schema', [
              refute_schema/2,
              is_simple_class/2,
              is_base_type/1,
              is_built_in/1,
              refute_class/3,
              class_predicate_type/4,
              type_descriptor/3,
              class_subsumed/3,
              key_descriptor/3
          ]).

/*
 * Schema language for JSON as a graph
 *
 */

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
%:- use_module(core(query)).

check_schema :-
    (   schema_not_wf(Witness)
    ->  throw(error(schema_check_failure(Witness), _))
    ;   true).

is_unit(Class) :-
    prefix_list([sys:'Unit'], List),
    memberchk(Class,List).

is_enum(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class, rdf:type, sys:'Enum').

is_tagged_union(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, rdf:type, sys:'TaggedUnion').

is_simple_class(Validation_Object,Class) :-
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
            type:integer, % List all here...
            type:string,
            type:date,
            type:dateTime
        ],
        List),

    memberchk(Type, List).

class_subsumed(Validation_Object,Class,Class).
class_subsumed(Validation_Object,Class,Subsumed) :-
    class_super(Validation_Object,Class,Super),
    class_subsumed(Validation_Object,Super, Subsumed).

class_super(Validation_Object,Class,Super) :-
    database_schema(Validation_Object,Schema),
    xrdf(Class, rdfs:subClassOf, Super).
class_super(Validation_Object,Class,Super) :-
    database_schema(Validation_Object,Schema),
    xrdf(Class, rdfs:subClassOf, Intermediate),
    class_super(Validation_Object,Intermediate,Super).

class_predicate_type(Validation_Object,Class,Predicate,Type) :-
    is_simple_class(Validation_Object,Class),

    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class,Predicate,Range),
    \+ is_built_in(Predicate),
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
    xrdf(Validation_Object,Class,rdfs:subClassOf,Subclass).

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
            rdfs:comment,
            rdfs:label,
            rdfs:subClassOf,
            sys:key,
            sys:base,
            sys:value,
            sys:class,
            sys:index
        ],
        List),
    memberchk(P,List).

is_type_family(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class,rdf:type,Type),

    prefix_list(
        [
            sys:'Set',
            sys:'List',
            sys:'Array',
            sys:'Cardinality',
            sys:'Optional',
            sys:'Enum',
            sys:'TaggedUnion'
        ],
        List),
    memberchk(Type,List).

refute_property(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Validation_Object, Class, rdf:type, sys:'Class'),
    xrdf(Validation_Object, Class, P, Range),
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

rdf_list(Validation_Object,rdf:nil,[]).
rdf_list(Validation_Object,Cell,[First|Rest]) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Cell, rdf:first, First),
    xrdf(Schema, Cell, rdf:rest, Next_Cell),
    rdf_list(Validation_Object, Next_Cell,Rest).

refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Validation_Object, Type, rdf:type, sys:'Enum'),
    (   \+ xrdf(Validation_Object, Type, sys:value, _),
        Witness = json{ '@type' : enum_has_no_elements,
                        type : Type }
    ;   xrdf(Validation_Object, Type, sys:value, Cons),
        refute_list(Validation_Object,Cons,List_Witness),
        Witness = (List_Witness.put({'@type' : enum_has_bad_enumeration,
                                     list_error : (List_Witness.'@type')}))
    ).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, sys:'TaggedUnion'),
    xrdf(Schema, Type, P, Class),
    \+ is_built_in(Validation_Object,P),
    refute_type(Validation_Object,Class,Class_Witness),
    (Witness.put({'@type' : tagged_union_has_bad_class,
                  class_error : (Class_Witness.'@type')
                 })).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Type, rdf:type, sys:'Set'),
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
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, 'Optional'),
    \+ xrdf(Schema, Type, sys:class, _),
    Witness = json{ '@type' : optional_has_no_class,
                    type : Type }.
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, 'Array'),
    \+ xrdf(Schema, Type, sys:class, _),
    Witness = json{ '@type' : array_has_no_class,
                    type : Type }.
refute_type(Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, 'Cardinality'),
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
    is_enum(Class),
    !,
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:value, Cons),
    rdf_list(Validation_Object, Cons, List).
type_descriptor(Validation_Object, Class, base_class(Class)) :-
    % Not sure these should be conflated...
    is_base_type(Validation_Object, Class),
    !.
type_descriptor(Validation_Object, Class, class(Class)) :-
    % Not sure these should be conflated...
    is_simple_class(Validation_Object, Class),
    !.
type_descriptor(Validation_Object, Type, set(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, 'Set'),
    !,
    xrdf(Schema, Type, sys:class, Class).
type_descriptor(Type, list(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, 'List'),
    !,
    xrdf(Schema, Type, sys:class, Class).
type_descriptor(Type, array(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, 'Array'),
    !,
    xrdf(Schema, Type, sys:class, Class).
type_descriptor(Type, card(Class,N)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, 'Cardinality'),
    !,
    xrdf(Schema, Type, sys:class, Class),
    xrdf(Schema, Type, sys:cardinality, N^^type:positiveInteger).
type_descriptor(Type, optional(Class)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, 'Optional'),
    !,
    xrdf(Schema, Type, sys:class, Class).

key_base(Validation_Object, Type, Base) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type,sys:base,Base^^type:string),
    !.
key_base(Validation_Object, _, '').

% should refactor to do key lookup once.
key_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type,sys:key,Obj),
    key_descriptor_(Type,Obj,Descriptor).

key_descriptor_(Validation_Object, Type, Obj, lexical(Base,Fields)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Obj,rdf:type,sys:lexical),
    xrdf(Schema, Obj,sys:fields,L),
    rdf_list(L,Fields),
    key_base(Validation_Object,Type,Base).
key_descriptor_(Validation_Object, Type, Obj, hash(Base,Fields)) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj,rdf:type,sys:hash),
    xrdf(Schema, Obj,sys:fields,L),
    rdf_list(L,Fields),
    key_base(Validation_Object,Type,Base).
key_descriptor_(Validation_Object, Type, Obj, value_hash(Base)) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj,rdf:type,sys:value_hash),
    key_base(Validation_Object,Type,Base).
key_descriptor_(Validation_Object, Type, Obj, random(Base)) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj,rdf:type,sys:random),
    key_base(Validation_Object,Type,Base).

/*
:- begin_tests(schema_checker).

test(check_for_cycles_good, [
         setup(
             (   delete_database,

                 % Schema
                 insert_triple(s(person, rdf:type, 'Class')),
                 insert_triple(s(person, name, type:string)),
                 insert_triple(s(person, birthdate, type:date)),
                 insert_triple(s(person, friends, set_person)),
                 insert_triple(s(set_person, rdf:type, 'Set')),
                 insert_triple(s(set_person, sys:class, person)),

                 insert_triple(s(employee, rdf:type, 'Class')),
                 insert_triple(s(employee, rdfs:subClassOf, person)),
                 insert_triple(s(employee, employee_number, type:integer)),

                 stage

             )),
         cleanup(
             delete_database
         )
     ]) :-

    check_schema.

test(check_for_cycles_bad, [
         setup(
             (   delete_database,

                 insert_triple(s(person, rdf:type, 'Class')),
                 insert_triple(s(person, rdfs:subClassOf, engineer)),
                 insert_triple(s(person, name, type:string)),
                 insert_triple(s(person, birthdate, type:date)),
                 insert_triple(s(person, friends, set_person)),
                 insert_triple(s(set_person, rdf:type, 'Set')),
                 insert_triple(s(set_person, sys:class, person)),

                 insert_triple(s(employee, rdf:type, 'Class')),
                 insert_triple(s(employee, rdfs:subClassOf, person)),
                 insert_triple(s(employee, employee_number, type:integer)),

                 insert_triple(s(engineer, rdf:type, 'Class')),
                 insert_triple(s(engineer, rdfs:subClassOf, employee)),

                 stage

             )),
         cleanup(
             delete_database
         ),
         error(
             schema_check_failure(
                 witness{
                     '@type':cycle_in_class,
                     from_class:person,
                     path:[engineer,person,employee,engineer],
                     to_class:employee
                 }
             ),_Ctx)
     ]) :-

    check_schema.

%% TODO: Check for diamond properties!

:- end_tests(schema_checker).
*/
