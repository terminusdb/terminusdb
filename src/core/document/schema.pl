:- module('document/schema', [
              refute_schema/2,
              is_enum/2,
              is_simple_class/2,
              is_base_type/1,
              is_built_in/1,
              is_list_type/1,
              is_array_type/1,
              refute_class/3,
              class_predicate_type/4,
              type_descriptor/3,
              class_subsumed/3,
              key_descriptor/3,
              documentation_descriptor/3,
              type_family_constructor/1,
              is_schemaless/1,
              class_subsumed/3,
              is_abstract/2,
              is_subdocument/2
          ]).

/*
 * Schema language for JSON as a graph
 *
 */

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(query), [has_at/1, compress_dict_uri/3]).

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

is_system_class(Class) :-
    prefix_list(
        [
            sys:'Class',
            sys:'TaggedUnion',
            sys:'Enum',
            sys:'Unit'
        ], List),
    memberchk(Class,List).

is_simple_class(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class, rdf:type, C),
    is_system_class(C).

is_base_type(Type) :-
    base_type(Type).

class_subsumed(_Validation_Object,Class,Class).
class_subsumed(Validation_Object,Class,Subsumed) :-
    class_super(Validation_Object,Class,Subsumed).

class_super(Validation_Object,Class,Super) :-
    subclass_of(Validation_Object, Class, Super).
class_super(Validation_Object,Class,Super) :-
    subclass_of(Validation_Object, Class, Intermediate),
    class_super(Validation_Object,Intermediate,Super).

class_predicate_type(Validation_Object,Class,Predicate,Type) :-
    is_simple_class(Validation_Object,Class),
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Class,Predicate,Range),
    \+ is_built_in(Predicate),
    do_or_die(
        \+ has_at(Predicate),
        throw(error(not_a_valid_keyword(Predicate), _))),
    type_descriptor(Validation_Object,Range,Type).
class_predicate_type(Validation_Object,Class,Predicate,Type) :-
    class_super(Validation_Object,Class,Super),
    class_predicate_type(Validation_Object,Super,Predicate,Type).

refute_schema_context_prefix(Schema, Prefix, Witness) :-
    \+ xrdf(Schema, Prefix, rdf:type, sys:'Prefix'),
    Witness = witness{
                  '@type': prefix_is_mistyped,
                  prefix: Prefix
              }.
refute_schema_context_prefix(Schema, Prefix, Witness) :-
    (   xrdf(Schema, Prefix, sys:prefix, Prefix_Name)
    ->  (   Prefix_Name = Prefix_String^^Type,
            Type = 'http://www.w3.org/2001/XMLSchema#string',
            string(Prefix_String)
        ->  fail
        ;   format(string(Content), "~q", [Prefix_Name]),
            Witness = witness{
                          '@type': prefix_name_malformed,
                          prefix: Prefix,
                          content: Content
                      })
    ;   Witness = witness{
                      '@type': prefix_name_missing,
                      prefix: Prefix
                  }).
refute_schema_context_prefix(Schema, Prefix, Witness) :-
    (   xrdf(Schema, Prefix, sys:url, Prefix_Url)
    ->  (   Prefix_Url = Prefix_Url_String^^Type,
            Type = 'http://www.w3.org/2001/XMLSchema#string',
            string(Prefix_Url_String)
        ->  fail
        ;   format(string(Content), "~q", [Prefix_Url]),
            Witness = witness{
                          '@type': prefix_name_malformed,
                          prefix: Prefix,
                          content: Content
                      })
    ;   Witness = witness{
                      '@type': prefix_url_missing,
                      prefix: Prefix
                  }).
refute_schema_context_prefix(Schema, Prefix, Witness) :-
    xrdf(Schema, Prefix, Property, _),
    prefix_list(
        [
            sys:prefix,
            sys:url,
            rdf:type
        ], List),
    \+ memberchk(Property, List),
    Witness = witness{
                  '@type': prefix_has_invalid_property,
                  prefix: Prefix,
                  property: Property
              }.

refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    \+ xrdf(Schema, 'terminusdb://context', rdf:type,sys:'Context'),
    Witness = witness{
                  '@type': context_not_found
              }.
refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    \+ xrdf(Schema, 'terminusdb://context', sys:base,_),
    Witness = witness{
                  '@type': context_has_no_base_prefix
              }.
refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    \+ xrdf(Schema, 'terminusdb://context', sys:schema,_),
    Witness = witness{
                  '@type': context_has_no_schema_prefix
              }.
refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, 'terminusdb://context', Property, _),
    prefix_list(
        [
            sys:base,
            sys:schema,
            sys:prefix_pair,
            rdf:type
        ], List),
    \+ memberchk(Property, List),
    Witness = witness{
                  '@type': context_has_invalid_property,
                  'property': Property
              }.
refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, 'terminusdb://context', sys:prefix_pair, Prefix),
    (   atom(Prefix)
    ->  refute_schema_context_prefix(Schema, Prefix, Witness)
    ;   Witness = witness{
                      '@type': context_has_malformed_prefix
                  }).

refute_schema(Validation_Object,Witness) :-
    is_circular_hasse_diagram(Validation_Object,Witness),
    % We should not proceed if we are circular!
    !.
refute_schema(Validation_Object, Witness) :-
    refute_schema_context(Validation_Object, Witness).
refute_schema(Validation_Object,Witness) :-
    is_simple_class(Validation_Object,Class),
    (   refute_class_definition(Validation_Object,Class,Witness)
    ;   refute_class_documentation(Validation_Object,Class,Witness)
    ;   refute_class_key(Validation_Object,Class,Witness)
    ;   refute_class_meta(Validation_Object,Class,Witness)
    ;   refute_diamond_property(Validation_Object,Class,Witness)).

/* O(n^3)
 */
is_circular_hasse_diagram(Validation_Object,Witness) :-
    is_simple_class(Validation_Object, Class),
    transitive_subclass_of(Validation_Object, Class, Subclass, Path),
    repeats(Path),
    Witness = witness{
                  '@type': cycle_in_class,
                  from_class : Class,
                  to_class : Subclass,
                  path : Path
              }.

subclass_of(Validation_Object,Subclass,Class) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema,Subclass,sys:inherits,Class).

repeats([X|T]) :-
    member(X,T),
    !.
repeats([_|T]) :-
    repeats(T).

/* Needs to check 'seen' classes so as not to loop infinitely */
transitive_subclass_of(Validation_Object, Subclass, Class, [Subclass,Class]) :-
    subclass_of(Validation_Object, Subclass,Class).
transitive_subclass_of(Validation_Object, Subclass, Super, [Subclass|Path]) :-
    subclass_of(Validation_Object, Subclass, Class),
    transitive_subclass_of(Validation_Object, Class, Super, Path).

refute_class_definition(Validation_Object,Class,Witness) :-
    refute_property(Validation_Object,Class, Witness).

% TODO: Add goal expansion magic here
is_built_in(P) :-
    prefix_list(
        [
            rdf:type,
            sys:comment,
            sys:properties,
            sys:documentation,
            sys:inherits,
            sys:key,
            sys:base,
            sys:value,
            sys:class,
            sys:index,
            sys:abstract,
            sys:subdocument
        ],
        List),
    memberchk(P,List).

is_abstract(Validation_Object, C) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, C, sys:abstract, rdf:nil).

is_direct_subdocument(Validation_Object, C) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, C, sys:subdocument, rdf:nil).

is_subdocument(Validation_Object, C) :-
    class_subsumed(Validation_Object, C, D),
    is_direct_subdocument(Validation_Object, D).

is_list_type(C) :-
    global_prefix_expand(rdf:'List', C).

is_array_type(C) :-
    global_prefix_expand(sys:'Array', C).

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

refute_class_documentation(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:documentation, Doc),
    \+ xrdf(Schema, Doc, rdf:type, sys:'Documentation'),
    Witness = witness{'@type' : not_a_valid_documenation_object,
                      class: Class,
                      subject: Doc }.
refute_class_documentation(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:documentation, Doc),
    xrdf(Schema, Doc, rdf:type, sys:'Documentation'),
    xrdf(Schema, Doc, Prop, _),
    prefix_list([sys:comment, sys:properties, rdf:type], List),
    (   \+ memberchk(Prop, List)
    ->  Witness = witness{ '@type' : not_a_valid_documenation_object,
                           predicate: Prop,
                           class: Class,
                           subject: Doc }
    ;   xrdf(Schema, Doc, sys:properties, Prop_Obj),
        xrdf(Schema, Prop_Obj, Key, Result),
        \+ global_prefix_expand(rdf:type, Key),
        global_prefix_expand(xsd:string, XSD),
        (   Result \= _^^XSD
        ->  Witness = witness{ '@type' : not_a_valid_property_documentation_object,
                               predicate: Key,
                               class: Class,
                               subject: Prop_Obj }
        ;   \+ class_predicate_type(Validation_Object,Class,Key,_),
            Witness = witness{ '@type' : invalid_property_in_property_documentation_object,
                               predicate: Key,
                               class: Class,
                               subject: Prop_Obj }
        )
    ).

refute_class_key(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:key, Key_Obj),
    refute_key_obj(Validation_Object,Class,Key_Obj, Witness).

refute_key_obj(Validation_Object,Class,Obj,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj, rdf:type, Type),
    prefix_list([sys:'Lexical', sys:'Hash', sys:'ValueHash', sys:'Random'], List),
    \+ memberchk(Type, List),
    Witness = witness{ '@type' : bad_key_type,
                       class: Class,
                       key: Obj,
                       type: Type }.
refute_key_obj(Validation_Object,Class,Obj,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj, rdf:type, Type),
    prefix_list([sys:'Lexical', sys:'Hash'], List),
    memberchk(Type, List),
    (   xrdf(Schema, Obj, sys:fields, List_Obj)
    ->  (   rdf_list(Validation_Object,List_Obj,Fields)
        ->  member(Field,Fields),
            \+ class_predicate_type(Validation_Object, Class, Field, _),
            Witness = witness{ '@type' : key_field_does_not_exist,
                               field: Field,
                               class: Class,
                               key: Obj,
                               type: Type }
        ;   Witness = witness{ '@type' : fields_not_a_valid_list_in_key,
                               class: Class,
                               key: Obj,
                               type: Type })
    ;   Witness = witness{ '@type' : no_fields_in_key,
                           class: Class,
                           key: Obj,
                           type: Type }
    ).

refute_class_meta(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:abstract, Result),
    global_prefix_expand(rdf:nil, RDF_Nil),
    Result \= RDF_Nil,
    Witness = witness{ '@type' : bad_abstract_value,
                       class: Class,
                       value: Result }.
refute_class_meta(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:subdocument, Result),
    global_prefix_expand(rdf:nil, RDF_Nil),
    Result \= RDF_Nil,
    Witness = witness{ '@type' : bad_subdocument_value,
                       class: Class,
                       value: Result }.

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
    xrdf(Schema, Type, rdf:type, rdf:'List').

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
    xrdf(Schema, Type, sys:cardinality, N^^xsd:nonNegativeInteger).
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
    get_dict('@schema', Context, Schema),
    put_dict(_{'@base' : Schema}, Context, New_Context),
    compress_dict_uri(Type,New_Context,Type_Compressed),
    atomic_list_concat([Type_Compressed,'_'],Base).

% should refactor to do key lookup once.
key_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, sys:key, Obj),
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

is_schemaless(Validation_Object) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, 'terminusdb://data/Schema', rdf:type, rdf:nil).

documentation_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, sys:documentation, Obj),
    documentation_descriptor_(Validation_Object,Obj,Descriptor).

documentation_property_obj(Validation_Object, Obj, Properties) :-
    database_schema(Validation_Object, Schema),
    findall(Key-Value,
            (   xrdf(Schema, Obj, sys:properties, Property),
                xrdf(Schema, Property, Key, Value^^xsd:string)),
            Pairs),
    dict_pairs(Properties,json,Pairs).

documentation_descriptor_(Validation_Object, Obj, documentation(Comment,Property_Obj)) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Obj, sys:comment, Comment^^xsd:string),
    documentation_property_obj(Validation_Object,Obj,Property_Obj).

refute_diamond_property(Validation_Object, Class, Witness) :-
    catch(
        (   class_frame(Validation_Object, Class, _),
            fail
        ),
        error(violation_of_diamond_property(Class,Predicate),_),
        Witness = witness{'@type':violation_of_diamond_property,
                          predicate: Predicate,
                          class: Class}
    ).
