:- module('document/schema', [
              graph_member_list/3,
              graph_member_array/3,
              is_system_class/1,
              refute_schema/2,
              is_enum/2,
              is_simple_class/2,
              is_tagged_union/2,
              is_base_type/1,
              is_built_in/1,
              is_list_type/1,
              is_array_type/1,
              is_key/1,
              is_documentation/1,
              refute_class/3,
              class_predicate_type/4,
              class_predicate_conjunctive_type/4,
              type_descriptor/3,
              schema_type_descriptor/3,
              class_subsumed/3,
              key_descriptor/3,
              key_descriptor/4,
              schema_key_descriptor/4,
              documentation_descriptor/3,
              schema_documentation_descriptor/3,
              metadata_descriptor/3,
              schema_metadata_descriptor/3,
              oneof_descriptor/3,
              schema_oneof_descriptor/3,
              type_family_constructor/1,
              is_schemaless/1,
              drop_schemaless_mode/1,
              concrete_subclass/3,
              is_abstract/2,
              is_subdocument/2,
              schema_is_subdocument/2,
              schema_class_predicate_conjunctive_type/4
          ]).

/*
 * Schema language for JSON as a graph
 *
 */

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(triple/literals)).
:- use_module(core(query)).

:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

% performance
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(json). % This feels very circular.
:- use_module(instance). % This is most definitely circular.
:- use_module(json_rdf, [graph_get_json_object/3]).

graph_member_list(Instance, O,L) :-
    xrdf(Instance, L, rdf:first, O).
graph_member_list(Instance, O,L) :-
    xrdf(Instance, L, rdf:rest, Cdr),
    graph_member_list(Instance,O,Cdr).

graph_member_array(Instance, O, A) :-
    xrdf(Instance, A, rdf:value, O).

is_unit(Class) :-
    global_prefix_expand(sys:'Unit', Class).

is_enum(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    is_schema_enum(Schema, Class).

is_choice(Validation_Object, Class) :-
    database_schema(Validation_Object,Schema),
    is_schema_choice(Schema, Class).

:- table is_schema_enum/2 as private.
is_schema_enum(Schema, Class) :-
    xrdf(Schema, Class, rdf:type, sys:'Enum').

:- table is_schema_choice/2 as private.
is_schema_choice(Schema, Class) :-
    xrdf(Schema, Class, rdf:type, sys:'Choice').

is_foreign(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    is_schema_foreign(Schema, Class).

:- table is_schema_foreign/2 as private.
is_schema_foreign(Schema, Class) :-
    xrdf(Schema, Class, rdf:type, sys:'Foreign').

is_tagged_union(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    is_schema_tagged_union(Schema, Class).

:- table is_schema_tagged_union/2 as private.
is_schema_tagged_union(Schema, Class) :-
    xrdf(Schema, Class, rdf:type, sys:'TaggedUnion').

is_system_class(Class) :-
    prefix_list(
        [
            sys:'Foreign',
            sys:'Class',
            sys:'TaggedUnion',
            sys:'Enum',
            sys:'Unit',
            sys:'JSON',
            sys:'JSONDocument'
        ], List),
    memberchk(Class,List).

is_simple_class(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    is_schema_simple_class(Schema, Class).

is_json_class(Validation_Object,Class) :-
    database_schema(Validation_Object,Schema),
    is_schema_json_class(Schema,Class).

is_schema_json_class(Schema,Class) :-
    global_prefix_expand(sys:'JSON',JSON),
    xrdf(Schema,Class,rdf:type,JSON).

% NOTE
% This generator is no longer stable under ordering!
:- table is_schema_simple_class/2 as private.
is_schema_simple_class(Schema, Class) :-
    xrdf(Schema,Class, rdf:type, C),
    is_system_class(C).

is_base_type(Type) :-
    base_type(Type).

concrete_subclass(Validation_Object,Class,Concrete) :-
    class_super(Validation_Object,Concrete,Class),
    \+ is_abstract(Validation_Object,Concrete).

class_subsumed(Validation_Object,Class,Super) :-
    database_schema(Validation_Object,Schema),
    schema_class_subsumed(Schema,Class,Super).

schema_class_subsumed(_Schema,Class,Class).
schema_class_subsumed(Schema,Class,Super) :-
    schema_class_super(Schema,Class,Super).

class_super(Validation_Object,Class,Super) :-
    database_schema(Validation_Object,Schema),
    schema_class_super(Schema,Class,Super).

schema_class_super(Schema,Class,Super) :-
    schema_subclass_of(Schema, Class, Super).
schema_class_super(Schema,Class,Super) :-
    schema_subclass_of(Schema, Class, Intermediate),
    schema_class_super(Schema,Intermediate,Super).

class_predicate_conjunctive_type(Validation_Object,Class,Predicate,Type) :-
    database_schema(Validation_Object,Schema),
    schema_class_predicate_conjunctive_type(Schema, Class, Predicate, Type).

:- table schema_class_predicate_conjunctive_type/4 as private.
schema_class_predicate_conjunctive_type(Schema,Class,Predicate,Type) :-
    schema_class_predicate_conjunctive_type_step(Schema,Class,Predicate,Type).
schema_class_predicate_conjunctive_type(Schema,Class,Predicate,Type) :-
    schema_class_super(Schema,Class,Super),
    schema_class_predicate_conjunctive_type(Schema,Super,Predicate,Type).

schema_class_predicate_conjunctive_type_step(Schema,Class,Predicate,Type) :-
    \+ is_schema_tagged_union(Schema,Class),
    is_schema_simple_class(Schema,Class),
    xrdf(Schema,Class,Predicate,Range),
    \+ is_built_in(Predicate),
    do_or_die(
        \+ has_at(Predicate),
        throw(error(not_a_valid_keyword(Predicate), _))),
    schema_type_descriptor(Schema,Range,Type).

class_predicate_oneof(Validation_Object,Class,Predicate,Type) :-
    database_schema(Validation_Object,Schema),
    schema_class_predicate_oneof(Schema, Class, Predicate, Type).

:- table schema_class_predicate_oneof/4 as private.
schema_class_predicate_oneof(Schema,Class,Predicate,Type) :-
    schema_class_predicate_oneof_step(Schema,Class,Predicate,Type).
schema_class_predicate_oneof(Schema,Class,Predicate,Type) :-
    schema_class_super(Schema,Class,Super),
    schema_class_predicate_oneof(Schema,Super,Predicate,Type).

schema_class_predicate_oneof_step(Schema,Class,Predicate,Type) :-
    is_schema_tagged_union(Schema,Class),
    xrdf(Schema,Class,Predicate,Range),
    \+ is_built_in(Predicate),
    do_or_die(
        \+ has_at(Predicate),
        throw(error(not_a_valid_keyword(Predicate), _))),
    schema_type_descriptor(Schema,Range,Type).
schema_class_predicate_oneof_step(Schema,Class,Predicate,Type) :-
    is_schema_simple_class(Schema,Class),
    xrdf(Schema,Class,sys:oneOf,R),
    xrdf(Schema,R,Predicate,Range),
    schema_type_descriptor(Schema,Range,Type).

class_predicate_type(Validation_Object,Class,Predicate,Type) :-
    database_schema(Validation_Object,Schema),
    schema_class_predicate_type(Schema, Class, Predicate, Type).

:- table schema_class_predicate_type/4 as private.
schema_class_predicate_type(Schema,Class,Predicate,Type) :-
    schema_class_predicate_oneof_step(Schema,Class,Predicate,Type).
schema_class_predicate_type(Schema,Class,Predicate,Type) :-
    schema_class_predicate_conjunctive_type_step(Schema,Class,Predicate,Type).
schema_class_predicate_type(Schema,Class,Predicate,Type) :-
    schema_class_super(Schema,Class,Super),
    schema_class_predicate_type(Schema,Super,Predicate,Type).

refute_schema_context_documentation(Validation_Object, Documentation, Witness) :-
    database_schema(Validation_Object,Schema),
    \+ xrdf(Schema, Documentation, rdf:type, sys:'SchemaDocumentation'),
    Witness = witness{
                  '@type': documentation_is_mistyped,
                  documentation: Documentation
              }.
refute_schema_context_documentation(Validation_Object, Documentation, Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Documentation, sys:authors, Author_ID),
    global_prefix_expand(xsd:string,XSD_String),
    (   rdf_list(Validation_Object, Author_ID, List)
    ->  \+ maplist([_^^XSD_String]>>true, List),
        Witness = witness{
                      '@type': documentation_authors_malformed,
                      documentation: Documentation
                  }
    ;   Witness = witness{
                      '@type': documentation_authors_malformed_list,
                      documentation: Documentation
                  }
    ).
refute_schema_context_documentation(Validation_Object, Documentation, Witness) :-
    database_schema(Validation_Object,Schema),
    global_prefix_expand(xsd:string,XSD_String),
    \+ xrdf(Schema, Documentation, sys:title, _Title^^XSD_String),
    Witness = witness{
                      '@type': documentation_title_malformed,
                      documentation: Documentation
              }.
refute_schema_context_documentation(Validation_Object, Documentation, Witness) :-
    database_schema(Validation_Object,Schema),
    global_prefix_expand(xsd:string,XSD_String),
    \+ xrdf(Schema, Documentation, sys:description, _Desc^^XSD_String),
    Witness = witness{
                  '@type': documentation_description_malformed,
                  documentation: Documentation
              }.

refute_schema_context_prefix(Schema, Prefix, Witness) :-
    \+ xrdf(Schema, Prefix, rdf:type, sys:'Prefix'),
    Witness = witness{
                  '@type': prefix_is_mistyped,
                  prefix: Prefix
              }.
refute_schema_context_prefix(Schema, Prefix, Witness) :-
    (   xrdf(Schema, Prefix, sys:prefix, Prefix_Name)
    ->  (   literal_to_string(Prefix_Name, _)
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
    ->  (   literal_to_string(Prefix_Url, _)
        ->  fail
        ;   format(string(Content), "~q", [Prefix_Url]),
            Witness = witness{
                          '@type': prefix_url_malformed,
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

refute_schema_context_system_prefix(Schema, Prefix_Name, Witness) :-
    (   xrdf(Schema, 'terminusdb://context', Prefix_Name, Prefix_Value)
    ->  (   literal_to_string(Prefix_Value, Prefix_Value_String)
        ->  (   uri_has_protocol(Prefix_Value_String)
            ->  fail
            ;   Witness = witness{
                              '@type': context_system_prefix_is_not_a_uri,
                              prefix_name: Prefix_Name,
                              prefix_value: Prefix_Value_String
                          }
            )
        ;   term_string(Prefix_Value, Prefix_Value_String),
            Witness = witness{
                          '@type': context_system_prefix_is_not_a_string,
                          prefix_name: Prefix_Name,
                          prefix_value: Prefix_Value_String
                      }
        )
    ;   Witness = witness{
                      '@type': context_missing_system_prefix,
                      prefix_name: Prefix_Name
                  }
    ).

refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    \+ xrdf(Schema, 'terminusdb://context', rdf:type,sys:'Context'),
    Witness = witness{
                  '@type': context_not_found
              }.
refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    (   global_prefix_expand(sys:base, Prefix_Name)
    ;   global_prefix_expand(sys:schema, Prefix_Name)),
    refute_schema_context_system_prefix(Schema, Prefix_Name, Witness).
refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, 'terminusdb://context', Property, _),
    prefix_list(
        [
            sys:base,
            sys:schema,
            sys:prefix_pair,
            sys:documentation,
            sys:metadata,
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
refute_schema_context(Validation_Object, Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, 'terminusdb://context', sys:documentation, Documentation),
    (   atom(Documentation)
    ->  refute_schema_context_documentation(Validation_Object, Documentation, Witness)
    ;   Witness = witness{
                      '@type': context_has_malformed_documentation
                  }
    ).

refute_schema(Validation_Object,Witness) :-
    is_circular_hasse_diagram(Validation_Object,Witness),
    % We should not proceed if we are circular!
    !.
refute_schema(Validation_Object, Witness) :-
    refute_schema_context(Validation_Object, Witness).
refute_schema(Validation_Object,Witness) :-
    database_prefixes(Validation_Object, Prefixes),
    is_simple_class(Validation_Object,Class),
    \+ is_json_class(Validation_Object, Class),
    (   refute_class_definition(Validation_Object,Class,Witness)
    ;   refute_unfoldable_cycle(Validation_Object,Class,Witness)
    ;   refute_class_inherits(Validation_Object,Class,Witness)
    ;   refute_class_documentation(Validation_Object,Class,Witness)
    ;   refute_class_key(Validation_Object,Class,Witness)
    ;   refute_class_meta(Validation_Object,Class,Witness)
    ;   refute_class_oneof(Validation_Object,Class,Witness)
    ;   refute_diamond_property(Validation_Object,Prefixes,Class,Witness)).

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

reachable_unfoldable(Schema,A,P,B) :-
    distinct(B,
             (   schema_class_subsumed(Schema,A,C),
                 schema_class_predicate_type(Schema,C,P,class(B)),
                 schema_is_unfoldable(Schema,B)
             )).

refute_unfoldable_cycle(DB, Original, Witness) :-
    database_schema(DB,Schema),
    schema_is_unfoldable(Schema, Original),
    State = state([]),
    unfoldable_property_cycle(Schema, Original, Original, [], Path, State),
    Witness = witness{
                  '@type' : property_path_cycle_detected,
                  path : Path,
                  class : Original
              }.

unfoldable_property_cycle(Schema, Original, A, Path, New_Path, State) :-
    reachable_unfoldable(Schema, A, P, C),
    arg(1,State,Set),
    (   schema_class_subsumed(Schema, Original, C)
    ->  reverse([C,P|Path],New_Path)
    ;   member(C, Set)
    ->  fail
    ;   nb_setarg(1,State,[C|Set]),
        unfoldable_property_cycle(Schema, Original, C, [C,P|Path], New_Path, State)
    ).

refute_class_inherits(DB,Class,Witness) :-
    database_schema(DB,Schema),
    xrdf(Schema,Class,sys:inherits,Super),
    (   \+ xrdf(Schema, Super, rdf:type, _Type)
    ->  Witness =
        witness{
            '@type': inherits_from_non_existent_class,
            class: Class,
            super: Super
        }
    ;   xrdf(Schema, Super, rdf:type, Type),
        global_prefix_expand(sys:'Class', Class_Type),
        global_prefix_expand(sys:'TaggedUnion', Tagged_Type),
        \+ member(Type, [Class_Type,Tagged_Type])
    ->  Witness =
        witness{
            '@type': inherits_from_invalid_super_class,
            class: Class,
            super: Super
        }
    ).

subclass_of(Validation_Object,Subclass,Class) :-
    database_schema(Validation_Object,Schema),
    schema_subclass_of(Schema,Subclass,Class).

schema_subclass_of(Schema,Subclass,Class) :-
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
            sys:oneOf,
            sys:base,
            sys:class,
            sys:abstract,
            sys:subdocument,
            sys:unfoldable
        ],
        List),
    memberchk(P,List).

is_abstract(Validation_Object, C) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, C, sys:abstract, rdf:nil).

is_direct_subdocument(Schema, C) :-
    xrdf(Schema, C, sys:subdocument, rdf:nil).

is_subdocument(Validation_Object, C) :-
    database_schema(Validation_Object,Schema),
    schema_is_subdocument(Schema, C).

:- table schema_is_subdocument/2 as private.
schema_is_subdocument(Schema, C) :-
    schema_class_subsumed(Schema, C, D),
    is_direct_subdocument(Schema, D).

is_direct_unfoldable(Schema, C) :-
    xrdf(Schema, C, sys:unfoldable, rdf:nil).

is_unfoldable(Validation_Object, C) :-
    database_schema(Validation_Object,Schema),
    schema_is_unfoldable(Schema, C).

:- table schema_is_unfoldable/2 as private.
schema_is_unfoldable(Schema, C) :-
    schema_class_subsumed(Schema, C, D),
    is_direct_unfoldable(Schema, D).

is_list_type(C) :-
    global_prefix_expand(rdf:'List', C).

is_array_type(C) :-
    global_prefix_expand(sys:'Array', C).

is_table_type(C) :-
    global_prefix_expand(rdf:'Table', C).

type_family_constructor(Type) :-
    prefix_list(
        [
            sys:'Set',
            sys:'List',
            sys:'Array',
            sys:'Table',
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
    refute_documentation_object(Validation_Object,Class,Doc,Witness).

refute_documentation_object(Validation_Object,Class,Doc,Witness) :-
    database_schema(Validation_Object,Schema),
    \+ xrdf(Schema, Doc, rdf:type, sys:'Documentation'),
    Witness = witness{ '@type' : not_a_typed_documentation_object,
                       class: Class,
                       object : Doc }.
refute_documentation_object(Validation_Object,Class,Doc,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Doc, Prop, _),
    prefix_list([sys:comment, sys:label, sys:properties, sys:values, sys:language, rdf:type], List),
    (   \+ memberchk(Prop, List)
    ->  Witness = witness{ '@type' : not_a_valid_documentation_object,
                           predicate: Prop,
                           class: Class,
                           subject: Doc }
    ;   is_enum(Validation_Object,Class)
    ->  (   xrdf(Schema, Doc, sys:properties, Val_Obj)
        ->  Witness = witness{ '@type' : enum_has_no_properties,
                               class: Class,
                               subject: Val_Obj }
        ;   xrdf(Schema, Doc, sys:values, Val_Obj)
        ->  xrdf(Schema, Val_Obj, Key, Result),
            \+ global_prefix_expand(rdf:type, Key),
            (   refute_documentation_value(Schema,enum,Class,Result,Witness)
            ->  true
            ;   xrdf(Schema, Class, sys:value, Cons),
                \+ graph_member_list(Schema, Key, Cons)
            ->   Witness = witness{ '@type' : invalid_enum_in_documentation_object,
                                    value: Key,
                                    class: Class,
                                    subject: Val_Obj }
            )
        )
    ;   xrdf(Schema, Doc, sys:properties, Prop_Obj),
        xrdf(Schema, Prop_Obj, Key, Result),
        \+ global_prefix_expand(rdf:type, Key),
        (   refute_documentation_value(Schema,property,Class,Result,Witness)
        ->  true
        ;   \+ class_predicate_type(Validation_Object,Class,Key,_),
            Witness = witness{ '@type' : invalid_property_in_property_documentation_object,
                               predicate: Key,
                               class: Class,
                               subject: Prop_Obj }
        )
    ).

documentation_error_selector(enum,not_a_valid_enum_documentation_object).
documentation_error_selector(property,not_a_valid_property_documentation_object).

refute_documentation_value(Schema,Type,Class,Result,Witness) :-
    documentation_error_selector(Type,Error),
    global_prefix_expand(xsd:string, XSD),
    \+ Result = _^^XSD,
    prefix_list([sys:comment, sys:label, rdf:type], DocList),
    xrdf(Schema, Result, Predicate, Value),
    (   \+ member(Predicate, DocList)
    ->  Witness = witness{ '@type' : Error,
                           predicate: Predicate,
                           class: Class,
                           subject: Value }
    ;   global_prefix_expand(rdf:type, Predicate)
    ->  \+ global_prefix_expand(sys:'DocumentationLabelComment', Value),
        Witness = witness{
                      '@type' : Error,
                      predicate : Predicate,
                      class: Class,
                      subject: Value
                  }
    ;   \+ Value = _^^XSD
    ->  Witness = witness{ '@type' : Error,
                           predicate: Predicate,
                           class: Class,
                           subject: Value }
    ).

is_key(Type) :-
    prefix_list([sys:'Lexical', sys:'Hash', sys:'ValueHash', sys:'Random'], List),
    memberchk(Type, List).

is_documentation(Type) :-
    prefix_list([sys:'SchemaDocumentation', sys:'PropertyDocumentation', sys:'Documentation'], List),
    memberchk(Type, List).

refute_class_key(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf_added(Schema, Class, sys:key, Key_Obj),
    (   refute_key_obj(Validation_Object,Class,Key_Obj, Witness)
    ->  true
    ;   refute_existing_object_keys(Validation_Object,Class,Witness)).

refute_key_obj(Validation_Object,Class,Obj,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Obj, rdf:type, Type),
    \+ is_key(Type),
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
refute_class_meta(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:unfoldable, Result),
    global_prefix_expand(rdf:nil, RDF_Nil),
    Result \= RDF_Nil,
    Witness = witness{ '@type' : bad_unfoldable_value,
                       class: Class,
                       value: Result }.

refute_class_oneof(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, sys:oneOf, Choice),
    (   is_schema_choice(Schema, Choice)
    ->  refute_property_(Validation_Object, Choice, Witness)
    ;   Witness = witness{ '@type': bad_choice_type,
                           class: Class,
                           choice: Choice }
    ).

refute_property(Validation_Object,Class,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, rdf:type, sys:'Class'),
    refute_property_(Validation_Object, Class, Witness).

refute_property_(Validation_Object, Class, Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Class, P, Range),
    \+ is_built_in(P),
    (   is_type_family(Validation_Object, Range)
    ->  refute_type(Validation_Object, Range, Witness)
    ;   refute_class_or_base_type(Validation_Object, Range, Witness)
    ).

refute_class_or_base_type(Validation_Object,Class,Witness) :-
    \+ is_base_type(Class),
    \+ is_unit(Class),
    refute_class(Validation_Object,Class,_Witness2),
    Witness = witness{
                 '@type': not_a_class_or_base_type,
                  class: Class
              }.

is_array(Validation_Object,Type) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, sys:'Array').

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
    refute_list(Validation_Object, Cdr, Witness).

refute_table(_Validation_Object,rdf:nil,_Witness) :-
    !,
    fail.
refute_table(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    \+ xrdf(Schema, Type, rdf:first, _Car),
    Witness = witness{ '@type': table_has_no_first,
                       'type': Type }.
refute_table(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:first, Car),
    refute_list(Validation_Object,Car, _),
    Witness = witness{ '@type': table_has_a_non_list,
                       'type': Type }.
refute_table(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    \+ xrdf(Schema, Type, rdf:rest, _Cdr),
    Witness = witness{ '@type' : table_has_no_rest,
                       type: Type }.
refute_table(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:first, _Car),
    xrdf(Schema, Type, rdf:rest, Cdr),
    refute_table(Validation_Object, Cdr, Witness).

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

rdf_list(Validation_Object,Cell,List) :-
    database_schema(Validation_Object,Schema),
    schema_rdf_list(Schema, Cell, List).

schema_rdf_list(_,P,[]) :-
    prefix_list([rdf:nil],List),
    memberchk(P,List),
    !.
schema_rdf_list(Schema,Cell,[First|Rest]) :-
    xrdf(Schema, Cell, rdf:first, First),
    xrdf(Schema, Cell, rdf:rest, Next_Cell),
    schema_rdf_list(Schema, Next_Cell,Rest).

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
    (   xrdf(Schema, Type, sys:class, Class)
    ->  refute_class_or_base_type(Validation_Object, Class, Witness)
    ;   Witness = json{ '@type' : set_has_no_class,
                        type : Type }
    ).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, sys:'List'),
    (   xrdf(Schema, Type, sys:class, Class)
    ->  refute_class_or_base_type(Validation_Object, Class, Witness)
    ;   Witness = json{ '@type' : list_has_no_class,
                        type : Type }
    ).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object,Schema),
    xrdf(Schema, Type, rdf:type, sys:'Table'),
    (   xrdf(Schema, Type, sys:class, Class)
    ->  refute_class_or_base_type(Validation_Object, Class, Witness)
    ;   Witness = json{ '@type' : table_has_no_class,
                        type : Type }
    ).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Optional'),
    (   xrdf(Schema, Type, sys:class, Class)
    ->  refute_class_or_base_type(Validation_Object, Class, Witness)
    ;   Witness = json{ '@type' : optional_has_no_class,
                        type : Type }
    ).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Array'),
    (   xrdf(Schema, Type, sys:class, Class)
    ->  refute_class_or_base_type(Validation_Object, Class, Witness)
    ;   Witness = json{ '@type' : array_has_no_class,
                        type : Type }
    ).
refute_type(Validation_Object,Type,Witness) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, Type, rdf:type, sys:'Cardinality'),
    (   xrdf(Schema, Type, sys:class, Class)
    ->  refute_class_or_base_type(Validation_Object, Class, Witness)
    ;   Witness = json{ '@type' : cardinality_has_no_class,
                        type : Type }
    ;   \+ xrdf(Schema, Type, sys:min_cardinality, _),
        \+ xrdf(Schema, Type, sys:max_cardinality, _)
    ->  Witness = json{ '@type' : cardinality_has_no_bound,
                        type : Type }
    ).

type_descriptor(Validation_Object, Class, Descriptor) :-
    database_schema(Validation_Object, Schema),
    schema_type_descriptor(Schema, Class, Descriptor).

:- table schema_type_descriptor/3 as private.
schema_type_descriptor(_Schema, Class, unit) :-
    is_unit(Class),
    !.
schema_type_descriptor(Schema, Class, foreign(Class)) :-
    is_schema_foreign(Schema,Class),
    !.
schema_type_descriptor(Schema, Class, enum(Class,List)) :-
    is_schema_enum(Schema,Class),
    !,
    xrdf(Schema, Class, sys:value, Cons),
    schema_rdf_list(Schema, Cons, List).
schema_type_descriptor(_Schema, Class, base_class(Class)) :-
    % Not sure these should be conflated...
    is_base_type(Class),
    !.
schema_type_descriptor(Schema, Class, class(Class)) :-
    % Not sure these should be conflated...
    is_schema_simple_class(Schema, Class),
    !.
schema_type_descriptor(Schema, Type, set(Class)) :-
    xrdf(Schema, Type, rdf:type, sys:'Set'),
    !,
    xrdf(Schema, Type, sys:class, Class).
schema_type_descriptor(Schema, Type, list(Class)) :-
    xrdf(Schema, Type, rdf:type, sys:'List'),
    !,
    xrdf(Schema, Type, sys:class, Class).
schema_type_descriptor(Schema, Type, table(Class)) :-
    xrdf(Schema, Type, rdf:type, sys:'Table'),
    !,
    xrdf(Schema, Type, sys:class, Class).
schema_type_descriptor(Schema, Type, array(Class,Dimensions)) :-
    xrdf(Schema, Type, rdf:type, sys:'Array'),
    !,
    xrdf(Schema, Type, sys:class, Class),
    (   xrdf(Schema, Type, sys:dimensions, Dimensions^^xsd:nonNegativeInteger)
    ->  true
    ;   Dimensions = 1).
schema_type_descriptor(Schema, Type, cardinality(Class,N,M)) :-
    xrdf(Schema, Type, rdf:type, sys:'Cardinality'),
    !,
    xrdf(Schema, Type, sys:class, Class),
    (   xrdf(Schema, Type, sys:min_cardinality, N^^xsd:nonNegativeInteger)
    ->  true
    ;   N = 0
    ),
    (   xrdf(Schema, Type, sys:max_cardinality, M^^xsd:nonNegativeInteger)
    ->  true
    ;   M = inf
    ).
schema_type_descriptor(Schema, Type, optional(Class)) :-
    xrdf(Schema, Type, rdf:type, sys:'Optional'),
    !,
    xrdf(Schema, Type, sys:class, Class).

key_base(Validation_Object, Context, Type, Base) :-
    database_schema(Validation_Object, Schema),
    schema_key_base(Schema, Context, Type, Base).

schema_key_base(Schema, _, Type, Base) :-
    xrdf(Schema, Type, sys:base, Base^^xsd:string),
    !.
schema_key_base(_Schema, Context, Type, Base) :-
    get_dict('@schema', Context, Schema),
    put_dict(_{'@base' : Schema}, Context, New_Context),
    compress_dict_uri(Type,New_Context,Type_Compressed),
    atomic_list_concat([Type_Compressed,'/'],Base).

key_descriptor(Validation_Object, Type, Descriptor) :-
    database_prefixes(Validation_Object, Prefixes),
    key_descriptor(Validation_Object, Prefixes, Type, Descriptor).

key_descriptor(Validation_Object, Prefixes, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    schema_key_descriptor(Schema, Prefixes, Type, Descriptor).

:- table schema_key_descriptor/4 as private.
schema_key_descriptor(Schema, Prefixes, Type, Descriptor) :-
    xrdf(Schema, Type, sys:key, Obj),
    schema_key_descriptor_(Schema,Prefixes,Type,Obj,Descriptor),
    !.
schema_key_descriptor(Schema, Prefixes, Type, base(Base)) :-
    schema_key_base(Schema,Prefixes,Type,Base).

schema_key_descriptor_(Schema, Prefixes, Type, Obj, lexical(Base,Fields)) :-
    xrdf(Schema, Obj,rdf:type, sys:'Lexical'),
    xrdf(Schema, Obj, sys:fields, L),
    schema_rdf_list(Schema,L,Fields),
    schema_key_base(Schema,Prefixes,Type,Base).
schema_key_descriptor_(Schema, Prefixes, Type, Obj, hash(Base,Fields)) :-
    xrdf(Schema, Obj, rdf:type, sys:'Hash'),
    xrdf(Schema, Obj, sys:fields, L),
    schema_rdf_list(Schema,L,Fields),
    schema_key_base(Schema,Prefixes,Type,Base).
schema_key_descriptor_(Schema, Prefixes, Type, Obj, value_hash(Base)) :-
    xrdf(Schema, Obj, rdf:type, sys:'ValueHash'),
    schema_key_base(Schema,Prefixes,Type,Base).
schema_key_descriptor_(Schema, Prefixes, Type, Obj, random(Base)) :-
    xrdf(Schema, Obj, rdf:type, sys:'Random'),
    schema_key_base(Schema,Prefixes,Type,Base).

is_schemaless(Validation_Object) :-
    database_schema(Validation_Object, Schema),
    xrdf(Schema, 'terminusdb://data/Schema', rdf:type, rdf:nil).

drop_schemaless_mode(Transaction) :-
   (   is_schemaless(Transaction)
   ->  database_schema(Transaction, [Schema]),
       delete(Schema, 'terminusdb://data/Schema', rdf:type, rdf:nil, _)
   ;   true).

documentation_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    schema_documentation_descriptor(Schema, Type, Descriptor).

schema_documentation_descriptor(Schema, Type, enum_documentation(Type,Records)) :-
    is_schema_enum(Schema,Type),
    !,
    findall(Record,
            (   xrdf(Schema, Type, sys:documentation, Obj),
                Record0 = json{},
                (   xrdf(Schema, Obj, sys:language, Lang^^xsd:language)
                ->  Record1 = (Record0.put('@language', Lang))
                ;   Record1 = Record0),
                (   xrdf(Schema, Obj, sys:comment, Comment^^xsd:string)
                ->  Record2 = (Record1.put('@comment', Comment))
                ;   Record2 = Record1),
                (   xrdf(Schema, Obj, sys:label, Label^^xsd:string)
                ->  Record3 = (Record2.put('@label', Label))
                ;   Record3 = Record2
                ),
                findall(Key-Value,
                        (  xrdf(Schema, Obj, sys:values, Enum),
                           xrdf(Schema, Enum, Key, Enum_Obj),
                           \+ global_prefix_expand(rdf:type, Key),
                           (   Enum_Obj = Value^^_
                           ->  true
                           ;   V = json{},
                               (   xrdf(Schema, Enum_Obj, sys:label, EnumLabel^^xsd:string)
                               ->  put_dict(_{ '@label' : EnumLabel}, V, V1)
                               ;   V = V1
                               ),
                               (   xrdf(Schema, Enum_Obj, sys:comment, EnumComment^^xsd:string)
                               ->  put_dict(_{ '@comment' : EnumComment}, V1, Value)
                               ;   Value = V1
                               )
                           )
                        ),
                        Pairs),
                dict_pairs(Elements,json,Pairs),
                (   Elements = _{}
                ->  Record = Record3
                ;   Record = (Record3.put('@values', Elements))
                ),
                Record \= _{}
            ),
            Records).
schema_documentation_descriptor(Schema, Type, property_documentation(Merged)) :-
    is_schema_simple_class(Schema,Type),
    findall(Record,
            (   schema_class_subsumed(Schema,Type,Super),
                xrdf(Schema, Super, sys:documentation, Obj),
                Record0 = json{},
                (   Type = Super
                ->  (   xrdf(Schema, Obj, sys:language, Lang^^xsd:language)
                    ->  Record1 = (Record0.put('@language', Lang))
                    ;   Record1 = Record0),
                    (   xrdf(Schema, Obj, sys:comment, Comment^^xsd:string)
                    ->  Record2 = (Record1.put('@comment', Comment))
                    ;   Record2 = Record1),
                    (   xrdf(Schema, Obj, sys:label, Label^^xsd:string)
                    ->  Record3 = (Record2.put('@label', Label))
                    ;   Record3 = Record2)
                ;   (   xrdf(Schema, Obj, sys:language, Lang^^xsd:language)
                    ->  Record3 = (Record0.put('@language', Lang))
                    ;   Record3 = Record0)
                ),
                findall(Key-Value,
                        (   xrdf(Schema, Obj, sys:properties, Property),
                            xrdf(Schema, Property, Key, Prop_Obj),
                            \+ global_prefix_expand(rdf:type, Key),
                            (   Prop_Obj = Value^^_
                            ->  true
                            ;   V = json{},
                                (   xrdf(Schema, Prop_Obj, sys:label, PropertyLabel^^xsd:string)
                                ->  put_dict(_{ '@label' : PropertyLabel}, V, V1)
                                ;   V = V1
                                ),
                                (   xrdf(Schema, Prop_Obj, sys:comment, PropertyComment^^xsd:string)
                                ->  put_dict(_{ '@comment' : PropertyComment}, V1, Value)
                                ;   Value = V1
                                )
                            )
                        ),
                        Pairs),
                dict_pairs(Elements,json,Pairs),
                (   Elements = _{}
                ->  Record = Record3
                ;   Record = (Record3.put('@properties', Elements))
                ),
                Record \= _{}
            ),
            Records),
    merge_documentation_language_records(Records,Merged).

merge_documentation_language_records([],[]).
merge_documentation_language_records([Record],[Record]) :-
    !.
merge_documentation_language_records([Record|Rest],[MergedRecord|Merged]) :-
    get_dict('@language', Record, Lang),
    partition({Lang}/[R]>>get_dict('@language',R,Lang),Rest,Same,Different),
    foldl([R1,R2,Result]>>(
              (   get_dict('@properties', R2, Properties2)
              ->  (   get_dict('@properties', R1, Properties1)
                  ->  put_dict(Properties2, Properties1, Properties),
                      put_dict(_{'@properties' : Properties}, R2, Result)
                  ;   put_dict(_{'@properties' : Properties2}, R2, Result)
                  )
              ;   R1 = R2
              )
          ),Same,Record,MergedRecord),
    merge_documentation_language_records(Different,Merged).

metadata_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    schema_metadata_descriptor(Schema, Type, Descriptor).

schema_metadata_descriptor(Schema, Type, metadata(JSON)) :-
    xrdf(Schema, Type, sys:metadata, Metadata),
    graph_get_json_object(Schema, Metadata, JSON).

schema_oneof_descriptor(Schema, Class, tagged_union(Class, Map)) :-
    is_schema_tagged_union(Schema, Class),
    !,
    findall(P-Desc,
            (
                distinct(P,(
                             xrdf(Schema, Class, P, C),
                             \+ is_built_in(P),
                             schema_type_descriptor(Schema, C, Desc)
                         ))
            ),
            Data),
    dict_create(Map,tagged_union,Data).
schema_oneof_descriptor(Schema, Type, tagged_union(Type, Map)) :-
    xrdf(Schema, Type, sys:oneOf, Class),
    findall(P-Desc,
            (
                distinct(P,(
                             xrdf(Schema, Class, P, C),
                             \+ is_built_in(P),
                             schema_type_descriptor(Schema, C, Desc)
                         ))
            ),
            Data),
    dict_create(Map,tagged_union,Data).

oneof_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    schema_oneof_descriptor(Schema, Type, Descriptor).
oneof_descriptor(Validation_Object, Type, Descriptor) :-
    database_schema(Validation_Object, Schema),
    schema_subclass_of(Schema,Type,Super),
    schema_oneof_descriptor(Schema, Super, Descriptor).

refute_diamond_property(Validation_Object, Prefixes, Class, Witness) :-
    catch(
        (   class_property_dictionary(Validation_Object, Prefixes, Class, _),
            fail
        ),
        error(violation_of_diamond_property(Class,Predicate),_),
        Witness = witness{'@type':violation_of_diamond_property,
                          predicate: Predicate,
                          class: Class}
    ).
