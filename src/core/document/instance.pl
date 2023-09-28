:- module('document/instance', [
              refute_instance/2,
              refute_instance_schema/2,
              refute_existing_object_keys/3,
              refute_referential_integrity/2,
              is_instance/3,
              is_instance_class/3,
              instance_of/3
          ]).

/*
 * Introconversion between JSON-LD and a schema language.
 *
 */

% performance
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).

:- use_module(core(util)).
:- use_module(core(util/xsd_parser)).
:- use_module(core(triple)).
:- use_module(core(query)).
:- use_module(library(terminus_store)).
:- use_module(core(transaction)).
:- use_module(json_rdf).

:- use_module(schema).

:- use_module(library(http/json)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(dicts)).
:- use_module(library(solution_sequences)).
:- use_module(library(uri), [uri_components/2]).

is_rdf_list_(_Instance, Type) :-
    global_prefix_expand(rdf:nil, Type),
    !.
is_rdf_list_(Instance, Type) :-
    xrdf(Instance, Type, rdf:first, _Car),
    xrdf(Instance, Type, rdf:rest, Cdr),
    is_rdf_list_(Instance, Cdr).

is_rdf_list(Validation_Object, Type) :-
    database_instance(Validation_Object, Instance),
    is_rdf_list_(Instance, Type).

is_instance(_Validation_Object, Literal, T) :-
    nonvar(Literal),
    Literal = _^^S,
    basetype_subsumption_of(S,T),
    !.
is_instance(_Validation_Object, Literal, T) :-
    nonvar(Literal),
    Literal = _@_,
    global_prefix_expand(rdf:langString,S),
    basetype_subsumption_of(S,T),
    !.
is_instance(_, Literal, _) :-
    nonvar(Literal),
    (   Literal = _^^_
    ;   Literal = _@_),
    !,
    fail.
% NOTE: Need a clause here for enumerated types!
is_instance(Validation_Object, X, C) :-
    ground(C),
    is_enum(Validation_Object, C),
    !,
    % This would be faster with set or even array!
    % probably also faster to use the database!
    database_schema(Validation_Object, Schema),
    xrdf(Schema, C, sys:value, Cons),
    graph_member_list(Schema, X, Cons).
is_instance(Validation_Object, X, C) :-
    is_instance_class(Validation_Object, X, C).

is_instance_class(Validation_Object, X, C) :-
    nonvar(X),
    ground(C),
    !,
    database_instance(Validation_Object, Instance),
    is_simple_class(Validation_Object, C),
    class_subsumed(Validation_Object, Class, C),
    xrdf(Instance, X, rdf:type, Class).
is_instance_class(Validation_Object, X, C) :-
    database_instance(Validation_Object, Instance),
    xrdf(Instance, X, rdf:type, Class),
    is_simple_class(Validation_Object, Class),
    class_subsumed(Validation_Object, Class,C).


instance_of(Validation_Object, X, C) :-
    database_instance(Validation_Object, Instance),
    xrdf(Instance, X,rdf:type,C).

foreign_instance_of(Validation_Object, X, C) :-
    database_instance(Validation_Object, Instance),
    xrdf(Instance, X,sys:foreign_type,C).

array_object(Validation_Object, S,I,O) :-
    database_instance(Validation_Object, Instance),
    xrdf(Instance, S,rdf:type,sys:'Array_Object'),
    % cardinality one
    findall(t(S,index,I),
            xrdf(Instance, S,sys:index,I),
            [t(S,index,I)]),
    % cardinality one
    findall(t(S,object,O),
            xrdf(Instance, S,sys:object,O),
            [t(S,object,O)]).

member_list(Validation_Object, O, L) :-
    database_instance(Validation_Object, Instance),
    graph_member_list(Instance, O, L).

member_array(Validation_Object, O, A) :-
    database_instance(Validation_Object, Instance),
    graph_member_array(Instance, O, A).

card_count(Validation_Object,S_Id,P_Id,N) :-
    % choose as existential anything free
    instance_layer(Validation_Object, Layer),
    (   integer(S_Id),
        integer(P_Id)
    ->  sp_card(Layer,S_Id,P_Id,N)
    % If no triples, or either P or S is missing from the dictionary then it is empty.
    ;   N = 0
    ).

test_cardinality_not_one(Validation_Object, Class, S, P, Witness) :-
    \+ card_count(Validation_Object, S,P,1),
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject_String, S),
    (   atom(P)
    ->  P = Predicate
    ;   terminus_store:predicate_id(Layer, Predicate_String, P),
        atom_string(Predicate, Predicate_String)
    ),
    atom_string(Subject, Subject_String),
    Witness = witness{ '@type': instance_not_cardinality_one,
                       instance: Subject,
                       class: Class,
                       predicate: Predicate
                     }.

refute_cardinality_(unit,Validation_Object,S,P,Witness) :-
    global_prefix_expand(sys:'Unit', Sys_Unit),
    test_cardinality_not_one(Validation_Object, Sys_Unit, S, P, Witness).

refute_cardinality_(class(C),Validation_Object,S,P,Witness) :-
    test_cardinality_not_one(Validation_Object, C, S, P, Witness).
refute_cardinality_(base_class(C),Validation_Object,S,P,Witness) :-
    test_cardinality_not_one(Validation_Object, C, S, P, Witness).
refute_cardinality_(enum(C,_),Validation_Object, S,P,Witness) :-
    test_cardinality_not_one(Validation_Object, C, S, P, Witness).
refute_cardinality_(tagged_union(C,_),Validation_Object,S,P,Witness) :-
    test_cardinality_not_one(Validation_Object, C, S, P, Witness).
refute_cardinality_(not_tagged_union(C,_),Validation_Object,S,P,Witness) :-
    \+ card_count(Validation_Object,S,P,0),
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject_String, S),
    atom_string(Subject, Subject_String),
    (   atom(P)
    ->  P = Predicate
    ;   terminus_store:predicate_id(Layer, Predicate_String, P),
        atom_string(Predicate, Predicate_String)
    ),
    Witness = witness{ '@type': forbidden_oneof_property_present,
                       instance: Subject,
                       class: C,
                       predicate: Predicate
                     }.
refute_cardinality_(set(_C),_Validation_Object,_S,_P,_Witness) :-
    % no bad cardinality possible
    fail.
refute_cardinality_(array(_C,_D),_Validation_Object,_S,_P,_Witness) :-
    % a property whose value is an array
    % No bad cardinality possible - absence means empty array
    % But we should check the cardinality of elements!
    fail.
refute_cardinality_(array,Validation_Object,S,P,Witness) :-
    % a property inside an array element
    \+ card_count(Validation_Object,S,P,1),
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject_String, S),
    atom_string(Subject, Subject_String),
    (   atom(P)
    ->  P = Predicate
    ;   terminus_store:predicate_id(Layer, Predicate_String, P),
        atom_string(Predicate, Predicate_String)
    ),
    Witness = witness{ '@type': array_predicate_not_cardinality_one,
                       instance: Subject,
                       predicate: Predicate
                     }.
refute_cardinality_(list(C),Validation_Object,S,P,Witness) :-
    % a property whose value is a list
    \+ card_count(Validation_Object,S,P,1),
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject_String, S),
    atom_string(Subject, Subject_String),
    (   atom(P)
    ->  P = Predicate
    ;   terminus_store:predicate_id(Layer, Predicate_String, P),
        atom_string(Predicate, Predicate_String)
    ),
    Witness = witness{ '@type': instance_not_cardinality_one,
                       instance: Subject,
                       class: C,
                       predicate: Predicate
                     }.
refute_cardinality_(list,Validation_Object,S,P,Witness) :-
    % a property inside a list cell
    \+ card_count(Validation_Object,S,P,1),
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject_String, S),
    atom_string(Subject, Subject_String),
    (   atom(P)
    ->  P = Predicate
    ;   terminus_store:predicate_id(Layer, Predicate_String, P),
        atom_string(Predicate, Predicate_String)
    ),
    Witness = witness{ '@type': list_predicate_not_cardinality_one,
                       instance: Subject,
                       predicate: Predicate
                     }.
refute_cardinality_(optional(C),Validation_Object,S,P,Witness) :-
    card_count(Validation_Object,S,P,N),
    (   \+ memberchk(N, [0,1])
    ->  instance_layer(Validation_Object, Layer),
        terminus_store:subject_id(Layer, Subject_String, S),
        atom_string(Subject, Subject_String),
        (   atom(P)
        ->  P = Predicate
        ;   terminus_store:predicate_id(Layer, Predicate_String, P),
            atom_string(Predicate, Predicate_String)
        ),
        range_term_list(Validation_Object,Subject,Predicate,L),
        Witness = witness{ '@type': instance_has_wrong_cardinality,
                           class: C,
                           instance: Subject,
                           predicate: Predicate,
                           cardinality: N,
                           object_list: L
                         }
    ).
refute_cardinality_(cardinality(C,N,M),Validation_Object,S,P,Witness) :-
    card_count(Validation_Object,S,P,Count),
    \+ (   N =< Count,
           Count =< M
       ),
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject, S),
    (   atom(P)
    ->  P = Predicate
    ;   terminus_store:predicate_id(Layer, Predicate_String, P),
        atom_string(Predicate, Predicate_String)
    ),
    range_term_list(Validation_Object,Subject,Predicate,L),
    Witness = witness{ '@type': instance_has_wrong_cardinality,
                       class: C,
                       instance: Subject,
                       object_list: L,
                       predicate: Predicate,
                       cardinality: Count
                     }.

internal_simple_json(X^^_, X) :-
    (   string(X)
    ;   atom(X)
    ;   number(X)
    ),
    !.
internal_simple_json(X, X) :-
    atom(X),
    !.
internal_simple_json(D^^T, X) :-
    typecast(D^^T, 'http://www.w3.org/2001/XMLSchema#string', [], X).

range_term_list(Validation_Object, S, P, L) :-
    database_instance(Validation_Object, Instance),
    findall(J,
            (   xrdf(Instance, S,P,O),
                internal_simple_json(O, J)
            ),
            L).

refute_cardinality(Validation_Object,S_Id,P_Id,C,Witness) :-
    is_array_type(C),
    !,
    refute_cardinality_(array, Validation_Object, S_Id, P_Id, Witness).
refute_cardinality(Validation_Object,S_Id,P_Id,C,Witness) :-
    is_list_type(C),
    !,
    refute_cardinality_(list, Validation_Object, S_Id, P_Id, Witness).
refute_cardinality(Validation_Object,S_Id,P_Id,C,Witness) :-
    oneof_descriptor(Validation_Object, C, tagged_union(TU,TC)),
    instance_layer(Validation_Object, Layer),
    (   integer(P_Id)
    ->  terminus_store:predicate_id(Layer, Predicate, P_Id)
    ;   Predicate = P_Id
    ),
    class_predicate_type(Validation_Object,C,Predicate,_),
    atom_string(P,Predicate),
    dict_keys(TC,Keys),
    member(P,Keys),
    !,
    (   refute_cardinality_(tagged_union(TU,TC),Validation_Object,S_Id,P_Id,Witness)
    ;   member(Q,Keys),
        % If it isn't in the dictionary, it isn't in the object and we're done...
        terminus_store:predicate_id(Layer, Q, Q_Id),
        P_Id \= Q_Id,
        refute_cardinality_(not_tagged_union(TU,TC),Validation_Object,S_Id,Q_Id,Witness)
    ).
refute_cardinality(Validation_Object,S_Id,P_Id,C,Witness) :-
    instance_layer(Validation_Object, Layer),
    (   integer(P_Id)
    ->  terminus_store:predicate_id(Layer, Predicate, P_Id)
    ;   Predicate = P_Id
    ),
    class_predicate_conjunctive_type(Validation_Object, C, Predicate, Desc),
    refute_cardinality_(Desc,Validation_Object,S_Id,P_Id,Witness).

refute_cardinality_new(Validation_Object,S_Id,C,Witness) :-
    instance_layer(Validation_Object, Layer),
    class_predicate_conjunctive_type(Validation_Object, C, Predicate, _),
    (   terminus_store:predicate_id(Layer, Predicate, P_Id)
    ->  \+ terminus_store:id_triple_addition(Layer, S_Id, P_Id, _) % unchecking
    ;   P_Id = Predicate % doesn't exist in the dictionary
    ),
    refute_cardinality(Validation_Object, S_Id, P_Id, C, Witness).
refute_cardinality_new(Validation_Object,S_Id,C,Witness) :-
    instance_layer(Validation_Object, Layer),
    oneof_descriptor(Validation_Object, C, tagged_union(_TU,TC)),
    dict_keys(TC,Keys),
    (   member(Predicate,Keys),
        terminus_store:predicate_id(Layer, Predicate, P_Id),
        terminus_store:id_triple_addition(Layer, S_Id, P_Id, _)
    % We only need one right answer
    ->  fail
    ;   terminus_store:subject_id(Layer, Subject, S_Id),
        Witness = witness{ '@type': no_choice_is_cardinality_one,
                           instance: Subject,
                           class: C,
                           choices: Keys
                         }
    ).


refute_built_in_value(Validation_Object, rdf:type,O,Witness) :-
    refute_class(Validation_Object, O,Witness).
refute_built_in_value(_Validation_Object, rdfs:comment,O@L,Witness) :-
    \+ (string(O),
        atom(L)),
    format(atom(Atom), '~q@~q', [O,L]),
    Witness = witness{
                  '@type': comment_not_valid,
                  comment: Atom
              }.
refute_built_in_value(_Validation_Object, rdfs:label,O@L,Witness) :-
    \+ (string(O),
        atom(L)),
    format(atom(Atom), '~q@~q', [O,L]),
    Witness = witness{
                  '@type': comment_not_valid,
                  comment: Atom
              }.

subject_changed(Validation_Object, S_Id) :-
    instance_layer(Validation_Object, Layer),
    distinct(S_Id,(   terminus_store:id_triple_removal(Layer, S_Id,_,_)
                  ;   terminus_store:id_triple_addition(Layer, S_Id,_,_))).

subject_inserted(Validation_Object, S_Id) :-
    instance_layer(Validation_Object, Layer),
    terminus_store:id_triple_addition(Layer, S_Id,_,_),
    \+ terminus_store:id_triple_removal(Layer, S_Id,_,_),
    !.

subject_updated(Validation_Object, S_Id) :-
    instance_layer(Validation_Object, Layer),
    distinct(S_Id,(terminus_store:id_triple_removal(Layer, S_Id,_,_),
                   terminus_store:id_triple_addition(Layer, S_Id,_,_))).

subject_deleted(Validation_Object, S_Id) :-
    instance_layer(Validation_Object, Layer),
    global_prefix_expand(rdf:type, Rdf_Type),
    terminus_store:predicate_id(Layer, Rdf_Type, Rdf_Type_Id),
    terminus_store:id_triple_removal(Layer, S_Id, Rdf_Type_Id, _).


subject_predicate_changed(Validation_Object, S_Id, P_Id) :-
    instance_layer(Validation_Object, Layer),
    distinct(S_Id-P_Id,(   terminus_store:id_triple_removal(Layer, S_Id, P_Id,_)
                       ;   terminus_store:id_triple_addition(Layer, S_Id ,P_Id,_))).

subject_predicate_updated(Validation_Object, S_Id, P_Id) :-
    instance_layer(Validation_Object, Layer),
    distinct(S_Id-P_Id,(terminus_store:id_triple_removal(Layer, S_Id, P_Id,_),
                        terminus_store:id_triple_addition(Layer, S_Id ,P_Id,_))).

refute_key(Validation_Object, S_Id,P_Id,Class,Witness) :-
    key_descriptor(Validation_Object, Class,Desc),
    subject_predicate_updated(Validation_Object,S_Id,P_Id),
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject_String, S_Id),
    atom_string(Subject, Subject_String),
    terminus_store:predicate_id(Layer, Predicate_String, P_Id),
    atom_string(Predicate, Predicate_String),
    refute_key_(Desc,Subject,Predicate,Witness).

refute_key_(lexical(_,Fields),Subject,Predicate,Witness) :-
    member(Predicate,Fields),
    Witness = json{ '@type' : lexical_key_changed,
                    subject: Subject,
                    predicate: Predicate}.
refute_key_(value_hash(_),Subject,Predicate,Witness) :-
    Witness = json{ '@type' : value_key_changed,
                    subject: Subject,
                    predicate: Predicate}.
refute_key_(hash(_,Fields),Subject,Predicate,Witness) :-
    member(Predicate,Fields),
    Witness = json{ '@type' : hash_key_changed,
                    subject: Subject,
                    predicate: Predicate}.

instance_layer(Validation_Object, Layer) :-
    database_instance(Validation_Object, Instance),
    member(G, Instance),
    read_write_obj_reader(G, Layer).

schema_layer(Validation_Object, Layer) :-
    database_schema(Validation_Object, Schema),
    member(G, Schema),
    read_write_obj_reader(G, Layer).

refute_existing_object_keys(Validation_Object,Class,Witness) :-
    % this is just wrong
    key_descriptor(Validation_Object, Class,Desc),
    instance_layer(Validation_Object, Layer),
    global_prefix_expand(rdf:type, Rdf_Type),
    terminus_store:predicate_id(Layer, Rdf_Type, Rdf_Type_Id),
    terminus_store:object_id(Layer, node(Class), Class_Id),
    distinct(
        S_Id,
        terminus_store:id_triple(Layer, S_Id,Rdf_Type_Id,Class_Id)
    ),

    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject_String, S_Id),

    memberchk(Desc, [lexical(_,_), hash(_,_), value_hash]),
    'document/json':get_document(Validation_Object, Subject_String, Document),
    catch(
        'document/json':json_elaborate(Validation_Object, Document, _),
        error(submitted_id_does_not_match_generated_id(Submitted,Generated),_),
        Witness = json{ '@type' : key_change_invalid,
                        id: Submitted,
                        generated_id: Generated
                      }
    ),
    (   var(Witness)
    ->  fail
    ;   true).

refute_subject_deletion(Validation_Object, S_Id, Witness) :-
    subject_deleted(Validation_Object, S_Id),
    refute_subject_deletion_(Validation_Object, S_Id, Witness).

refute_subject_deletion_(Validation_Object, S_Id,Witness) :-
    instance_layer(Validation_Object, Layer),
    (   terminus_store:id_triple(Layer,S_Id,P_Id,O_Id),
        terminus_store:subject_id(Layer, Subject_String, S_Id),
        terminus_store:predicate_id(Layer, Predicate_String, P_Id),
        terminus_store:object_id(Layer, node(Object_String), O_Id),
        atom_string(Subject, Subject_String),
        atom_string(Predicate, Predicate_String),
        atom_string(Object, Object_String),
        Witness = json{ '@type' : entire_object_not_deleted,
                        subject : Subject,
                        predicate : Predicate,
                        object : Object }
    ;   terminus_store:id_triple(Layer,OS_Id,P_Id,S_Id),
        terminus_store:subject_id(Layer, Other_Subject_String, OS_Id),
        terminus_store:predicate_id(Layer, Predicate_String, P_Id),
        terminus_store:object_id(Layer, node(Subject_String), S_Id),
        atom_string(Other_Subject, Other_Subject_String),
        atom_string(Subject, Subject_String),
        atom_string(Predicate, Predicate_String),
        Witness = json{ '@type' : deleted_object_still_referenced,
                        subject : Other_Subject,
                        predicate : Predicate,
                        object : Subject }).

refute_subject_type_change(Validation_Object,S_Id,Witness) :-
    instance_layer(Validation_Object, Layer),

    global_prefix_expand(rdf:type,Rdf_Type),
    terminus_store:predicate_id(Layer,Rdf_Type, Rdf_Type_Id),
    terminus_store:id_triple_removal(Layer,S_Id,Rdf_Type_Id,Old_Type_Id),
    terminus_store:id_triple_addition(Layer,S_Id,Rdf_Type_Id,New_Type_Id),

    terminus_store:object_id(Layer,Old_Type_Id,node(Old_Type)),
    terminus_store:object_id(Layer,New_Type_Id,node(New_Type)),
    Witness = json{ '@type' : subject_type_has_changed,
                    old_type : Old_Type,
                    new_type : New_Type}.

refute_object_type(_Validation_Object,Class,_,_,_) :-
    (   is_json_document_type(Class)
    ;   is_json_subdocument_type(Class)
    ),
    % Extremely weak checking here - we could check the form of
    % predicates, but this will be faster.
    !,
    fail.
refute_object_type(Validation_Object,Class,S_Id,P_Id,Witness) :-
    is_array_type(Class),
    !,
    instance_layer(Validation_Object, Layer),
    terminus_store:predicate_id(Layer, Predicate_String, P_Id),
    atom_string(Predicate, Predicate_String),
    \+ (   global_prefix_expand(sys:index3, SYS_Index4),
           global_prefix_expand(sys:index3, SYS_Index3),
           global_prefix_expand(sys:index2, SYS_Index2),
           global_prefix_expand(sys:index, SYS_Index),
           global_prefix_expand(sys:value, SYS_Value),
           global_prefix_expand(rdf:type, RDF_Type),
           memberchk(Predicate, [SYS_Index4,
                                 SYS_Index3,
                                 SYS_Index2,
                                 SYS_Index,
                                 SYS_Value,
                                 RDF_Type])),
    terminus_store:subject_id(Layer, Subject, S_Id),
    Witness = json{ '@type' : invalid_array_type,
                    subject: Subject,
                    class: Class }.
refute_object_type(Validation_Object,Class,S_Id,P_Id,Witness) :-
    is_list_type(Class),
    !,
    instance_layer(Validation_Object, Layer),
    terminus_store:predicate_id(Layer, Predicate_String, P_Id),
    atom_string(Predicate, Predicate_String),
    \+ (   global_prefix_expand(rdf:first, RDF_First),
           global_prefix_expand(rdf:rest, RDF_Rest),
           global_prefix_expand(rdf:type, RDF_Type),
           memberchk(Predicate, [RDF_First, RDF_Rest, RDF_Type])),
    terminus_store:subject_id(Layer, Subject, S_Id),
    Witness = json{ '@type' : invalid_list_type,
                    subject: Subject,
                    class: Class }.
refute_object_type(Validation_Object, Class,S_Id,P_Id,Witness) :-
    instance_layer(Validation_Object, Layer),
    terminus_store:predicate_id(Layer, Predicate_String, P_Id),
    atom_string(Predicate, Predicate_String),
    (   class_predicate_type(Validation_Object, Class, Predicate, Type)
    ->  terminus_store:id_triple_addition(Layer,S_Id,P_Id,O_Id),
        terminus_store:object_id(Layer,O,O_Id),
        storage_object(O,Object),
        refute_object_type_(Type,Validation_Object,Object,Witness)
    ;   terminus_store:id_triple(Layer,S_Id,P_Id,_),
        terminus_store:subject_id(Layer, Subject_String, S_Id),
        atom_string(Subject, Subject_String),
        Witness = json{ '@type' : invalid_predicate,
                        class: Class,
                        predicate: Predicate,
                        subject: Subject }
    ).

refute_object_type_(enum(C,List),_Validation_Object,Object,Witness) :-
    \+ memberchk(Object, List),
    Witness = witness{ '@type' : instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(base_type(C),_Validation_Object,Object,Witness) :-
    refute_basetype_elt(Object,C,Witness).
refute_object_type_(foreign(C),Validation_Object,Object,Witness) :-
    \+ foreign_instance_of(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(class(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(set(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(cardinality(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(optional(C),Validation_Object,Object,Witness) :-
    \+ is_instance(Validation_Object,Object,C),
    Witness = witness{ '@type': instance_not_of_class,
                       class: C,
                       instance: Object }.
refute_object_type_(array(C,_D),Validation_Object,Object,Witness) :-
    database_instance(Validation_Object, Instance),
    xrdf_added(Instance,Object,sys:value,O),
    \+ is_instance(Validation_Object,O,C),
    Witness = witness{
                  '@type': array_instance_not_of_class,
                  class: C,
                  object: O,
                  array: Object
              }.
refute_object_type_(list(C),Validation_Object,Object,Witness) :-
    (   \+ is_rdf_list(Validation_Object, Object)
    ->  Witness = witness{'@type':not_a_valid_list,
                          class:C,
                          list:Object}
    ;   member_list(Validation_Object, Elt,Object),
        \+ is_instance(Validation_Object,Elt,C),
        Witness = witness{
                      '@type': list_element_of_wrong_type,
                      class: C,
                      object: Elt,
                      list: Object
                  }
    ).
refute_object_type_(table(C),Validation_Object,Object,Witness) :-
    (   \+ is_rdf_list(Validation_Object, Object)
    ->  Witness = witness{'@type':not_a_valid_table,
                          class:C,
                          table:Object}
    ;   member_array(Validation_Object, Array_Elt, Object),
        refute_object_type_(array(C,2),Validation_Object,Array_Elt,Array_Witness),
        (   witness{'@type':array_element_not_of_class,
                    object: Elt} :< Array_Witness,
            Witness = witness{
                          '@type': table_element_of_wrong_type,
                          class: C,
                          object: Elt,
                          list: Array_Elt,
                          table: Object
                      }
        )
    ).

refute_built_in(Validation_Object,Subject,Predicate,Witness) :-
    database_instance(Validation_Object, Instance),
    xrdf_added(Instance,Subject,Predicate,Value),
    refute_built_in_value(Validation_Object,Predicate,Value,Witness).

refute_abstract(Subject, Class, Witness) :-
    Witness = witness{
                  '@type': reifying_abstract_class,
                  class: Class,
                  subject: Subject
              }.

refute_typed_subject(Validation_Object,S_Id,Class,Witness) :-
    subject_predicate_changed(Validation_Object,S_Id,P_Id),
    % We also need to check arrays / lists for coherence here?
    (   instance_layer(Validation_Object, Layer),
        terminus_store:predicate_id(Layer, Predicate_String, P_Id),
        atom_string(Predicate,Predicate_String),
        is_built_in(Predicate)
    ->  (   refute_built_in(Validation_Object,S_Id,P_Id,Witness)
        ;   global_prefix_expand(rdf:type, Predicate),
            (   refute_subject_deletion(Validation_Object, S_Id, Witness)
            ;   refute_subject_type_change(Validation_Object,S_Id,Witness)
            ;   refute_cardinality_new(Validation_Object,S_Id,Class,Witness)))
    ;   is_abstract(Validation_Object,Class)
    ->  refute_abstract(S_Id, Class, Witness)
    ;   refute_subject_type_change(Validation_Object,S_Id,Witness)
    ;   refute_key(Validation_Object,S_Id,P_Id,Class,Witness)
        % NOTE: Perhaps this can be more intelligence predicates
    ;   refute_cardinality(Validation_Object,S_Id,P_Id,Class,Witness)
    ;   refute_object_type(Validation_Object,Class,S_Id,P_Id,Witness)
    ).

refute_subject(Validation_Object,S_Id,Witness) :-
    (   refute_subject_deletion(Validation_Object,S_Id,Witness)
    *-> true
    ;   refute_subject_1(Validation_Object, S_Id, Witness)).

refute_subject_1(Validation_Object,S_Id,_Witness) :-
    instance_layer(Validation_Object, Layer),
    \+ terminus_store:id_triple(Layer,S_Id, _,_),
    !,
    fail.
refute_subject_1(Validation_Object,S_Id,Witness) :-
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject, S_Id),
    foreign_instance_of(Validation_Object, Subject, Class),
    !,
    database_instance(Validation_Object, Instance),
    global_prefix_expand(sys:foreign_type,Foreign_Type),
    xrdf(Instance,Subject, P, _),
    \+ P = Foreign_Type,
    Witness = witness{
                  '@type': foreign_type_has_local_properties,
                  property: P,
                  foreign_type: Class,
                  instance: Subject
              }.
refute_subject_1(Validation_Object,S_Id,Witness) :-
    instance_layer(Validation_Object, Layer),
    terminus_store:subject_id(Layer, Subject, S_Id),
    (   instance_of(Validation_Object, Subject, Class)
    ->  refute_typed_subject(Validation_Object, S_Id, Class, Witness)
    ;   Witness = witness{
                      '@type': subject_has_no_type,
                      subject: Subject
                  }).

refute_instance(Validation_Object, Witness) :-
    subject_changed(Validation_Object, Subject),
    refute_subject(Validation_Object,Subject,Witness).

refute_instance_schema(Validation_Object, Witness) :-
    refute_schema(Validation_Object,Witness).
refute_instance_schema(Validation_Object, Witness) :-
    instance_layer(Validation_Object, Layer),
    distinct(S_Id,
             terminus_store:id_triple(Layer, S_Id, _, _)),
    refute_subject(Validation_Object,S_Id,Witness).

/*

Referential integrity only checking
===================================

↓ = subdocument link
⇓ = Document link
abc = Document id
a.n = Subdocument id (of a)

Deletion:
=========

If document 'b' is deleted in the following diagram, we must check to
see that a.1 has no remaining references

In general check:

* ∀y. -(s,p,o) ∧ doc(o) ∧ domain(p)=T ⇒ Card(x,p,T)
* ∀o. -o:S ∧ ¬ (s,p,o)

            a
          ↙
         a.1
         ⇓
         b
       ↙  ↘
      b.1   b.2
      ⇓     ⇓
      c     d

Insertion
=========

Supposing I insert documents 'a' and 'b'. I need to check that 'c and
'd' have the appropriate ascribed type.

In general check ∀o. +(s,p,o) ∧ doc(o) ∧ range(p)=T ⇒ o:T

            a
          ↙
         a.1
         ⇓
         b
       ↙  ↘
      b.1   b.2
      ⇓      ⇓
      c      d

a -q-> o     a:C, range(C,q) = T, o:T
b -p-/       b:D, range(D,p) = S, o:S

Linking
=======

Suppose we insert a document by using @linked-by. We now have to check:

* Type correctness of the object according to the subjects class & property.
* Cardinality of the subjects class & property.

For subdocuments we must also check:

* Uniqueness of reference

*/

validation_object_backlinks(Validation_Object, BackLinks) :-
    database_instance(Validation_Object, [RWO]),
    get_dict(backlinks,RWO,BackLinks).

is_document(Validation_Object, I) :-
    instance_layer(Validation_Object, Instance),
    global_prefix_expand(rdf:type, Rdf_Type),
    triple(Instance, I, Rdf_Type, node(CS)),
    atom_string(C, CS),
    is_simple_class(Validation_Object, C),
    \+ is_subdocument(Validation_Object, C).

was_document(Validation_Object, I) :-
    instance_layer(Validation_Object, Instance),
    global_prefix_expand(rdf:type, Rdf_Type),
    terminus_store:triple_removal(Instance, I, Rdf_Type, node(CS)),
    atom_string(C, CS),
    is_simple_class(Validation_Object, C),
    \+ is_subdocument(Validation_Object, C).

was_list(Validation_Object, I) :-
    instance_layer(Validation_Object, Instance),
    rdf_type(Rdf_Type),
    global_prefix_expand(rdf:type, Rdf_Type),
    global_prefix_expand(rdf:'List', Rdf_List),
    atom_string(Rdf_List, C),
    triple_removal(Instance, I, Rdf_Type, node(C)).

was_in_list(Validation_Object, L, O) :-
    instance_layer(Validation_Object, Instance),
    rdf_first(Rdf_First),
    rdf_type(Rdf_Type),
    triple_removal(Instance, O, Rdf_Type, node(_)),
    was_document(Validation_Object, O),
    triple(Instance, L, Rdf_First, node(O)).

% Generator for: ∀ +(s,p,o). ∃ C,p,o,T. doc(o) ∧ s:C ⇒ range(C,p)=T ∧ o:T
referential_range_candidate(Validation_Object,C,P,O) :-
    instance_layer(Validation_Object, Instance),
    global_prefix_expand(rdf:type, Rdf_Type),
    validation_object_backlinks(Validation_Object, BackLinks),
    global_prefix_expand(rdf:'List', Rdf_List),
    global_prefix_expand(sys:'Array', Sys_Array),
    % Shared dictionary for predicates would be handy here!
    distinct(C-P-O,
             (   (   triple_addition(Instance, S, P, node(O)),
                     is_document(Validation_Object, O)
                 ;   member(link(S,P,O), BackLinks),
                     is_subdocument(Validation_Object, O)
                 ),
                 triple(Instance, S, Rdf_Type, node(C_String)),
                 atom_string(C, C_String),
                 \+ memberchk(C,[Sys_Array,Rdf_List])
             )).

% Generator for: ∃ s,o,p. +(s,p,o) ∧ ¬ (∃ T. o:T)
references_untyped_range(Validation_Object,S,P,O) :-
    instance_layer(Validation_Object, Instance),
    global_prefix_expand(rdf:type, Rdf_Type),
    global_prefix_expand(sys:foreign_type, Foreign_Type),
    global_prefix_expand(rdf:nil, Rdf_Nil),
    global_prefix_expand(sys:value, Value),
    global_prefix_expand(rdf:first, Rdf_First),
    % Shared dictionary for predicates would be handy here!
    distinct(S-P-O,
             (   triple_addition(Instance, S, P, node(O)),
                 %% valid typeless scenarios:
                 %% * P = rdf:type
                 %% * P = sys:foreign_type
                 %% * O is rdf:nil
                 %% * range(P) = C ∧ is_enum(C) ∨ is_foreign(C)
                 \+ (   atom_string(Rdf_Type,P)
                    ;   atom_string(Foreign_Type, P)
                    ;   atom_string(Rdf_Nil,O)
                    ;   atom_string(Value,P)
                    ;   atom_string(Rdf_First,P)
                    ;   triple(Instance, S, Rdf_Type, node(C)),
                        class_predicate_type(Validation_Object, C, P, Pre_Type),
                        (   Pre_Type = set(E)
                        ->  type_descriptor(Validation_Object, E, Type)
                        ;   Pre_Type = optional(E)
                        ->  type_descriptor(Validation_Object, E, Type)
                        ;   Pre_Type = cardinality(E,_,_)
                        ->  type_descriptor(Validation_Object, E, Type)
                        ;   Pre_Type = Type
                        ),
                        (   Type = enum(_,_)
                        ->  true
                        ;   Type = foreign(_)
                        )
                    ),
                 \+ triple(Instance, O, Rdf_Type, node(_))
             )).

references_untyped_array_range(Validation_Object,S,P,O) :-
    instance_layer(Validation_Object, Instance),
    global_prefix_expand(rdf:type, Rdf_Type),
    global_prefix_expand(sys:value, Value),
    % Shared dictionary for predicates would be handy here!
    distinct(S-P-O,
             (   triple_addition(Instance, Array, Value, node(O)),
                 \+ triple(Instance, O, Rdf_Type, node(_)),
                 triple(Instance, S, P, node(Array)),
                 triple(Instance, S, Rdf_Type, node(C)),
                 class_predicate_type(Validation_Object, C, P, array(E,_Dimensions)),
                 \+ (   type_descriptor(Validation_Object, E, Type),
                        (   Type = enum(_,_)
                        ->  true
                        ;   Type = foreign(_)
                        )
                     )
             )).

% Try to scan exactly once for lists. This is surely too much work, and
% relies on new cons cells being created, but should be correct.
references_untyped_list_range(Validation_Object,S,P,O,Idx) :-
    instance_layer(Validation_Object, Instance),
    global_prefix_expand(rdf:type, Rdf_Type),
    global_prefix_expand(rdf:'List', Rdf_List),
    distinct(S-P-O,
             (   triple_addition(Instance, S, P, node(List)),
                 triple_addition(Instance, List, Rdf_Type, node(Rdf_List)),
                 triple(Instance, S, Rdf_Type, node(C)),
                 class_predicate_type(Validation_Object, C, P, list(E)),
                 \+ (   type_descriptor(Validation_Object, E, Type),
                        (   Type = enum(_,_)
                        ->  true
                        ;   Type = foreign(_)
                        )
                    ),
                 find_untyped_element_in_list(Instance,List,O,Idx)
             )).

find_untyped_element_in_list(Instance,Cons,O,Idx) :-
    find_untyped_element_in_list(Instance,Cons,O,0,Idx).

find_untyped_element_in_list(Instance,Cons,O,Current_Idx,Idx) :-
    global_prefix_expand(rdf:first, Rdf_First),
    global_prefix_expand(rdf:rest, Rdf_Rest),
    global_prefix_expand(rdf:type, Rdf_Type),
    triple(Instance,Cons,Rdf_First,node(Elt)),
    (   \+ triple(Instance,Elt,Rdf_Type,node(_Type))
    ->  Current_Idx = Idx,
        O = Elt
    ;   Next_Idx is Current_Idx + 1,
        triple(Instance,Cons,Rdf_Rest,node(Next_Cons)),
        find_untyped_element_in_list(Instance,Next_Cons,O,Next_Idx,Idx)
    ).

instance_domain(Validation_Object, S, Descriptor) :-
    rdf_list(Rdf_List),
    rdf_type(Rdf_Type),
    instance_layer(Validation_Object, Instance),
    instance_layer(Validation_Object, Schema),
    triple(Instance, S, Rdf_Type, node(T)),
    (   atom_string(Rdf_List, T)
    ->  Descriptor = Rdf_List
    ;   schema_type_descriptor(Schema,T,Descriptor)
    ).

rdf_type(Rdf_Type) :-
    global_prefix_expand(rdf:type, Rdf_Type).

rdf_first(Rdf_First) :-
    global_prefix_expand(rdf:first, Rdf_First).

rdf_list(Rdf_List) :-
    global_prefix_expand(rdf:'List', Rdf_List).

% Generator for: ∃ s,p,o. -o:S ∧ (s,p,o)
dangling_reference_candidate(Validation_Object,S,P,O) :-
    instance_layer(Validation_Object, Instance),
    rdf_type(Rdf_Type),
    %test_utils:print_all_triples(Validation_Object, []),
    !,
    distinct(S-P-O,
             (   triple_removal(Instance, O, Rdf_Type, node(_)),
                 triple(Instance, S, P, node(O))
             ;   % check if we were dropped from a list
                 was_in_list(Validation_Object, S, O),
                 rdf_first(P)
             )).

% Generator for: ∀ -(s,p,o). ∃ o,p,C. doc(o) ∧ s:C ⇒ card(s,p,C)
referential_cardinality_candidate(Validation_Object,S,P,C) :-
    instance_layer(Validation_Object, Instance),
    rdf_type(Rdf_Type),
    validation_object_backlinks(Validation_Object, BackLinks),
    % Shared dictionary for predicates would be handy here!
    distinct(S-P-C,
             (   (   triple_removal(Instance, S, P, node(O)),
                     \+ atom_string(Rdf_Type,P),
                     was_document(Validation_Object, O)
                 ;   distinct(
                         S-P,
                         (   member(link(S,P,_),BackLinks),
                             \+ atom_string(Rdf_Type,P)
                         ))
                 ),
                 triple(Instance, S, Rdf_Type, node(C))
             )).

referential_unique_candidate(Validation_Object, O) :-
    validation_object_backlinks(Validation_Object, BackLinks),
    distinct(O,
             member(link(_,_,O), BackLinks)),
    is_subdocument(Validation_Object, O).

refute_uniqueness_of_reference(Validation_Object, O, Witness) :-
    database_instance(Validation_Object,Instance),
    findall(
        json{
            subject : S,
            predicate : P
        },
        xrdf(Instance,S,P,O),
        References),
    \+ References = [_],
    Witness = witness{'@type':referenced_document_is_not_unique,
                      object: O,
                      references: References}.

refute_referential_integrity(Validation_Object,Witness) :-
    referential_unique_candidate(Validation_Object, O),
    refute_uniqueness_of_reference(Validation_Object, O, Witness).
refute_referential_integrity(Validation_Object,Witness) :-
    referential_cardinality_candidate(Validation_Object, S, P, T),
    instance_layer(Validation_Object, Instance),
    terminus_store:subject_id(Instance,S,S_Id),
    terminus_store:predicate_id(Instance,P,P_Id),
    refute_cardinality(Validation_Object,S_Id,P_Id,T,Witness).
refute_referential_integrity(Validation_Object,Witness) :-
    dangling_reference_candidate(Validation_Object, S, P, O),
    Witness = witness{'@type':deleted_object_still_referenced,
                      object:O,
                      predicate:P,
                      subject:S}.
refute_referential_integrity(Validation_Object,Witness) :-
    referential_range_candidate(Validation_Object, C, P, O),
    refute_range(Validation_Object, C, P, O, Witness).
refute_referential_integrity(Validation_Object,Witness) :-
    references_untyped_range(Validation_Object, S, P, O),
    Witness =
    witness{ '@type': references_untyped_object,
             subject: S,
             predicate : P,
             object: O
           }.
refute_referential_integrity(Validation_Object,Witness) :-
    references_untyped_array_range(Validation_Object, S, P, O),
    Witness =
    witness{ '@type': references_untyped_array_range,
             subject: S,
             predicate : P,
             object: O
           }.
refute_referential_integrity(Validation_Object,Witness) :-
    references_untyped_list_range(Validation_Object, S, P, O, Idx),
    Witness =
    witness{ '@type': references_untyped_list_range,
             subject: S,
             predicate : P,
             object: O,
             index: Idx
           }.

refute_range(Validation_Object, C, P, O, Witness) :-
    (   class_predicate_type(Validation_Object, C, P, T)
    ->  instance_layer(Validation_Object, Instance),
        global_prefix_expand(rdf:type, Rdf_Type),
        triple(Instance, O, Rdf_Type, node(CS)),
        atom_string(C,CS),
        do_or_die(extract_base_type(T,Super),
                  error(unexpected_document_type_encountered(O,P,T,C))),
        (   \+ class_subsumed(Validation_Object, C, Super)
        ->  extract_base_type(T, Base),
            Witness = witness{ '@type': referential_integrity_violation,
                               instance: O,
                               actual_class: C,
                               required_class: Base,
                               predicate: P
                             }
        )
    ;   Witness =
        json{ '@type' : unknown_property_for_type,
              property : P,
              type : C }
    ).

extract_base_type(T,C) :-
    extract_base_type_(T,CS),
    atom_string(C,CS).

extract_base_type_(class(C),C).
extract_base_type_(optional(C),C).
extract_base_type_(set(C),C).
extract_base_type_(cardinality(C),C).
extract_base_type_(optional(C),C).

%%%%%%%%%%%%%%%%%%%%%%
%%  BASETYPES ONLY  %%
%%%%%%%%%%%%%%%%%%%%%%

/*
 * refute_basetype_elt(+Literal,+Type,-Reason)
 */
refute_basetype_elt(L,T,R) :-
    (   L = _^^T2,
        \+ basetype_subsumption_of(T,T2)
    ->  R = json{
                '@type' : 'vio:DataTypeSubsumptionViolation',
                'vio:message' : 'Could not subsume type1:required_type with type2:found_type',
			    'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : T},
			    'vio:parent_type' : json{ '@type' : 'xsd:string', '@value' : T2}
            }
    ;   refute_basetype_elt_(T,L,R)
    ).

refute_basetype_elt_('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',S@L,Reason) :-
    (   \+ (atom(S) ; string(S)),
        term_to_atom(S@L,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom or string for language value, found term.',
			         'vio:literal' : json{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/1999/02/22-rdf-syntax-ns#langString',S@L,Reason) :-
    (   \+ atom(L),
        term_to_atom(S@L,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom in language section, found term.',
			         'vio:literal' : json{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S@L,Reason) :-
    (   \+ atom(L),
        term_to_atom(S@L,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom in language section, found term.',
			         'vio:literal' : json{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S@L,Reason) :-
    (   \+ (atom(S) ; string(S)),
        term_to_atom(S@L,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom or string for language value, found term.',
			         'vio:literal' : json{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S^^_,Reason) :-
    (   \+ (atom(S) ; string(S)),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom, found term as element.',
			         'vio:literal' : json{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#string',S^^T,Reason) :-
    (   \+ atom(T),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
			         'vio:message' : 'Expected atom, found term as type.',
			         'vio:literal' : json{ '@value' : A, '@type' : 'xsd:anySimpleType'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#coordinatePolygon',S^^_, Reason) :-
    (   \+ is_coordinate_polygon(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate polygon',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:coordinatePolygon'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#coordinatePolyline',S^^_, Reason) :-
    (   \+ is_coordinate_polygon(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate polyline',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:coordinatePolyline'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#coordinate',S^^_, Reason) :-
    (   \+ is_point(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed coordinate',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:coordinate'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#dateRange',S^^_, Reason) :-
    (   \+ is_date_range(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed dateRange',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:dateRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#integerRange',S^^_, Reason) :-
    (   \+ is_integer_range(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed integerRange',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:integerRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#decimalRange',S^^_, Reason) :-
    (   \+ is_decimal_range(S),
        term_to_atom(S,A)
	->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed decimalRange',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:decimalRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#gYearRange',S^^_, Reason) :-
    (   \+ is_gyear_range(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed gYearRange',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:gYearRange'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#url',S^^_, Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:url,C,[])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid URL',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:url'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#email',S^^_, Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:email,C,[])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid email address',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:email'}
                 }
    ).
refute_basetype_elt_('http://terminusdb.com/schema/xdd#json',S^^_, Reason) :-
    (   \+ (catch(atom_json_dict(S,_,[]),_,fail)),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a valid json object',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xdd:json'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#boolean',S^^_,Reason) :-
    (   \+ is_boolean(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed boolean.',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:boolean'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#decimal',S^^_,Reason) :-
    (   \+ number(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed decimal.',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:decimal'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#integer',S^^_,Reason) :-
    (   \+ integer(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed integer.',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:integer'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#double',S^^_,Reason) :-
    (   \+ float(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed double.',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:double'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#float',S^^_,Reason) :-
    (   \+ float(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed float.',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:float'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#time',S^^_,Reason) :-
    (   \+ is_time(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:time',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:time'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#date',S^^_,Reason) :-
    (   \+ is_date(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:date.',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:date'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#dateTime', S^^_,Reason) :-
    (   \+ is_date_time(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:dateTime : parameter out of range.',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:dateTime'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gYear',S^^_,Reason) :-
    (   \+ is_gyear(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYear',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:gYear'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gMonth',S^^_,Reason) :-
    (   \+ is_gmonth(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:Month',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gDay',S^^_,Reason) :-
    (   \+ is_gday(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gMonth',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:gMonth'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gYearMonth',S^^_,Reason) :-
    (   \+ is_gyear_month(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYearMonth',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:gYearMonth'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#gMonthDay',S^^_,Reason) :-
    (   \+ is_gmonth_day(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:gYearMonth',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:gMonthDay'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#duration',S^^_,Reason) :-
    (   \+ is_duration(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:duration',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:duration'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#byte',S^^_,Reason) :-
    (   \+ is_byte(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:byte',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:byte'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#short',S^^_,Reason) :-
    (   \+ is_short(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:short',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:short'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#int',S^^_,Reason) :-
    (   \+ is_int(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:int',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:int'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#long',S^^_,Reason) :-
    (   \+ is_long(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:long',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:long'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedByte',S^^_,Reason) :-
    (   \+ is_unsigned_byte(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedByte',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedByte'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedShort',S^^_,Reason) :-
    (   \+ is_unsigned_short(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedShort',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedShort'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedInt',S^^_,Reason) :-
    (   \+ is_unsigned_int(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedInt',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedInt'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#unsignedLong',S^^_,Reason) :-
    (   \+ is_unsigned_long(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:unsignedLong',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:unsignedLong'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#positiveInteger',S^^_,Reason) :-
    (   \+ is_positive_integer(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:positiveInteger',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:positiveInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',S^^_,Reason) :-
    (   \+ is_nonnegative_integer(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:nonNegativeInteger',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
	                 'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:nonNegativeInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#negativeInteger',S^^_,Reason) :-
    (   \+ is_negative_integer(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:negativeInteger',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:negativeInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#nonPositiveInteger',S^^_,Reason) :-
    (   \+ is_nonpositive_integer(S),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:nonPositiveInteger',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:nonPositiveInteger'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#base64Binary',S^^_,Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:base64Binary,C,[])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:base64Binary',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:base64Binary'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#hexBinary',S^^_,Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:hexBinary,C,[])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:hexBinary',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:hexBinary'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#anyURI',S^^_,Reason) :-
    (   \+ uri_components(S,_),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:anyUri',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:anyURI'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#language',S^^_,Reason) :-
    (   \+ (atom_codes(S,C), phrase(xsd_parser:language, C, [])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:language',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:language'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#normalizedString',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:normalizedString,C, [])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:normalizedString',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:normalizedString'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#token',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:normalizedString,C, [])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:token',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:token'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#NMTOKEN',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:nmtoken,C, [])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:NMTOKEN',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:NMTOKEN'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#Name',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:name,C, [])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:Name',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:Name'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/2001/XMLSchema#NCName',S^^_,Reason) :-
    (   \+  (atom_codes(S,C), phrase(xsd_parser:ncname,C, [])),
        term_to_atom(S,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed xsd:NCName',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'xsd:NCName'}
                 }
    ).
refute_basetype_elt_('http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral',T,Reason) :-
    (   \+ (atom(T) ; string(T)),
        term_to_atom(T,A)
    ->  Reason = json{
                     '@type' : 'vio:ViolationWithDatatypeObject',
                     'vio:message' : 'Not a well formed rdf:PlainLiteral',
                     'vio:literal' : json{ '@type' : 'xsd:anySimpleType', '@value' : A},
                     'vio:base_type' : json{ '@type' : 'xsd:string', '@value' : 'rdf:PlainLiteral'}
                 }
    ).
