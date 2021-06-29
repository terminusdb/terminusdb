:- module('document/query', [
          ]).

:- use_module(instance).
:- use_module(schema).
:- use_module(json).

:- use_module(library(pcre)).
:- use_module(library(yall)).
:- use_module(library(apply_macros)).
:- use_module(library(terminus_store)).
:- use_module(library(http/json)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

expand_query_document_class_property('@id', DB, Type, Query, '@id', Query_Ex) :-
    (   string(Query)
    ;   atom(Query)),
    !,
    database_context(DB,Prefixes),
    do_or_die(prefix_expand(Query, Prefixes, Expanded_Id),
              query_error(unknown_prefix(Query), _)),
    Query_Ex = id_exact(Type, Expanded_Id).

expand_query_document_class_property('@id', DB, Type, _{ '@one-of' : List}, '@id', Query_Ex) :-
    is_list(List),
    !,
    database_context(DB,Prefixes),
    maplist({Prefixes}/[Q, Q_Ex]>>(
                do_or_die(prefix_expand(Q, Prefixes, Q_Ex),
                          query_error(unknown_prefix(Q), _))
            ),
            List,
            List_Ex),
    Query_Ex = id_one_of(Type, List_Ex).
expand_query_document_class_property('@id', _DB, _Type, Query, _Prop_Ex, _Query_Ex) :-
    throw(error(query_error(unrecognized_query_document(Query)), _)).
expand_query_document_class_property('@type', DB, _Type, Query, '@type', Query_Ex) :-
    (   string(Query)
    ;   atom(Query)),
    !,
    database_context(DB,Prefixes),
    do_or_die(prefix_expand_schema(Query, Prefixes, Query_Type_Ex),
              query_error(unknown_prefix(Query), _)),

    Query_Ex = type_exact(Query_Type_Ex).
expand_query_document_class_property('@type', _DB, _Type, Query, _Prop_Ex, _Query_Ex) :-
    throw(error(query_error(unrecognized_query_document(Query)), _)).
expand_query_document_class_property(Prop, DB, Type, Query, Prop_Ex, Query_Ex) :-
    database_context(DB, Database_Context),
    do_or_die(prefix_expand_schema(Prop, Database_Context, Prop_Ex),
              error(query_error(unknown_prefix(Prop)), _)),

    do_or_die(class_predicate_type(DB, Type, Prop_Ex, Prop_Type),
              error(query_error(unknown_property_for_type(Type, Prop)), _)),

    expand_query_document_for_type(Prop_Type, DB, Query, Query_Ex).

expand_query_document_for_type(_, _, _{}, _{}) :- !.
expand_query_document_for_type(base_class(Type), _DB, Query, Query_Ex) :-
    atomic(Query),
    !,
    % todo check that the given value actually matches what we need here
    Query_Ex = base_class_match_value(Type, Query).
expand_query_document_for_type(base_class(Type), _DB, Query, Query_Ex) :-
    Query = _{'@regex': Regex},
    !,
    do_or_die(string(Regex),
              error(query_error(regex_not_a_string(Regex)),_)),
    do_or_die(re_compile(Regex, Compiled, []),
              error(query_error(regex_not_valid(Regex)))),
    % todo this error is better if we somehow had a property still
    do_or_die(Type = 'http://www.w3.org/2001/XMLSchema#string',
              error(query_error(regex_against_non_string(Type, Regex)),_)),
    % todo check that the given value actually matches what we need here
    Query_Ex = string_match_regex(Compiled).
expand_query_document_for_type(base_class(Type), _DB, Query, _Query_Ex) :-
    throw(error(query_error(unknown_query_format_for_datatype(Type, Query)), _)).

expand_query_document_for_type(class(Type), DB, Query, Query_Ex) :-
    !,
    dict_pairs(Query, Query_Tag, Query_Pairs),
    findall(
        Prop_Ex-Prop_Query_Ex,
        (   
            member(Prop-Prop_Query, Query_Pairs),
            expand_query_document_class_property(Prop, DB, Type, Prop_Query, Prop_Ex, Prop_Query_Ex)
        ),
        Query_Pairs_Ex),

    dict_pairs(Query_Ex, Query_Tag, Query_Pairs_Ex).
expand_query_document_for_type(optional(Type), DB, Query, Query_Ex) :-
    Query = _{'@if-exists': Inner_Query},
    !,
    Query_Ex = _{'@optional-if-exists': Inner_Query_Ex},
    expand_query_document_(DB, Type, Inner_Query, Inner_Query_Ex).
expand_query_document_for_type(optional(Type), DB, null, null) :-
    !,
    expand_query_document_(DB, Type, {}, _).
expand_query_document_for_type(optional(Type), DB, Query, Query_Ex) :-
    !,
    expand_query_document_(DB, Type, Query, Query_Ex).
expand_query_document_for_type(Type, _Db, _Query, _Query_Ex) :-
    throw(error(query_error(unknown_type(Type)), _)).

expand_query_document_(DB, Type, Query, Query_Ex) :-
    (   get_dict('@type', Query, Query_Type)
    ->  do_or_die(class_subsumed(DB, Type, Query_Type),
                  error(query_error(not_a_a_subclass(Type, Query_Type)), _))
    ;   Query_Type = Type),

    do_or_die(type_descriptor(DB, Query_Type, Query_Type_Descriptor),
              error(query_error(type_not_found(Query_Type)), _)),

    expand_query_document_for_type(Query_Type_Descriptor, DB, Query, Query_Ex).

expand_query_document(DB, Type, Query, Query_Ex) :-
    database_context(DB,Prefixes),
    do_or_die(prefix_expand_schema(Type, Prefixes, Type_Ex),
              query_error(unknown_prefix(Type), _)),

    expand_query_document_(DB, Type_Ex, Query, Query_Ex).

match_query_document_against_uri_property(base_class_match_value(Type, Value), DB, URI, Property) :-
    !,
    database_instance(DB, Instance),
    xrdf(Instance, URI, Property, Value^^Type).
match_query_document_against_uri_property(string_match_regex(Regex), DB, URI, Property) :-
    !,
    database_instance(DB, Instance),
    xrdf(Instance, URI, Property, String^^'http://www.w3.org/2001/XMLSchema#string'),
    re_match(Regex, String).
match_query_document_against_uri_property(Query, DB, URI, Property) :-
    is_dict(Query),
    !,
    database_instance(DB, Instance),
    xrdf(Instance, URI, Property, Object),
    match_query_document_(Query, DB, Object),
    !.

match_query_document_(Query, DB, Uri) :-
    do_or_die(is_dict(Query),
              error(query_error(not_a_dict(Query)), _)),

    dict_pairs(Query, _, Pairs),
    forall(member(P-V, Pairs),
           match_query_document_against_uri_property(V, DB, Uri, P)),
    !.
    
match_query_document(DB, Type, Query, Compress, Unfold, Document) :-
    expand_query_document(DB, Type, Query, Query_Ex),
    get_document_uri(DB, Type, Uri),
    match_query_document_(Query_Ex, DB, Uri),
    get_document(DB, Compress, Unfold, Uri, Document).
