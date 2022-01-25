:- module('document/query', [
              match_query_document_uri/4
          ]).

:- use_module(instance).
:- use_module(schema).
:- use_module(json).

:- use_module(library(pcre)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(terminus_store)).
:- use_module(library(http/json)).
:- use_module(library(lists)).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).

expand_query_document_class_property('@id', DB, Type, Query, '@id', Query_Ex) :-
    (   string(Query)
    ;   atom(Query)),
    !,
    database_prefixes(DB,Prefixes),
    do_or_die(prefix_expand(Query, Prefixes, Expanded_Id),
              query_error(unknown_prefix(Query), _)),
    Query_Ex = id_exact(Type, Expanded_Id).

expand_query_document_class_property('@id', DB, Type, _{ '@one-of' : List}, '@id', Query_Ex) :-
    is_list(List),
    !,
    database_prefixes(DB,Prefixes),
    maplist({Prefixes}/[Q, Q_Ex]>>(
                do_or_die(prefix_expand(Q, Prefixes, Q_Ex),
                          query_error(unknown_prefix(Q), _))
            ),
            List,
            List_Ex),
    Query_Ex = id_one_of(Type, List_Ex).
expand_query_document_class_property('@id', _DB, _Type, _{}, _Prop_Ex, _{}) :- !.
expand_query_document_class_property('@id', _DB, _Type, Query, _Prop_Ex, _Query_Ex) :-
    throw(error(query_error(unrecognized_query_document(Query)), _)).
expand_query_document_class_property('@type', DB, _Type, Query, '@type', Query_Ex) :-
    (   string(Query)
    ;   atom(Query)),
    !,
    database_prefixes(DB,Prefixes),
    do_or_die(prefix_expand_schema(Query, Prefixes, Query_Type_Ex),
              query_error(unknown_prefix(Query), _)),

    Query_Ex = type_subsumed(Query_Type_Ex).
expand_query_document_class_property('@type', _DB, _Type, Query, _Prop_Ex, _Query_Ex) :-
    throw(error(query_error(unrecognized_query_document(Query)), _)).
expand_query_document_class_property(Prop, DB, Type, Query, Prop_Ex, Query_Ex) :-
    database_prefixes(DB, Database_Prefixes),
    do_or_die(prefix_expand_schema(Prop, Database_Prefixes, Prop_Ex),
              error(query_error(unknown_prefix(Prop)), _)),

    do_or_die(class_predicate_type(DB, Type, Prop_Ex, Prop_Type),
              error(query_error(unknown_property_for_type(Type, Prop)), _)),

    expand_query_document_for_type(Prop_Type, DB, Query, Query_Ex).

expand_query_document_for_type(base_class(_), _, _{}, _{}) :- !.
expand_query_document_for_type(base_class(Type), _DB, Query, Query_Ex) :-
    atomic(Query),
    !,
    catch(json_value_cast_type(Query,Type,Casted),
          error(casting_error(_,_),_),
          throw(error(query_error(casting_error(Query, Type)), _))),
    % todo check that the given value actually matches what we need here
    Query_Ex = match_value(Casted).
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
expand_query_document_for_type(class(_), _, _{}, _{}) :- !.
expand_query_document_for_type(class(Type), DB, Query, Query_Ex) :-
    is_dict(Query),
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
expand_query_document_for_type(class(_Type), DB, Query, Query_Ex) :-
    atomic(Query),
    !,
    database_prefixes(DB,Prefixes),
    do_or_die(prefix_expand(Query, Prefixes, Expanded),
              query_error(unknown_prefix(Query), _)),
    Query_Ex = match_value(Expanded).
expand_query_document_for_type(optional(_), _, _{}, _{}) :- !.
expand_query_document_for_type(optional(Type), DB, Query, Query_Ex) :-
    \+ is_dict(Query),
    !,
    expand_query_document_for_type(base_class(Type), DB, Query, Query_Ex).
expand_query_document_for_type(optional(Type), DB, Query, Query_Ex) :-
    Query = _{'@if-exists': Inner_Query},
    !,
    Query_Ex = _{'@optional-if-exists': Inner_Query_Ex},
    expand_query_document_(DB, Type, Inner_Query, Inner_Query_Ex,_).
expand_query_document_for_type(optional(Type), DB, null, null) :-
    !,
    expand_query_document_(DB, Type, _{}, _,_).
expand_query_document_for_type(optional(Type), DB, Query, Query_Ex) :-
    expand_query_document_(DB, Type, Query, Query_Ex,_).
expand_query_document_for_type(enum(Type, Values), _Db, Query, Query_Ex) :-
    atomic(Query),
    !,
    atomic_list_concat([Type, '/', Query], Expanded_Enum_Value),
    do_or_die(memberchk(Expanded_Enum_Value, Values),
              error(query_error(value_not_in_enum(Type, Query)), _)),
    Query_Ex = match_value(Expanded_Enum_Value).
expand_query_document_for_type(enum(_Type, _Values), _Db, Query, _Query_Ex) :-
    throw(error(query_error(unknown_query_format_for_enum(Query)), _)).
expand_query_document_for_type(Type, _Db, _Query, _Query_Ex) :-
    throw(error(query_error(unknown_type(Type)), _)).

expand_query_document_(DB, Type, Query, Query_Ex, Type) :-
    _{'@one-of': Documents} = Query,
    !,
    do_or_die(is_list(Documents),
              error(query_error(not_a_query_document_list(Documents)))),
    Query_Ex = class_one_of(Documents_Ex),
    maplist([Documents,Documents_Ex]>>
            expand_query_document_(DB, Type, Documents, Documents_Ex,_),
            Documents,
            Documents_Ex).
expand_query_document_(DB, Type, Query, Query_Ex, Type) :-
    _{'@all': Documents} = Query,
    !,
    do_or_die(is_list(Documents),
              error(query_error(not_a_query_document_list(Documents)))),
    Query_Ex = class_all(Documents_Ex),
    maplist([Documents,Documents_Ex]>>
            expand_query_document_(DB, Type, Documents, Documents_Ex,_),
            Documents,
            Documents_Ex).
expand_query_document_(DB, Type, Query, Query_Ex, Query_Type_Ex) :-
    (   get_dict('@type', Query, Query_Type)
    ->  database_prefixes(DB, Prefixes),
        do_or_die(prefix_expand_schema(Query_Type, Prefixes, Query_Type_Ex),
                  error(query_error(unknown_prefix(Query_Type)),_)),
        do_or_die(once(class_subsumed(DB, Query_Type_Ex, Type)),
                  error(query_error(not_a_subclass(Type, Query_Type_Ex)), _))
    ;   var(Type)
    ->  throw(error(query_error(missing_type(Query)), _))
    ;   Query_Type_Ex = Type),

    do_or_die(type_descriptor(DB, Query_Type_Ex, Query_Type_Descriptor),
              error(query_error(type_not_found(Query_Type_Ex)), _)),

    expand_query_document_for_type(Query_Type_Descriptor, DB, Query, Query_Ex).

expand_query_document(DB, Type, Query, Query_Ex, Query_Type_Ex) :-
    database_prefixes(DB,Prefixes),
    (   var(Type)
    ->  Type_Ex = _
    ;   do_or_die(prefix_expand_schema(Type, Prefixes, Type_Ex),
                  error(query_error(unknown_prefix(Type)), _))),

    expand_query_document_(DB, Type_Ex, Query, Query_Ex, Query_Type_Ex).

% todo rather than check, we should have shortcutted earlier and not retrieve subdocuments with any other URI in the first place.
match_query_document_against_uri_property(id_exact(_Type,URI), _DB, URI, '@id') :-
    !.
match_query_document_against_uri_property(id_one_of(_Type,URIs), _DB, URI, '@id') :-
    !,
    memberchk(URI, URIs).
match_query_document_against_uri_property(_{}, _DB, _URI, '@id') :-
    !.
match_query_document_against_uri_property(type_subsumed(Type), DB, URI, '@type') :-
    !,
    database_instance(DB, Instance),
    xrdf(Instance, URI, rdf:type, Retrieved_Type),
    class_subsumed(DB, Retrieved_Type, Type).
match_query_document_against_uri_property(match_value(Value), DB, URI, Property) :-
    !,
    database_instance(DB, Instance),
    xrdf(Instance, URI, Property, Value).
match_query_document_against_uri_property(string_match_regex(Regex), DB, URI, Property) :-
    !,
    database_instance(DB, Instance),
    xrdf(Instance, URI, Property, String^^'http://www.w3.org/2001/XMLSchema#string'),
    re_match(Regex, String).
match_query_document_against_uri_property(null, DB, URI, Property) :-
    !,
    database_instance(DB, Instance),
    \+ xrdf(Instance, URI, Property, _).
match_query_document_against_uri_property(Query, DB, URI, Property) :-
    is_dict(Query),
    !,
    database_instance(DB, Instance),
    xrdf(Instance, URI, Property, Object),
    match_query_document_uri_(Query, DB, Object),
    !.

match_query_document_uri_(class_one_of(Queries), DB, Uri) :-
    !,
    member(Inner_Query, Queries),
    match_query_document_uri_(Inner_Query, DB, Uri),
    !.
match_query_document_uri_(class_all(Queries), DB, Uri) :-
    !,
    forall(member(Inner_Query, Queries),
           match_query_document_uri_(Inner_Query, DB, Uri)).

match_query_document_uri_(Query, DB, Uri) :-
    do_or_die(is_dict(Query),
              error(query_error(not_a_dict(Query)), _)),

    dict_pairs(Query, _, Pairs),
    forall(member(P-V, Pairs),
           match_query_document_against_uri_property(V, DB, Uri, P)),
    !.

match_query_document_uri(DB, Type, Query, Uri) :-
    expand_query_document(DB, Type, Query, Query_Ex, Type_Ex),

    (   (   is_dict(Query_Ex)
        ->  (   get_dict('@id', Query_Ex, id_exact(_,Uri))
            ->  true
            ;   get_dict('@id', Query_Ex, id_one_of(_, Uris))
            ->  member(Uri, Uris)))
    *->  true
    ;   get_document_uri_by_type(DB, Type_Ex, Uri)),
    match_query_document_uri_(Query_Ex, DB, Uri).

:- begin_tests(query_document).
:- use_module([core(util),
               core(util/test_utils),
               core(transaction),
               core(api),
               json]).

test(query_optional,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: _{'@type': "Optional",
                                         '@class': "xsd:string"}})
                          ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: "foo"},
                                  _Doc1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: "bar"},
                                  Doc2)
                          )),

    open_descriptor(Desc, Db),
    findall(Uri,
            match_query_document_uri(Db,
                                     "Thing",
                                     _{'field': "bar"},
                                     Uri),
            Uris),

    Uris = [Doc2].

test(query_int,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: "xsd:integer"})
                          ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: 1},
                                  _Doc1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: 2},
                                  Doc2)
                          )),

    open_descriptor(Desc, Db),
    findall(Uri,
            match_query_document_uri(Db,
                                     "Thing",
                                     _{'field': 2},
                                     Uri),
            Uris),

    Uris = [Doc2].

test(query_float,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: "xsd:decimal"})
                          ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: 1.2},
                                  _Doc1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: 2.3},
                                  Doc2)
                          )),

    open_descriptor(Desc, Db),
    findall(Uri,
            match_query_document_uri(Db,
                                     "Thing",
                                     _{'field': 2.3},
                                     Uri),
            Uris),

    Uris = [Doc2].

test(query_type_not_found,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          insert_schema_document(
                              C1,
                              _{'@type': "Class",
                                '@id': "Thing",
                                '@key': _{'@type': "Lexical",
                                          '@fields': ["field"]},
                                field: _{'@type': "Optional",
                                         '@class': "xsd:string"}})
                          ),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: "foo"},
                                  _Doc1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    field: "bar"},
                                  _Doc2)
                          )),

    open_descriptor(Desc, Db),
    catch(
        findall(Uri,
                match_query_document_uri(Db,
                                         "Blah",
                                         _{'field': "bar"},
                                         Uri),
                _Uris),
        Error,
        api_error_jsonld(get_documents,Error,JSON)
    ),

    JSON = _{'@type':'api:GetDocumentErrorResponse',
             'api:error':_{'@type':'api:QueryTypeNotFound'},
             'api:message':"Query provided a type 'http://somewhere.for.now/schema#Blah' which was not found in the schema.",
             'api:status':'api:failure'}.

test(query_uri,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          (
                              insert_schema_document(
                                  C1,
                                  _{'@type': "Class",
                                    '@id': "Color",
                                    '@key': _{'@type': "Lexical",
                                              '@fields': [name]},
                                    'name': "xsd:string"}),
                              insert_schema_document(
                                  C1,
                                  _{'@type': "Class",
                                    '@id': "Thing",
                                    color: "Color"})
                          )),

    with_test_transaction(Desc,
                          C2,
                          (
                              insert_document(
                                  C2,
                                  _{'@type': "Color",
                                    name: "red"},
                                  _),
                              insert_document(
                                  C2,
                                  _{'@type': "Color",
                                    name: "blue"},
                                  _),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    color: "Color/red"},
                                  Doc1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    color: "Color/red"},
                                  Doc2),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    color: "Color/blue"},
                                  _Doc3)
                          )),

    open_descriptor(Desc, Db),
    findall(Uri,
            match_query_document_uri(Db,
                                     "Thing",
                                     _{'color': "Color/red"},
                                     Uri),
            Uris),


    sort(Uris, Uris_Sorted),
    sort([Doc1, Doc2], Expected_Uris_Sorted),

    Uris_Sorted = Expected_Uris_Sorted.

test(query_enum,
     [setup((setup_temp_store(State),
             create_db_with_empty_schema("admin", "testdb"),
             resolve_absolute_string_descriptor("admin/testdb", Desc))),
      cleanup(teardown_temp_store(State))]) :-

    with_test_transaction(Desc,
                          C1,
                          (
                              insert_schema_document(
                                  C1,
                                  _{'@type': "Enum",
                                    '@id': "Color",
                                    '@value': ["red",
                                               "green",
                                               "blue"]}),
                              insert_schema_document(
                                  C1,
                                  _{'@type': "Class",
                                    '@id': "Thing",
                                    color: "Color"})
                          )),

    with_test_transaction(Desc,
                          C2,
                          (   insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    color: "red"},
                                  Doc1),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    color: "red"},
                                  Doc2),
                              insert_document(
                                  C2,
                                  _{'@type': "Thing",
                                    color: "blue"},
                                  _Doc3)
                          )),

    open_descriptor(Desc, Db),
    findall(Uri,
            match_query_document_uri(Db,
                                     "Thing",
                                     _{'color': "red"},
                                     Uri),
            Uris),

    sort(Uris, Uris_Sorted),
    sort([Doc1, Doc2], Expected_Uris_Sorted),

    Uris_Sorted = Expected_Uris_Sorted.


:- end_tests(query_document).
