:- module(api_document, [
              api_generate_documents/9,
              api_generate_documents_by_type/10,
              api_generate_documents_by_query/11,
              api_get_document/8,
              api_insert_documents/9,
              api_delete_documents/7,
              api_delete_document/7,
              api_replace_documents/9,
              api_nuke_documents/6
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(document)).
:- use_module(core(account)).

:- use_module(library(http/json)).

document_auth_action_type(Descriptor_Type, Graph_Type_String, ReadWrite_String, Action) :-
    atom_string(Graph_Type, Graph_Type_String),
    atom_string(ReadWrite, ReadWrite_String),

    document_auth_action_type_(Descriptor_Type, Graph_Type, ReadWrite, Action).
document_auth_action_type_(system_descriptor, _, _, '@schema':'Action/manage_capabilities').
document_auth_action_type_(database_descriptor, _, read, '@schema':'Action/meta_read_access').
document_auth_action_type_(database_descriptor, instance, write, '@schema':'Action/meta_write_access').
document_auth_action_type_(repository_descriptor, _, read, '@schema':'Action/commit_read_access').
document_auth_action_type_(repository_descriptor, instance, write, '@schema':'Action/commit_write_access').
document_auth_action_type_(branch_descriptor, instance, read, '@schema':'Action/instance_read_access').
document_auth_action_type_(branch_descriptor, instance, write, '@schema':'Action/instance_write_access').
document_auth_action_type_(branch_descriptor, schema, read, '@schema':'Action/schema_read_access').
document_auth_action_type_(branch_descriptor, schema, write, '@schema':'Action/schema_write_access').
document_auth_action_type_(commit_descriptor, instance, read, '@schema':'Action/instance_read_access').
document_auth_action_type_(commit_descriptor, schema, read, '@schema':'Action/schema_read_access').

assert_document_auth(SystemDB, Auth, Descriptor, Graph_Type, ReadWrite) :-
    Descriptor_Type{} :< Descriptor,
    do_or_die(document_auth_action_type(Descriptor_Type, Graph_Type, ReadWrite, Action),
              error(document_access_impossible(Descriptor, Graph_Type, ReadWrite), _)),

    check_descriptor_auth(SystemDB, Descriptor, Action, Auth).

api_generate_document_uris_(instance, Transaction, Unfold, Skip, Count, Uri) :-
    (   Unfold = true
    ->  Include_Subdocuments = false
    ;   Include_Subdocuments = true),
    skip_generate_nsols(
        get_document_uri(Transaction, Include_Subdocuments, Uri),
        Skip,
        Count).
api_generate_document_uris_(schema, Transaction, _Unfold, Skip, Count, Uri) :-
    skip_generate_nsols(
        get_schema_document_uri(Transaction, Uri),
        Skip,
        Count).

api_generate_document_uris_by_type_(instance, Transaction, Type, Skip, Count, Uri) :-
    skip_generate_nsols(
        get_document_uri_by_type(Transaction, Type, Uri),
        Skip,
        Count).
api_generate_document_uris_by_type_(schema, Transaction, Type, Skip, Count, Uri) :-
    skip_generate_nsols(
        get_schema_document_uri_by_type(Transaction, Type, Uri),
        Skip,
        Count).

api_generate_documents_(instance, Transaction, Prefixed, Unfold, Skip, Count, Document) :-
    api_generate_document_uris_(instance, Transaction, Unfold, Skip, Count, Uri),
    get_document(Transaction, Prefixed, Unfold, Uri, Document).

api_generate_documents_(schema, Transaction, _Prefixed, Unfold, Skip, Count, Document) :-
    api_generate_document_uris_(schema, Transaction, Unfold, Skip, Count, Uri),
    get_schema_document(Transaction, Uri, Document).

api_generate_documents(SystemDB, Auth, Path, Schema_Or_Instance, Prefixed, Unfold, Skip, Count, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, read),

    do_or_die(open_descriptor(Descriptor, Transaction),
              error(unresolvable_collection(Descriptor), _)),

    api_generate_documents_(Schema_Or_Instance, Transaction, Prefixed, Unfold, Skip, Count, Document).

api_generate_documents_by_type_(schema, Transaction, Type, _Prefixed, _Unfold, Skip, Count, Document) :-
    api_generate_document_uris_by_type_(schema, Transaction, Type, Skip, Count, Uri),
    get_schema_document(Transaction, Uri, Document).
api_generate_documents_by_type_(instance, Transaction, Type, Prefixed, Unfold, Skip, Count, Document) :-
    api_generate_document_uris_by_type_(instance, Transaction, Type, Skip, Count, Uri),
    get_document(Transaction, Prefixed, Unfold, Uri, Document).

api_generate_documents_by_type(SystemDB, Auth, Path, Graph_Type, Prefixed, Unfold, Type, Skip, Count, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Graph_Type, read),

    do_or_die(open_descriptor(Descriptor, Transaction),
              error(unresolvable_collection(Descriptor), _)),

    api_generate_documents_by_type_(Graph_Type, Transaction, Type, Prefixed, Unfold, Skip, Count, Document).

api_generate_documents_by_query(SystemDB, Auth, Path, Graph_Type, Prefixed, Unfold, Type, Query, Skip, Count, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Graph_Type, read),

    do_or_die(open_descriptor(Descriptor, Transaction),
              error(unresolvable_collection(Descriptor), _)),

    do_or_die(Graph_Type = instance,
              error(query_is_only_supported_for_instance_graphs, _)),

    skip_generate_nsols(
        match_query_document_uri(Transaction, Type, Query, Uri),
        Skip,
        Count),
    get_document(Transaction, Prefixed, Unfold, Uri, Document).

api_get_document_(instance, Transaction, Prefixed, Unfold, Id, Document) :-
    do_or_die(get_document(Transaction, Prefixed, Unfold, Id, Document),
              error(document_not_found(Id), _)).

api_get_document_(schema, Transaction, _Prefixed, _Unfold, Id, Document) :-
    do_or_die(get_schema_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).

api_get_document(SystemDB, Auth, Path, Schema_Or_Instance, Prefixed, Unfold, Id, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, read),

    do_or_die(open_descriptor(Descriptor, Transaction),
              error(unresolvable_collection(Descriptor), _)),
    api_get_document_(Schema_Or_Instance, Transaction, Prefixed, Unfold, Id, Document).

embed_document_in_error(Error, Document, New_Error) :-
    Error =.. Error_List,
    append(Error_List, [Document], New_Error_List),
    New_Error =.. New_Error_List.

known_document_error(type_not_found(_)).
known_document_error(can_not_insert_existing_object_with_id(_)).
known_document_error(unrecognized_property(_,_,_)).
known_document_error(casting_error(_,_)).
known_document_error(submitted_id_does_not_match_generated_id(_,_)).
known_document_error(submitted_document_id_does_not_have_expected_prefix(_,_)).
known_document_error(document_key_type_unknown(_)).
known_document_error(document_key_type_missing(_)).
known_document_error(subdocument_key_missing).
known_document_error(key_missing_required_field(_)).
known_document_error(document_key_not_object(_)).
known_document_error(empty_key).
known_document_error(bad_field_value(_, _)).
known_document_error(key_missing_fields(_)).
known_document_error(key_fields_not_an_array(_)).
known_document_error(key_fields_is_empty).

:- meta_predicate call_catch_document_mutation(+, :).
call_catch_document_mutation(Document, Goal) :-
    catch(Goal,
          error(E, Context),
          (   known_document_error(E)
          ->  embed_document_in_error(E, Document, New_E),
              throw(error(New_E, _))
          ;   throw(error(E, Context)))).

api_insert_document_(schema, Transaction, Stream, Id) :-
    json_read_dict_stream(Stream, JSON),
    (   is_list(JSON)
    ->  !,
        member(Document, JSON)
    ;   Document = JSON),
    call_catch_document_mutation(
        Document,
        do_or_die(insert_schema_document(Transaction, Document),
                  error(document_insertion_failed_unexpectedly(Document), _))),

    do_or_die(Id = (Document.get('@id')),
              error(document_has_no_id_somehow, _)).
api_insert_document_(instance, Transaction, Stream, Id) :-
    json_read_dict_stream(Stream, JSON),
    (   is_list(JSON)
    ->  !,
        member(Document, JSON)
    ;   Document = JSON),
    call_catch_document_mutation(
        Document,
        do_or_die(insert_document(Transaction, Document, Id),
                  error(document_insertion_failed_unexpectedly(Document), _))).

replace_existing_graph(schema, Transaction, Stream) :-
    replace_json_schema(Transaction, Stream).
replace_existing_graph(instance, Transaction, Stream) :-
    [RWO] = (Transaction.instance_objects),
    delete_all(RWO),
    forall(api_insert_document_(instance, Transaction, Stream, _),
           true).

api_insert_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Full_Replace, Stream, Ids) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, write),

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(unresolvable_collection(Descriptor), _)),
    query_default_collection(Context, Transaction),

    stream_property(Stream, position(Pos)),

    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         Full_Replace = true
                     ->  replace_existing_graph(Schema_Or_Instance, Transaction, Stream),
                         Ids = []
                     ;   findall(Id,
                                 api_insert_document_(Schema_Or_Instance, Transaction, Stream, Id),
                                 Ids),
                         die_if(has_duplicates(Ids, Duplicates), error(same_ids_in_one_transaction(Duplicates), _))),
                     _).

api_delete_document_(schema, Transaction, Id) :-
    delete_schema_document(Transaction, Id).
api_delete_document_(instance, Transaction, Id) :-
    delete_document(Transaction, Id).

api_delete_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Stream) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, write),

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(unresolvable_collection(Descriptor), _)),
    query_default_collection(Context, Transaction),

    stream_property(Stream, position(Pos)),

    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         forall(
                             (   json_read_dict_stream(Stream,JSON),
                                 (   is_list(JSON)
                                 ->  member(ID, JSON)
                                 ;   ID = JSON),
                                 do_or_die(
                                     string(ID),
                                     error(not_a_proper_id_for_deletion(ID), _))),
                             api_delete_document_(Schema_Or_Instance, Transaction, ID))),
                     _).

api_delete_document(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, ID) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, write),

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(unresolvable_collection(Descriptor), _)),
    query_default_collection(Context, Transaction),

    with_transaction(Context,
                     api_delete_document_(Schema_Or_Instance, Transaction, ID),
                     _).

api_nuke_documents_(schema, Transaction) :-
    nuke_schema_documents(Transaction).
api_nuke_documents_(instance, Transaction) :-
    nuke_documents(Transaction).

api_nuke_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, write),

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(unresolvable_collection(Descriptor), _)),
    query_default_collection(Context, Transaction),

    with_transaction(Context,
                     api_nuke_documents_(Schema_Or_Instance, Transaction),
                    _).

api_replace_document_(instance, Transaction, Document, Create, Id):-
    replace_document(Transaction, Document, Create, Id).
api_replace_document_(schema, Transaction, Document, Create, Id):-
    replace_schema_document(Transaction, Document, Create, Id).

api_replace_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Stream, Create, Ids) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, write),

    do_or_die(
        create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
        error(unresolvable_collection(Descriptor), _)),

    query_default_collection(Context, Transaction),

    stream_property(Stream, position(Pos)),

    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         findall(Id,
                                 (   json_read_dict_stream(Stream,JSON),
                                     (   is_list(JSON)
                                     ->  !,
                                         member(Document, JSON)
                                     ;   Document = JSON),
                                     call_catch_document_mutation(
                                         Document,
                                         api_replace_document_(Schema_Or_Instance,
                                                               Transaction,
                                                               Document,
                                                               Create,
                                                               Id))
                                 ),
                                 Ids),
                         die_if(has_duplicates(Ids, Duplicates), error(same_ids_in_one_transaction(Duplicates), _))
                     ),
                     _).

:- begin_tests(delete_document).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).

insert_some_cities(System, Path) :-
    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Dublin" }
{ "@type": "City",
  "@id" : "City/Pretoria",
  "name" : "Pretoria" }
{ "@type": "City",
  "@id" : "City/Utrecht",
  "name" : "Utrecht" }',
                Stream),
    api_insert_documents(System, 'User/admin', Path, instance, "author", "message", false, Stream, _Out_Ids).

test(delete_objects_with_stream,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('"City/Dublin" "City/Pretoria"', Stream),
    api_delete_documents(system_descriptor{}, 'User/admin', 'admin/foo', instance, "author", "message", Stream),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                'document/json':compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
            Ids),

    Ids = ['City/Utrecht'].

test(delete_objects_with_string,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('["City/Dublin", "City/Pretoria"]', Stream),
    api_delete_documents(system_descriptor{}, 'User/admin', 'admin/foo', instance, "author", "message", Stream),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                'document/json':compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
            Ids),

    Ids = ['City/Utrecht'].

test(delete_objects_with_mixed_string_stream,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('"City/Dublin"\n["City/Pretoria"]', Stream),
    api_delete_documents(system_descriptor{}, 'User/admin', 'admin/foo', instance, "author", "message", Stream),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                'document/json':compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
            Ids),

    Ids = ['City/Utrecht'].

:- end_tests(delete_document).

:- begin_tests(replace_document).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).

insert_some_cities(System, Path) :-
    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Dublin" }
{ "@type": "City",
  "@id" : "City/Pretoria",
  "name" : "Pretoria" }
{ "@type": "City",
  "@id" : "City/Utrecht",
  "name" : "Utrecht" }',
                Stream),
    api_insert_documents(System, 'User/admin', Path, instance, "author", "message", false, Stream, _Out_Ids).

test(replace_objects_with_stream,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('
{ "@type": "City",
  "@id" : "City/Dublin",
  "name" : "Baile Atha Cliath" }
{ "@type": "City",
  "@id" : "City/Pretoria",
  "name" : "Tshwane" }', Stream),
    api_replace_documents(system_descriptor{}, 'User/admin', 'admin/foo', instance, "author", "message", Stream, false, Ids),

    Ids = ['http://example.com/data/world/City/Dublin','http://example.com/data/world/City/Pretoria'].

:- end_tests(replace_document).


:- begin_tests(document_error_reporting).

:- use_module(core(util/test_utils)).
:- use_module(core(document)).
:- use_module(core(api/api_error)).

test(key_missing, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State))
     ]) :-

    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Thing",
              '@key': _{'@type': "Lexical",
                        '@fields': ["field"]},
              field: "xsd:string"})
    ),

    Document = _{'@type': "Thing"},

    % GMG: this is clearly too elaborate to be an effective test...
    catch(
        call_catch_document_mutation(
            Document,
            with_test_transaction(
                Desc,
                C2,
                insert_document(
                    C2,
                    Document,
                    _)
            )
        ),
        Error,
        api_error_jsonld(
            insert_documents,
            Error,
            JSON
        )
    ),
    JSON = _{'@type':'api:InsertDocumentErrorResponse',
             'api:error':
             _{'@type':'api:RequiredKeyFieldMissing',
               'api:document':json{'@type':"Thing"},
               'api:field':'http://somewhere.for.now/schema#field'},
             'api:message':"The required field 'http://somewhere.for.now/schema#field' is missing from the submitted document",
             'api:status':"api:failure"
            }.

:- end_tests(document_error_reporting).
