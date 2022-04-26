:- module(api_document, [
              api_get_document/6,
              api_get_documents/11,
              api_get_documents_by_type/12,
              api_get_documents_by_query/13,
              api_get_document_by_id/10,
              api_insert_documents/11,
              api_delete_documents/9,
              api_delete_document/9,
              api_replace_documents/11,
              api_nuke_documents/8,
              api_generate_document_ids/6,
              call_catch_document_mutation/2
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(document)).
:- use_module(core(account)).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(plunit)).
:- use_module(library(pprint), [print_term/2]).

before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction) :-
    do_or_die(
        open_descriptor(Descriptor, Transaction),
        error(unresolvable_collection(Descriptor), _)),

    transaction_data_version(Transaction, Actual_Data_Version),
    compare_data_versions(Requested_Data_Version, Actual_Data_Version).

before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction) :-
    do_or_die(
        create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
        error(unresolvable_collection(Descriptor), _)),
    do_or_die(
        query_default_collection(Context, Transaction),
        error(query_default_collection_failed_unexpectedly(Context), _)),

    transaction_data_version(Transaction, Actual_Data_Version),
    compare_data_versions(Requested_Data_Version, Actual_Data_Version).

api_generate_document_ids(instance, Transaction, Unfold, Skip, Count, Id) :-
    (   Unfold = true
    ->  Include_Subdocuments = false
    ;   Include_Subdocuments = true),
    skip_generate_nsols(
        get_document_uri(Transaction, Include_Subdocuments, Id),
        Skip,
        Count).
api_generate_document_ids(schema, Transaction, _Unfold, Skip, Count, Id) :-
    skip_generate_nsols(
        get_schema_document_uri(Transaction, Id),
        Skip,
        Count).

api_get_documents(SystemDB, Auth, Path, Graph_Type, Compress_Ids, Unfold, Skip, Count, Requested_Data_Version, Actual_Data_Version, Goal) :-
    resolve_descriptor_auth(read, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    Goal = {Graph_Type, Transaction, Skip, Count, Compress_Ids, Unfold}/[Document]>>(
        api_document:api_generate_document_ids(Graph_Type, Transaction, Unfold, Skip, Count, Id),
        api_document:api_get_document(Graph_Type, Transaction, Compress_Ids, Unfold, Id, Document)
    ).

api_generate_document_ids_by_type(instance, Transaction, Type, Skip, Count, Id) :-
    skip_generate_nsols(
        get_document_uri_by_type(Transaction, Type, Id),
        Skip,
        Count).
api_generate_document_ids_by_type(schema, Transaction, Type, Skip, Count, Id) :-
    skip_generate_nsols(
        get_schema_document_uri_by_type(Transaction, Type, Id),
        Skip,
        Count).

api_get_documents_by_type(SystemDB, Auth, Path, Graph_Type, Compress_Ids, Unfold, Type, Skip, Count, Requested_Data_Version, Actual_Data_Version, Goal) :-
    resolve_descriptor_auth(read, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    Goal = {Graph_Type, Transaction, Type, Skip, Count, Compress_Ids, Unfold}/[Document]>>(
        api_document:api_generate_document_ids_by_type(Graph_Type, Transaction, Type, Skip, Count, Id),
        api_document:api_get_document(Graph_Type, Transaction, Compress_Ids, Unfold, Id, Document)
    ).

api_generate_document_ids_by_query(instance, Transaction, Type, Query, Skip, Count, Id) :-
    skip_generate_nsols(
        match_query_document_uri(Transaction, Type, Query, Id),
        Skip,
        Count).
api_generate_document_ids_by_query(schema, _Transaction, _Type, _Query, _Skip, _Count, _Id) :-
    throw(error(query_is_only_supported_for_instance_graphs, _)).

api_get_documents_by_query(SystemDB, Auth, Path, Graph_Type, Compress_Ids, Unfold, Type, Query, Skip, Count, Requested_Data_Version, Actual_Data_Version, Goal) :-
    resolve_descriptor_auth(read, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    Goal = {Graph_Type, Transaction, Type, Query, Skip, Count, Compress_Ids, Unfold}/[Document]>>(
        api_document:api_generate_document_ids_by_query(Graph_Type, Transaction, Type, Query, Skip, Count, Id),
        api_document:api_get_document(Graph_Type, Transaction, Compress_Ids, Unfold, Id, Document)
    ).

api_get_document(instance, Transaction, Compress_Ids, Unfold, Id, Document) :-
    do_or_die(get_document(Transaction, Compress_Ids, Unfold, Id, Document),
              error(document_not_found(Id), _)).
api_get_document(schema, Transaction, _Prefixed, _Unfold, Id, Document) :-
    do_or_die(get_schema_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).

api_get_document_by_id(SystemDB, Auth, Path, Graph_Type, Compress_Ids, Unfold, Requested_Data_Version, Actual_Data_Version, Id, Document) :-
    resolve_descriptor_auth(read, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_read(Descriptor, Requested_Data_Version, Actual_Data_Version, Transaction),
    api_get_document(Graph_Type, Transaction, Compress_Ids, Unfold, Id, Document).

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
known_document_error(unable_to_assign_ids).
known_document_error(inserted_subdocument_as_document).
known_document_error(capture_already_bound(_)).
known_document_error(wrong_array_dimensions(_,_)).
known_document_error(not_a_unit_type(_)).

:- meta_predicate call_catch_document_mutation(+, :).
call_catch_document_mutation(Document, Goal) :-
    catch(Goal,
          error(E, Context),
          (   known_document_error(E)
          ->  embed_document_in_error(E, Document, New_E),
              throw(error(New_E, _))
          ;   throw(error(E, Context)))).

api_insert_document_(schema, Transaction, Document, state(Captures), Id, Captures) :-
    call_catch_document_mutation(
        Document,
        do_or_die(insert_schema_document(Transaction, Document),
                  error(document_insertion_failed_unexpectedly(Document), _))),
    do_or_die(Id = (Document.get('@id')),
              error(document_has_no_id_somehow, _)).
api_insert_document_(instance, Transaction, Document, state(Captures_In), Id, Captures_Out) :-
    call_catch_document_mutation(
        Document,
        do_or_die(insert_document(Transaction, Document, Captures_In, Id, _Dependencies, Captures_Out),
                  error(document_insertion_failed_unexpectedly(Document), _))).

api_insert_document_unsafe_(schema, Transaction, Context, Document, state(Captures), Id, Captures) :-
    do_or_die(
        insert_schema_document_unsafe(Transaction, Context, Document),
        error(document_insertion_failed_unexpectedly(Document), _)),
    do_or_die(
        Id = (Document.get('@id')),
        error(document_has_no_id_somehow, _)).
api_insert_document_unsafe_(instance, Transaction, Context, Document, state(Captures_In), Id, Captures_Out) :-
    do_or_die(
        insert_document_unsafe(Transaction, Context, Document, Captures_In, Id, Captures_Out),
        error(document_insertion_failed_unexpectedly(Document), _)).

insert_documents_(true, Graph_Type, Stream, Transaction, Captures_Var, Ids) :-
    api_nuke_documents_(Graph_Type, Transaction),
    (   Graph_Type = schema
    ->  % For a schema full replace, read the context and replace the existing one.
        json_read_required_context(Stream, Context, Tail_Stream),
        replace_context_document(Transaction, Context)
    ;   % Otherwise, do nothing. Tail_Stream is effectively just Stream.
        json_init_tail_stream(Stream, Tail_Stream)
    ),
    findall(
        Id,
        (   json_read_tail_stream(Tail_Stream, Document),
            nb_thread_var(
                {Graph_Type, Transaction, Context, Document, Id}/[State, Captures_Out]>>(
                    api_insert_document_unsafe_(Graph_Type, Transaction, Context, Document, State, Id, Captures_Out)
                ),
                Captures_Var
            )
        ),
        Ids
    ).
insert_documents_(false, Graph_Type, Stream, Transaction, Captures_Var, Ids) :-
    findall(
        Id,
        (   json_read_list_stream(Stream, Document),
            nb_thread_var(
                {Graph_Type, Transaction, Document, Id}/[State, Captures_Out]>>(
                    api_insert_document_(Graph_Type, Transaction, Document, State, Id, Captures_Out)
                ),
                Captures_Var
            )
        ),
        Ids
    ).

api_insert_documents(SystemDB, Auth, Path, Graph_Type, Author, Message, Full_Replace, Stream, Requested_Data_Version, New_Data_Version, Ids) :-
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         empty_assoc(Captures),
                         nb_thread_var_init(Captures, Captures_Var),
                         ensure_transaction_has_builder(Graph_Type, Transaction),
                         insert_documents_(Full_Replace, Graph_Type, Stream, Transaction, Captures_Var, Ids),
                         die_if(nonground_captures(Captures_Var, Nonground),
                                error(not_all_captures_found(Nonground), _)),
                         die_if(has_duplicates(Ids, Duplicates),
                                error(same_ids_in_one_transaction(Duplicates), _))
                     ),
                     Meta_Data),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

nonground_captures(state(Captures), Nonground) :-
    findall(Ref,
            (   gen_assoc(Ref, Captures, Var),
                var(Var)),
            Nonground),
    Nonground \= [].

api_delete_document_(schema, Transaction, Id) :-
    delete_schema_document(Transaction, Id).
api_delete_document_(instance, Transaction, Id) :-
    delete_document(Transaction, Id).

api_delete_documents(SystemDB, Auth, Path, Graph_Type, Author, Message, Stream, Requested_Data_Version, New_Data_Version) :-
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         forall(
                             (   json_read_list_stream(Stream, ID_Unchecked),
                                 param_check_json(non_empty_string, id, ID_Unchecked, ID)),
                             api_delete_document_(Graph_Type, Transaction, ID))),
                     Meta_Data),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_delete_document(SystemDB, Auth, Path, Graph_Type, Author, Message, ID, Requested_Data_Version, New_Data_Version) :-
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    with_transaction(Context,
                     api_delete_document_(Graph_Type, Transaction, ID),
                     Meta_Data),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_nuke_documents_(schema, Transaction) :-
    nuke_schema_documents(Transaction).
api_nuke_documents_(instance, Transaction) :-
    nuke_documents(Transaction).

api_nuke_documents(SystemDB, Auth, Path, Graph_Type, Author, Message, Requested_Data_Version, New_Data_Version) :-
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    with_transaction(Context,
                     api_nuke_documents_(Graph_Type, Transaction),
                     Meta_Data),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

api_replace_document_(instance, Transaction, Document, Create, state(Captures_In), Id, Captures_Out):-
    replace_document(Transaction, Document, Create, Captures_In, Id, _Dependencies, Captures_Out).
api_replace_document_(schema, Transaction, Document, Create, state(Captures_In), Id, Captures_In):-
    replace_schema_document(Transaction, Document, Create, Id).

api_replace_documents(SystemDB, Auth, Path, Graph_Type, Author, Message, Stream, Create, Requested_Data_Version, New_Data_Version, Ids) :-
    resolve_descriptor_auth(write, SystemDB, Auth, Path, Graph_Type, Descriptor),
    before_write(Descriptor, Author, Message, Requested_Data_Version, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         empty_assoc(Captures),
                         nb_thread_var_init(Captures, Captures_Var),
                         ensure_transaction_has_builder(Graph_Type, Transaction),
                         findall(Id,
                                 nb_thread_var(
                                     {Graph_Type, Transaction,Stream,Id}/[State,Captures_Out]>>
                                     (   json_read_list_stream(Stream, Document),
                                         call_catch_document_mutation(
                                             Document,
                                             api_replace_document_(Graph_Type,
                                                                   Transaction,
                                                                   Document,
                                                                   Create,
                                                                   State,
                                                                   Id,
                                                                   Captures_Out))
                                     ),
                                     Captures_Var),
                                 Ids),
                         die_if(nonground_captures(Captures_Var, Nonground),
                                error(not_all_captures_found(Nonground), _)),
                         die_if(has_duplicates(Ids, Duplicates), error(same_ids_in_one_transaction(Duplicates), _))
                     ),
                     Meta_Data),
    meta_data_version(Transaction, Meta_Data, New_Data_Version).

:- begin_tests(delete_document, []).
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
    api_insert_documents(System, 'User/admin', Path, instance, "author", "message", false, Stream, no_data_version, _New_Data_Version, _Ids).

test(delete_objects_with_stream,
     [setup((setup_temp_store(State),
             create_db_with_test_schema(admin,foo))),
      cleanup(teardown_temp_store(State))
     ]) :-
    open_descriptor(system_descriptor{}, System),
    insert_some_cities(System, 'admin/foo'),

    open_string('"City/Dublin" "City/Pretoria"', Stream),
    api_delete_documents(System, 'User/admin', 'admin/foo', instance, "author", "message", Stream, no_data_version, _New_Data_Version),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
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
    api_delete_documents(System, 'User/admin', 'admin/foo', instance, "author", "message", Stream, no_data_version, _New_Data_Version),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
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
    api_delete_documents(System, 'User/admin', 'admin/foo', instance, "author", "message", Stream, no_data_version, _New_Data_Version),

    resolve_absolute_string_descriptor("admin/foo", Descriptor),
    create_context(Descriptor, Context),
    findall(Id_Compressed,
            (   get_document_uri(Context, true, Id),
                compress_dict_uri(Id, Context.prefixes, Id_Compressed)),
            Ids),

    Ids = ['City/Utrecht'].

:- end_tests(delete_document).

:- begin_tests(replace_document, []).
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
    api_insert_documents(System, 'User/admin', Path, instance, "author", "message", false, Stream, no_data_version, _New_Data_Version, _Ids).

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
    api_replace_documents(System, 'User/admin', 'admin/foo', instance, "author", "message", Stream, false, no_data_version, _New_Data_Version, Ids),

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
:- begin_tests(document_id_capture, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(document)).

test(basic_capture, [
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
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
}
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream, no_data_version, _New_Data_Version, _Ids),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).

test(capture_missing, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(not_all_captures_found(["C_Ernie"]))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
}',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream, no_data_version, _New_Data_Version, _Ids).

test(double_capture, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(capture_already_bound("Capture",_))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "@capture": "Capture",
  "name" : "Bert",
  "friends" : []
}
{ "@type": "Person",
  "@capture": "Capture",
  "name" : "Ernie",
  "friends" : []
}
',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream, no_data_version, _New_Data_Version, _Ids).

test(basic_capture_list, [
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
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
[{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
},
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}]',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream, no_data_version, _New_Data_Version, _Ids),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).

test(basic_capture_replace, [
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
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),

    open_string('
{ "@type": "Person",
  "name" : "Bert",
  "friends" : []
}
{ "@type": "Person",
  "name" : "Ernie",
  "friends" : []
}',
                Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream_1, no_data_version, _New_Data_Version_1, _Ids_1),

    open_string('
{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
}
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}',
                Stream_2),

    api_replace_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", Stream_2, false, no_data_version, _New_Data_Version_2, _Ids_2),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).

test(basic_capture_list_replace, [
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
              '@id': "Person",
              '@key': _{'@type': "Lexical",
                        '@fields': ["name"]},
              name: "xsd:string",
              friends: _{'@type': "Set",
                         '@class': "Person"}})
    ),


    open_string('
{ "@type": "Person",
  "name" : "Bert",
  "friends" : []
}
{ "@type": "Person",
  "name" : "Ernie",
  "friends" : []
}',
                Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream_1, no_data_version, _New_Data_Version_1, _Ids_1),

    open_string('
[{ "@type": "Person",
  "@capture": "C_Bert",
  "name" : "Bert",
  "friends" : {"@ref" : "C_Ernie"}
},
{ "@type": "Person",
  "@capture": "C_Ernie",
  "name" : "Ernie",
  "friends" : {"@ref" : "C_Bert"}
}]',
                Stream_2),

    api_replace_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", Stream_2, false, no_data_version, _New_Data_Version_2, _Ids_2),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).


:- end_tests(document_id_capture).


:- begin_tests(subdocument_as_document, []).
:- use_module(core(util/test_utils)).
:- use_module(core(transaction)).
:- use_module(core(document)).

test(insert_subdocument_as_document, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(inserted_subdocument_as_document(_{'@type':"Thing"}))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Thing",
              '@key': _{
                          '@type': "Random"
                      },
              '@subdocument': []})
    ),


    open_string('
{"@type": "Thing"}
',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream, no_data_version, _New_Data_Version, _Ids).

test(replace_nonexisting_subdocument_as_document, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(inserted_subdocument_as_document(_{'@type':"Thing"}))
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        insert_schema_document(
            C1,
            _{'@type': "Class",
              '@id': "Thing",
              '@key': _{
                          '@type': "Random"
                      },
              '@subdocument': []})
    ),


    open_string('
{"@type": "Thing"}
',
                Stream),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_replace_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", Stream, true, no_data_version, _New_Data_Version, _Ids).

test(replace_existing_subdocument_as_document, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         % Right now, idgen will generate ids of submitted documents
         % without taking into account that they may be
         % subdocuments. For the replace case, this means we cannot
         % directly replace a subdocument, as the submitted id will
         % never match the generated id. idgen fix is planned but not
         % yet implemented. Unblock when it is here.
         blocked('idgen prevents subdocument replace')
     ]) :-
    with_test_transaction(
        Desc,
        C1,
        (   insert_schema_document(
                C1,
                _{'@type': "Class",
                  '@id': "Thing",
                  '@key': _{
                              '@type': "Random"
                          },
                  '@subdocument': [],
                  'name': "xsd:string"}),
            insert_schema_document(
                C1,
                _{'@type': "Class",
                  '@id': "Outer",
                  '@key': _{
                              '@type': "Random"
                          },
                  'thing': "Thing"}))
    ),


    open_string('
{"@type": "Outer",
 "thing": {"@type": "Thing", "name": "Foo"}
}
',
                Stream_1),

    open_descriptor(system_descriptor{}, SystemDB),
    super_user_authority(Auth),

    api_insert_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", false, Stream_1, no_data_version, _New_Data_Version_1, [Outer_Id]),

    get_document(Desc, Outer_Id, Outer_Document),
    Id = (Outer_Document.thing.'@id'),

    format(string(Replace_String),
           '{"@type": "Thing", "@id": "~q", "name": "Bar"}',
           [Id]),
    open_string(Replace_String, Stream_2),

    api_replace_documents(SystemDB, Auth, "admin/testdb", instance, "author", "message", Stream_2, false, no_data_version, _New_Data_Version_2, _Ids_2),

    get_document(Desc, Id, _Inner_Document).


:- end_tests(subdocument_as_document).
