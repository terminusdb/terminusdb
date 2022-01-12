:- module(api_document, [
              api_get_document_read_transaction/5,
              api_generate_document_ids/6,
              api_generate_document_ids_by_type/6,
              api_generate_document_ids_by_query/7,
              api_get_document/6,
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
:- use_module(library(lists)).
:- use_module(library(plunit)).

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

api_get_document_read_transaction(SystemDB, Auth, Path, Schema_Or_Instance, Transaction) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, read),

    do_or_die(
        open_descriptor(Descriptor, Transaction),
        error(unresolvable_collection(Descriptor), _)).

api_get_document_write_transaction(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Context, Transaction) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    assert_document_auth(SystemDB, Auth, Descriptor, Schema_Or_Instance, write),

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(unresolvable_collection(Descriptor), _)),
    do_or_die(query_default_collection(Context, Transaction),
              error(query_default_collection_failed_unexpectedly(Context), _)).

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

api_generate_document_ids_by_query(instance, Transaction, Type, Query, Skip, Count, Id) :-
    skip_generate_nsols(
        match_query_document_uri(Transaction, Type, Query, Id),
        Skip,
        Count).
api_generate_document_ids_by_query(schema, _Transaction, _Type, _Query, _Skip, _Count, _Id) :-
    throw(error(query_is_only_supported_for_instance_graphs, _)).

api_get_document(instance, Transaction, Compress_Ids, Unfold, Id, Document) :-
    do_or_die(get_document(Transaction, Compress_Ids, Unfold, Id, Document),
              error(document_not_found(Id), _)).

api_get_document(schema, Transaction, _Prefixed, _Unfold, Id, Document) :-
    do_or_die(get_schema_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).

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

replace_existing_graph(schema, Transaction, Stream) :-
    replace_json_schema(Transaction, Stream).
replace_existing_graph(instance, Transaction, Stream) :-
    [RWO] = (Transaction.instance_objects),
    delete_all(RWO),
    empty_assoc(Captures),
    forall(json_read_dict_list_stream(Stream, Document),
           nb_thread_var({Transaction,Document}/[State,Captures_Out]>>(api_insert_document_(instance, Transaction, Document, State, _, Captures_Out)),
                         state(Captures))).

api_insert_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Full_Replace, Stream, Ids) :-
    api_get_document_write_transaction(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         Full_Replace = true
                     ->  replace_existing_graph(Schema_Or_Instance, Transaction, Stream),
                         Ids = []
                     ;   empty_assoc(Captures),
                         Captures_Var = state(Captures),
                         ensure_transaction_has_builder(Schema_Or_Instance, Transaction),
                         findall(Id,
                                 (   json_read_dict_list_stream(Stream, Document),
                                     nb_thread_var({Schema_Or_Instance,Transaction,Document,Id}/[State,Captures_Out]>>(api_insert_document_(Schema_Or_Instance, Transaction, Document, State, Id, Captures_Out)),
                                                   Captures_Var)),
                                 Ids),

                         die_if(nonground_captures(Captures_Var, Nonground),
                                error(not_all_captures_found(Nonground), _)),
                         die_if(has_duplicates(Ids, Duplicates), error(same_ids_in_one_transaction(Duplicates), _))),
                     _).

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

api_delete_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Stream) :-
    api_get_document_write_transaction(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         forall(
                             (   json_read_dict_stream(Stream,JSON),
                                 (   is_list(JSON)
                                 ->  member(ID_Unchecked, JSON)
                                 ;   ID_Unchecked = JSON),
                                 param_check_json(string, id, ID_Unchecked, ID)),
                             api_delete_document_(Schema_Or_Instance, Transaction, ID))),
                     _).

api_delete_document(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, ID) :-
    api_get_document_write_transaction(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Context, Transaction),
    with_transaction(Context,
                     api_delete_document_(Schema_Or_Instance, Transaction, ID),
                     _).

api_nuke_documents_(schema, Transaction) :-
    nuke_schema_documents(Transaction).
api_nuke_documents_(instance, Transaction) :-
    nuke_documents(Transaction).

api_nuke_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message) :-
    api_get_document_write_transaction(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Context, Transaction),
    with_transaction(Context,
                     api_nuke_documents_(Schema_Or_Instance, Transaction),
                    _).

api_replace_document_(instance, Transaction, Document, Create, state(Captures_In), Id, Captures_Out):-
    replace_document(Transaction, Document, Create, Captures_In, Id, _Dependencies, Captures_Out).
api_replace_document_(schema, Transaction, Document, Create, state(Captures_In), Id, Captures_In):-
    replace_schema_document(Transaction, Document, Create, Id).

api_replace_documents(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Stream, Create, Ids) :-
    api_get_document_write_transaction(SystemDB, Auth, Path, Schema_Or_Instance, Author, Message, Context, Transaction),
    stream_property(Stream, position(Pos)),
    with_transaction(Context,
                     (   set_stream_position(Stream, Pos),
                         empty_assoc(Captures),
                         Captures_Var = state(Captures),
                         ensure_transaction_has_builder(Schema_Or_Instance, Transaction),
                         findall(Id,
                                 nb_thread_var(
                                     {Schema_Or_Instance, Transaction,Stream,Id}/[State,Captures_Out]>>
                                     (   json_read_dict_list_stream(Stream,Document),
                                         call_catch_document_mutation(
                                             Document,
                                             api_replace_document_(Schema_Or_Instance,
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
:- begin_tests(document_id_capture).
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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream,
                         _Ids),

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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream,
                         _Ids).

test(double_capture, [
         setup((setup_temp_store(State),
                create_db_with_empty_schema("admin", "testdb"),
                resolve_absolute_string_descriptor("admin/testdb", Desc))),
         cleanup(teardown_temp_store(State)),
         error(capture_already_bound("Capture"))
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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream,
                         _Ids).
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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream,
                         _Ids),

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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream_1,
                         _Ids_1),

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


    api_replace_documents(SystemDB,
                          Auth,
                          "admin/testdb",
                          instance,
                          "testauthor",
                          "testmessage",
                          Stream_2,
                          false,
                          _Ids_2),

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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream_1,
                         _Ids_1),

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


    api_replace_documents(SystemDB,
                          Auth,
                          "admin/testdb",
                          instance,
                          "testauthor",
                          "testmessage",
                          Stream_2,
                          false,
                          _Ids_2),

    open_descriptor(Desc, T),
    get_document(T, 'Person/Bert', Bert),
    get_document(T, 'Person/Ernie', Ernie),

    ['Person/Ernie'] = (Bert.friends),
    ['Person/Bert'] = (Ernie.friends).


:- end_tests(document_id_capture).


:- begin_tests(subdocument_as_document).
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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream,
                         _Ids).

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

    api_replace_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         Stream,
                         true,
                         _Ids).

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

    api_insert_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         false,
                         Stream_1,
                         [Outer_Id]),

    get_document(Desc, Outer_Id, Outer_Document),
    Id = (Outer_Document.thing.'@id'),

    format(string(Replace_String),
           '{"@type": "Thing", "@id": "~q", "name": "Bar"}',
           [Id]),
    open_string(Replace_String, Stream_2),

    api_replace_documents(SystemDB,
                         Auth,
                         "admin/testdb",
                         instance,
                         "testauthor",
                         "testmessage",
                         Stream_2,
                         false,
                         _Ids),

    get_document(Desc, Id, Inner_Document),
    print_term(Inner_Document),nl.


:- end_tests(subdocument_as_document).
