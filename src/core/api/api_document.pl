:- module(api_document, [
              api_generate_documents/9,
              api_generate_documents_by_type/10,
              api_get_document/8,
              api_insert_documents/9,
              api_delete_documents/7,
              api_delete_document/7,
              api_replace_documents/7,
              api_nuke_documents/6
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(triple)).
:- use_module(core(transaction)).
:- use_module(core(document)).

:- use_module(library(http/json)).

api_generate_document_uris_(instance, Transaction, Skip, Count, Uri) :-
    skip_generate_nsols(
        get_document_uri(Transaction, false, Uri),
        Skip,
        Count).
api_generate_document_uris_(schema, Transaction, Skip, Count, Uri) :-
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
    api_generate_document_uris_(instance, Transaction, Skip, Count, Uri),
    get_document(Transaction, Prefixed, Unfold, Uri, Document).

api_generate_documents_(schema, Transaction, _Prefixed, _Unfold, Skip, Count, Document) :-
    api_generate_document_uris_(schema, Transaction, Skip, Count, Uri),
    get_schema_document(Transaction, Uri, Document).

api_generate_documents(_System_DB, _Auth, Path, Schema_Or_Instance, Prefixed, Unfold, Skip, Count, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),
    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),

    api_generate_documents_(Schema_Or_Instance, Transaction, Prefixed, Unfold, Skip, Count, Document).

api_generate_documents_by_type_(schema, Transaction, Type, _Prefixed, _Unfold, Skip, Count, Document) :-
    api_generate_document_uris_by_type_(schema, Transaction, Type, Skip, Count, Uri),
    get_schema_document(Transaction, Uri, Document).
api_generate_documents_by_type_(instance, Transaction, Type, Prefixed, Unfold, Skip, Count, Document) :-
    api_generate_document_uris_by_type_(instance, Transaction, Type, Skip, Count, Uri),
    get_document(Transaction, Prefixed, Unfold, Uri, Document).

api_generate_documents_by_type(_System_DB, _Auth, Path, Graph_Type, Prefixed, Unfold, Type, Skip, Count, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),
    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),

    api_generate_documents_by_type_(Graph_Type, Transaction, Type, Prefixed, Unfold, Skip, Count, Document).

api_get_document_(instance, Transaction, Prefixed, Unfold, Id, Document) :-
    do_or_die(get_document(Transaction, Prefixed, Unfold, Id, Document),
              error(document_not_found(Id), _)).

api_get_document_(schema, Transaction, _Prefixed, _Unfold, Id, Document) :-
    do_or_die(get_schema_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).

api_get_document(_System_DB, _Auth, Path, Schema_Or_Instance, Prefixed, Unfold, Id, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),
    api_get_document_(Schema_Or_Instance, Transaction, Prefixed, Unfold, Id, Document).

api_insert_document_(schema, Transaction, Stream, Id) :-
    json_read_dict_stream(Stream, JSON),
    (   is_list(JSON)
    ->  !,
        member(Document, JSON)
    ;   Document = JSON),
    do_or_die(insert_schema_document(Transaction, Document),
              error(document_insertion_failed_unexpectedly(Document), _)),

    do_or_die(Id = (Document.get('@id')),
              error(document_has_no_id_somehow, _)).
api_insert_document_(instance, Transaction, Stream, Id) :-
    json_read_dict_stream(Stream, JSON),
    (   is_list(JSON)
    ->  !,
        member(Document, JSON)
    ;   Document = JSON),
    do_or_die(insert_document(Transaction, Document, Id),
              error(document_insertion_failed_unexpectedly(Document), _)).

replace_existing_graph(schema, Transaction, Stream) :-
    replace_json_schema(Transaction, Stream).
replace_existing_graph(instance, Transaction, Stream) :-
    [RWO] = (Transaction.instance_objects),
    delete_all(RWO),
    forall(api_insert_document_(instance, Transaction, Stream, _),
           true).

api_insert_documents(_System_DB, _Auth, Path, Schema_Or_Instance, Author, Message, Full_Replace, Stream, Ids) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(resource_does_not_exist(Path), _)),
    query_default_collection(Context, Transaction),

    with_transaction(Context,
                     (   Full_Replace = true
                     ->  replace_existing_graph(Schema_Or_Instance, Transaction, Stream),
                         Ids = []
                     ;   findall(Id,
                                 api_insert_document_(Schema_Or_Instance, Transaction, Stream, Id),
                                 Ids)),
                     _).

api_delete_document_(schema, Transaction, Id) :-
    delete_schema_document(Transaction, Id).
api_delete_document_(instance, Transaction, Id) :-
    delete_document(Transaction, Id).

api_delete_documents(_System_DB, _Auth, Path, Schema_Or_Instance, Author, Message, Stream) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(resource_does_not_exist(Path), _)),
    query_default_collection(Context, Transaction),

    with_transaction(Context,
                     forall(
                         (   json_read_dict_stream(Stream,ID),
                             do_or_die(
                                 string(ID),
                                 error(not_a_proper_id(ID)))),
                         api_delete_document_(Schema_Or_Instance, Transaction, ID)),
                     _).

api_delete_document(_System_DB, _Auth, Path, Schema_Or_Instance, Author, Message, ID) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(resource_does_not_exist(Path), _)),
    query_default_collection(Context, Transaction),

    with_transaction(Context,
                     api_delete_document_(Schema_Or_Instance, Transaction, ID),
                     _).

api_nuke_documents_(schema, Transaction) :-
    nuke_schema_documents(Transaction).
api_nuke_documents_(instance, Transaction) :-
    nuke_documents(Transaction).

api_nuke_documents(_System_DB, _Auth, Path, Schema_Or_Instance, Author, Message) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(resource_does_not_exist(Path), _)),
    query_default_collection(Context, Transaction),

    with_transaction(Context,
                     api_nuke_documents_(Schema_Or_Instance, Transaction),
                    _).

api_replace_document_(schema, Transaction, Document) :-
    replace_schema_document(Transaction, Document).
api_replace_document_(instance, Transaction, Document) :-
    replace_document(Transaction, Document).

api_replace_documents(_System_DB, _Auth, Path, Schema_Or_Instance, Author, Message, Stream) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(resource_does_not_exist(Path), _)),
    query_default_collection(Context, Transaction),

    with_transaction(Context,
                     forall(
                         (   json_read_dict_stream(Stream,JSON),
                             (   is_list(JSON)
                             ->  !,
                                 member(Document, JSON)
                             ;   Document = JSON)),
                         api_replace_document_(Schema_Or_Instance, Transaction, Document)),
                     _).
