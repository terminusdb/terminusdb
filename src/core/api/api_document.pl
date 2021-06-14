:- module(api_document, [
              api_generate_documents/5,
              api_generate_documents_by_type/6,
              api_get_document/6,
              api_insert_documents/8
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(document)).

:- use_module(library(http/json)).

api_generate_documents_(instance, Transaction, Document) :-
    get_document(Transaction, Document).

api_generate_documents_(schema, Transaction, Document) :-
    get_schema_document(Transaction, Document).

api_generate_documents(_System_DB, _Auth, Path, Schema_Or_Instance, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),
    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),

    api_generate_documents_(Schema_Or_Instance, Transaction, Document).

api_generate_documents_by_type_(schema, Transaction, Type, Document) :-
    get_schema_document_by_type(Transaction, Type, Document).
api_generate_documents_by_type_(instance, Transaction, Type, Document) :-
    get_document_by_type(Transaction, Type, Document).

api_generate_documents_by_type(_System_DB, _Auth, Path, Graph_Type, Type, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),
    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),

    api_generate_documents_by_type_(Graph_Type, Transaction, Type, Document).

api_get_document_(instance, Transaction, Id, Document) :-
    do_or_die(get_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).

api_get_document_(schema, Transaction, Id, Document) :-
    do_or_die(get_schema_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).
    
api_get_document(_System_DB, _Auth, Path, Schema_Or_Instance, Id, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),
    api_get_document_(Schema_Or_Instance, Transaction, Id, Document).

api_insert_document_(schema, Transaction, Stream, Id) :-
    json_read_dict_stream(Stream, Document),
    insert_schema_document(Transaction, Document),

    do_or_die(Id = (Document.get('@id')),
              error(document_has_no_id_somehow, _)).
api_insert_document_(instance, Transaction, Stream, Id) :-
    json_read_dict_stream(Stream, Document),
    insert_document(Transaction, Document, Id).

api_insert_documents(_System_DB, _Auth, Path, Schema_Or_Instance, Author, Message, Stream, Ids) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(create_context(Descriptor, commit_info{author: Author, message: Message}, Context),
              error(resource_does_not_exist(Path), _)),
    query_default_collection(Context, Transaction),
    with_transaction(Context,
                     findall(Id,
                             api_insert_document_(Schema_Or_Instance, Transaction, Stream, Id),
                             Ids),
                     _).
