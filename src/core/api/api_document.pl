:- module(api_document, [
              api_generate_documents/4,
              api_generate_documents_by_type/5,
              api_get_document/5
          ]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(document)).

api_generate_documents(_System_DB, _Auth, Path, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),
    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),

    get_document(Transaction, Document).

api_generate_documents_by_type(_System_DB, _Auth, Path, Type, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),
    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),

    get_document_by_type(Transaction, Type, Document).
    
api_get_document(_System_DB, _Auth, Path, Id, Document) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_path(Path),_)),

    % todo authentication

    do_or_die(open_descriptor(Descriptor, Transaction),
              error(resource_does_not_exist(Path), _)),
    do_or_die(get_document(Transaction, Id, Document),
              error(document_not_found(Id), _)).

