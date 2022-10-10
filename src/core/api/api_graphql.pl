:- module(api_graphql, [handle_graphql_request/8]).

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(document)).

handle_graphql_request(System_DB, Auth, Method, Path_Atom, Input_Stream, Output_Stream, _Content_Type, Content_Length) :-
    atom_string(Path_Atom, Path),
    (   resolve_absolute_string_descriptor(Path, Desc)
    ->  open_descriptor(Desc, Transaction)
    ;   Desc = system_descriptor{},
        Transaction = System_DB),
    (   branch_descriptor{} :< Desc
    ->  Commit_DB = (Transaction.parent),
        Meta_DB = (Commit_DB.parent)
    ;   repository_descriptor{} :< Desc
    ->  Commit_DB = Transaction,
        Meta_DB = (Transaction.parent)
    ;   database_descriptor{} :< Desc
    ->  Commit_DB = none,
        Meta_DB = Transaction
    ;   Commit_DB = none,
        Meta_DB = none
    ),
    all_class_frames(Transaction, Frames, [compress_ids(true),expand_abstract(true),simple(true)]),
    json_log_info_formatted("frames: ~q", [Frames]),
    '$graphql':handle_request(Method, Frames, System_DB, Meta_DB, Commit_DB, Transaction, Auth, Content_Length, Input_Stream, Output_Stream).
