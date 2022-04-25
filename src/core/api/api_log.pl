:- module(api_log, [api_log/4, format_log/2]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(library(lists)).

api_log(System_DB, Auth, Path, Log) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path,Branch_Descriptor),
        error(invalid_absolute_path(Path),_)),

    check_descriptor_auth(System_DB, Branch_Descriptor, '@schema':'Action/meta_read_access', Auth),

    do_or_die(
        open_descriptor(Branch_Descriptor, _Branch_Transaction),
        error(unresolvable_absolute_descriptor(Branch_Descriptor),_)),

    Repository_Descriptor = (Branch_Descriptor.repository_descriptor),
    branch_head_commit(Repository_Descriptor,
                       (Branch_Descriptor.branch_name),
                       Commit_Uri),
    commit_uri_to_history_commit_uris(Repository_Descriptor, Commit_Uri, History_Commit_Uris),

    findall(Commit_Doc,
            (   member(This_Uri, History_Commit_Uris),
                get_document(Repository_Descriptor, This_Uri, Commit_Doc)
            ),
            Rev_Log),
    reverse(Rev_Log, Log).

format_log(Stream, Log) :-
    forall(
        member(Commit_Doc, Log),
        (   get_dict('identifier', Commit_Doc, Id),
            format(Stream,'~s~n', [Id]),
            format(Stream,'--------------------------------~n', []),
            get_dict('timestamp', Commit_Doc, TimeStamp),
            stamp_date_time(TimeStamp, DateTime, 0),
            format_time(Stream, 'Date: %FT%T%:z', DateTime),
            format(Stream, '~n', []),
            get_dict('author', Commit_Doc, Auth),
            format(Stream,'Author: ~s~n', [Auth]),
            get_dict('message', Commit_Doc, Message),
            format(Stream,'Message: ~s~n', [Message]),
            format(Stream, '~n', [])
        )
    ).
