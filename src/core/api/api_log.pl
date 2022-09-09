:- module(api_log, [api_log/5, format_log/2]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(library(lists)).

descriptor_type_has_history(branch_descriptor).
descriptor_type_has_history(commit_descriptor).

descriptor_has_history(Descriptor) :-
    Type{} :< Descriptor,
    descriptor_type_has_history(Type).

loggable_commit_uri(Descriptor, Repository_Descriptor, Commit_Uri) :-
    branch_descriptor{repository_descriptor: Repository_Descriptor,
                      branch_name: Branch_Name} :< Descriptor,
    !,
    branch_head_commit(Repository_Descriptor,
                       Branch_Name,
                       Commit_Uri).
loggable_commit_uri(Descriptor, Repository_Descriptor, Commit_Uri) :-
    commit_descriptor{commit_id: Commit_Id,
                      repository_descriptor: Repository_Descriptor} :< Descriptor,
    commit_id_uri(Repository_Descriptor, Commit_Id, Commit_Uri).

api_log(System_DB, Auth, Path, Log, Options) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path,Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(descriptor_has_history(Descriptor),
              error(resource_has_no_history(Descriptor), _)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/meta_read_access', Auth),

    do_or_die(
        open_descriptor(Descriptor, _Branch_Transaction),
        error(unresolvable_absolute_descriptor(Descriptor),_)),

    loggable_commit_uri(Descriptor, Repository_Descriptor, Commit_Uri),

    commit_uri_to_history_commit_uris(Repository_Descriptor, Commit_Uri, History_Commit_Uris, Options),

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
