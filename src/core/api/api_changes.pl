:- module(api_changes, [api_changes/5]).

:- use_module(core(util)).
:- use_module(core(document)).
:- use_module(core(account)).
:- use_module(core(query)).
:- use_module(core(transaction)).

:- use_module(library(lists)).
:- use_module(library(option)).

%% api_changes(+System_DB, +Auth, +Path, -Changes, +Options) is det.
%
%  Returns document changes for a given commit or branch path.
%
%  Path may be a branch path (e.g. "admin/foo/local/branch/main") or
%  a commit path (e.g. "admin/foo/local/commit/abc123"). For a branch
%  path, the changes are computed for the branch head commit.
%
%  Changes is a dict with keys:
%    added   - list of document IDs that were inserted
%    changed - list of document IDs that were modified
%    deleted - list of document IDs that were deleted
%
%  Options:
%    count - maximum number of IDs per category (-1 = unlimited, default -1)
api_changes(System_DB, Auth, Path, Changes, Options) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Descriptor),
        error(invalid_absolute_path(Path), _)),

    check_descriptor_auth(System_DB, Descriptor, '@schema':'Action/meta_read_access', Auth),

    (   catch(
            (   open_descriptor(Descriptor, Transaction),
                (   '$changes':collect_changed_documents(Transaction, Added, Changed, Deleted)
                ->  true
                ;   Added = [], Changed = [], Deleted = []
                )
            ),
            _,
            fail
        )
    ->  true
    ;   Added = [], Changed = [], Deleted = []
    ),

    option(count(Count), Options, -1),
    (   Count >= 0
    ->  take_n(Added, Count, Added_limited),
        take_n(Changed, Count, Changed_limited),
        take_n(Deleted, Count, Deleted_limited)
    ;   Added_limited = Added,
        Changed_limited = Changed,
        Deleted_limited = Deleted
    ),

    Changes = json{ added: Added_limited,
                    changed: Changed_limited,
                    deleted: Deleted_limited }.

%% take_n(+List, +N, -Result) is det.
%
%  Take at most N elements from List.
take_n(List, N, Result) :-
    (   N =< 0
    ->  Result = []
    ;   length(List, Len),
        MinLen is min(Len, N),
        length(Result, MinLen),
        append(Result, _, List)
    ).

:- begin_tests(api_changes).

test(take_n_zero) :-
    take_n([a,b,c], 0, R),
    assertion(R == []).

test(take_n_negative) :-
    take_n([a,b,c], -1, R),
    assertion(R == []).

test(take_n_less_than_list) :-
    take_n([a,b,c,d,e], 3, R),
    assertion(R == [a,b,c]).

test(take_n_equal_to_list) :-
    take_n([a,b,c], 3, R),
    assertion(R == [a,b,c]).

test(take_n_greater_than_list) :-
    take_n([a,b], 5, R),
    assertion(R == [a,b]).

test(take_n_empty_list) :-
    take_n([], 3, R),
    assertion(R == []).

:- end_tests(api_changes).
