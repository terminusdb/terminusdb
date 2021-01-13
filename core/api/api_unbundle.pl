:- module(api_unbundle, [unbundle/4]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(api/api_remote)).
:- use_module(core(api/db_pull)).

unbundle(System_DB, Auth, Path, Payload) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Branch_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (branch_descriptor{} :< Branch_Descriptor),
        error(push_requires_branch(Branch_Descriptor),_)),

    % 1. Query for repo name (packed at beginning of file)
    % (32 is length of md5 hash)
    sub_string(Payload, 0, 32, After, Remote_Name),
    sub_string(Payload, 32, After, 0, Pull_Payload),

    setup_call_cleanup(
        % 2. create repo
        add_remote(System_DB, Auth, Path, Remote_Name, "terminusdb:///bundle"),
        % 3. pull from repo with fake remote predicate
        pull(System_DB, Auth, Path, Remote_Name, "main",
             {Pull_Payload}/[_URL,_Repository_Head_Option,some(P)]>>(Pull_Payload = P),
             _Result
            ),
        % 4. remove repo
        remove_remote(System_DB, Auth, Path, Remote_Name)
    ).
