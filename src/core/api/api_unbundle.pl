:- module(api_unbundle, [unbundle/4]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).
:- use_module(core(api/api_remote)).
:- use_module(core(api/db_pull)).

:- use_module(library(yall)).
:- use_module(library(md5)).

unbundle(System_DB, Auth, Path, Payload) :-
    do_or_die(
        resolve_absolute_string_descriptor(Path, Branch_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (branch_descriptor{} :< Branch_Descriptor),
        error(push_requires_branch(Branch_Descriptor),_)),

    setup_call_cleanup(
        % 1. create repo
        (   random_string(String),
            md5_hash(String, Remote_Name_Atom, []), % 32 chars.
            atom_string(Remote_Name_Atom,Remote_Name),
            add_remote(System_DB, Auth, Path, Remote_Name, "terminusdb:///bundle")
        ),
        % 2. pull from repo with fake remote predicate
        pull(System_DB, Auth, Path, Remote_Name, "main",
             {Payload}/[_URL,_Repository_Head_Option,some(P)]>>(
                 Payload = P),
             _Result
            ),
        % 3. remove repo
        remove_remote(System_DB, Auth, Path, Remote_Name)
    ).
