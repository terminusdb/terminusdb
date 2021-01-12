:- module(api_bundle, [bundle/4]).

:- use_module(core(util)).
:- use_module(core(query)).
:- use_module(core(transaction)).
:- use_module(core(triple)).
:- use_module(core(account)).
:- use_module(library(terminus_store)).

bundle(System_DB, Auth, Path, Payload) :-

    do_or_die(
        resolve_absolute_string_descriptor(Path, Branch_Descriptor),
        error(invalid_absolute_path(Path),_)),

    do_or_die(
        (branch_descriptor{} :< Branch_Descriptor),
        error(push_requires_branch(Branch_Descriptor),_)),

    get_dict(branch_name, Branch_Descriptor, Branch_Target),

    % This looks like it could have race conditions and consistent problems.
    setup_call_cleanup(
        % Setup
        (   random_string(String),
            atomic_list_concat(['Bundle_', String], Remote_Name),
            add_remote(System_DB, Auth, Path, Remote_Name, "terminusdb:///bundle")
        ),
        % Call
        (   push(System_DB, Auth, Branch, Remote_Name, Branch_Target, Options,
                 [_,Payload]>>true, _)
        ),
        % Cleanup
        remove_remote(SystemDB, Auth, Path, Remote_Name)
    ).
