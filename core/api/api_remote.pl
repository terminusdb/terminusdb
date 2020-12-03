:- module(api_remote, [
              add_remote/5,
              remove_remote/4,
              update_remote/5,
              show_remote/5,
              list_remotes/4
          ]).

:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(account)).

add_remote(SystemDB, Auth, Path, Remote_Name, Remote_Location) :-
    atomic_list_concat([Path, '/_meta'], Repo_Path),

    do_or_die(
        resolve_absolute_string_descriptor(Repo_Path, Descriptor),
        error(invalid_absolute_path(Repo_Path),_)),

    check_descriptor_auth(SystemDB, Descriptor,
                          system:meta_write_access, Auth),

    do_or_die(
        create_context(Descriptor, Context),
        error(unresolvable_descriptor(Descriptor),_)),

    do_or_die(
        \+ has_repository(Context, Remote_Name),
        error(remote_exists(Remote_Name),_)),

    with_transaction(
        Context,
        insert_remote_repository(Context, Remote_Name, Remote_Location, _),
        _
    ).

remove_remote(SystemDB, Auth, Path, Remote_Name) :-
    atomic_list_concat([Path, '/_meta'], Repo_Path),

    do_or_die(
        resolve_absolute_string_descriptor(Repo_Path, Descriptor),
        error(invalid_absolute_path(Repo_Path),_)),

    check_descriptor_auth(SystemDB, Descriptor,
                          system:meta_write_access, Auth),

    do_or_die(
        create_context(Descriptor, Context),
        error(unresolvable_target_descriptor(Descriptor),_)),

    do_or_die(
        has_remote_repository(Context, Remote_Name),
        error(remote_does_not_exist(Remote_Name),_)),

    with_transaction(
        Context,
        remove_remote_repository(Context, Remote_Name),
        _
    ).

update_remote(SystemDB, Auth, Path, Remote_Name, Remote_Location) :-
    atomic_list_concat([Path, '/_meta'], Repo_Path),

    do_or_die(
        resolve_absolute_string_descriptor(Repo_Path, Descriptor),
        error(invalid_absolute_path(Repo_Path),_)),

    check_descriptor_auth(SystemDB, Descriptor,
                          system:meta_write_access, Auth),

    do_or_die(
        create_context(Descriptor, Context),
        error(unresolvable_target_descriptor(Descriptor),_)),

    do_or_die(
        has_remote_repository(Context, Remote_Name),
        error(remote_does_not_exist(Remote_Name),_)),

    with_transaction(
        Context,
        update_repository_remote_url(Context, Remote_Name, Remote_Location),
        _
    ).

show_remote(SystemDB, Auth, Path, Remote_Name, Remote_Location) :-
    atomic_list_concat([Path, '/_meta'], Repo_Path),

    do_or_die(
        resolve_absolute_string_descriptor(Repo_Path, Descriptor),
        error(invalid_absolute_path(Repo_Path),_)),

    check_descriptor_auth(SystemDB, Descriptor,
                          system:meta_write_access, Auth),

    do_or_die(
        create_context(Descriptor, Context),
        error(unresolvable_target_descriptor(Descriptor),_)),

    do_or_die(
        has_remote_repository(Context, Remote_Name),
        error(remote_does_not_exist(Remote_Name),_)),

    repository_remote_url(Context, Remote_Name, Remote_Location).

list_remotes(SystemDB, Auth, Path, Remote_Names) :-
    atomic_list_concat([Path, '/_meta'], Repo_Path),

    do_or_die(
        resolve_absolute_string_descriptor(Repo_Path, Descriptor),
        error(invalid_absolute_path(Repo_Path),_)),

    check_descriptor_auth(SystemDB, Descriptor,
                          system:meta_write_access, Auth),

    do_or_die(
        create_context(Descriptor, Context),
        error(unresolvable_target_descriptor(Descriptor),_)),

    do_or_die(
        has_remote_repository(Context, Remote_Name),
        error(remote_does_not_exist(Remote_Name),_)),

    findall(Remote_Name,
            has_remote_repository(Context, Remote_Name),
            Remote_Names).

