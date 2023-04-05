:- module(api_migration, [
              api_migrate_resource/7,
              api_migrate_resource_to/7
          ]).

:- use_module(core(account)).
:- use_module(core(document/migration)).
:- use_module(core(document/migration_dict)).
:- use_module(core(util)).
:- use_module(core(transaction)).
:- use_module(core(query)).
:- use_module(config(terminus_config), [max_transaction_retries/1]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(yall)).
:- use_module(library(http/json)).

api_migrate_resource(System, Auth, Path, Commit_Info0, Operations, Result, Options) :-
    resolve_descriptor_auth(write, System, Auth, Path, instance, _Descriptor),
    resolve_descriptor_auth(write, System, Auth, Path, schema, Descriptor),
    atom_json_dict(Operations_String, Operations, [default_tag(json)]),
    put_dict(migration, Commit_Info0, Operations_String, Commit_Info),
    perform_instance_migration(Descriptor, Commit_Info, Operations, Result, Options).

auth_check_migrate_resource_to(System, Auth, Path, Target, Our_Descriptor, Their_Descriptor) :-
    resolve_absolute_typed_string_descriptor_ex(Path, branch_descriptor, Our_Descriptor),
    check_descriptor_auth(System, Our_Descriptor, '@schema':'Action/schema_read_access', Auth),
    check_descriptor_auth(System, Our_Descriptor, '@schema':'Action/schema_write_access', Auth),
    check_descriptor_auth(System, Our_Descriptor, '@schema':'Action/commit_write_access', Auth),

    resolve_absolute_typed_string_descriptor_ex(Target, branch_descriptor, Their_Descriptor),
    check_descriptor_auth(System, Their_Descriptor, '@schema':'Action/schema_read_access', Auth),
    check_descriptor_auth(System, Their_Descriptor, '@schema':'Action/commit_read_access', Auth).

schema_migration_for_commit(Transaction, Commit_Id, Migration) :-
    schema_change_for_commit(Transaction, Commit_Id, Change),
    (   Change = no_change
    ->  fail
    ;   Change = no_migration
    ->  throw(error(no_migration_at_commit(Commit_Id), _))
    ;   Change = migration(Migration)
    ->  true
    ;   throw(error(unexpected_change_type(Change)))).

combined_migration_from_commits(Transaction, Commits, Migrations) :-
    convlist(schema_migration_for_commit(Transaction),
             Commits,
             Migrations_List),
    append(Migrations_List, Migrations).

api_migrate_resource_to(System, Auth, Path, Target, Commit_Info, Result, Options) :-
    (   option(dry_run(true), Options)
    ->  api_migrate_resource_to_(System, Auth, Path, Target, Commit_Info, Result, Options)
    ;   api_migrate_resource_to_retry(System, Auth, Path, Target, Commit_Info, Result, Options)
    ).

api_migrate_resource_to_retry(System, Auth, Path, Target, Commit_Info, Result, Options) :-
    max_transaction_retries(Max),
    between(0, Max, _),

    api_migrate_resource_to_(System, Auth, Path, Target, Commit_Info, Result, Options),
    !.
api_migrate_resource_to_retry(_System, _Auth, _Path, _Target, _Commit_Info, _Result, _Options) :-
    throw(error(transaction_retry_exceeded, _)).

api_migrate_resource_to_(System, Auth, Path, Target, Commit_Info0, Result, Options) :-
    % find recent common ancestor
    % determine all migrations that happened in Target since that point
    % - glue together
    % determine all migrations that happened in us since that point
    % - glue together
    % - determine that this is prefix of the target migrations
    % - determine the suffix
    % apply suffix
    auth_check_migrate_resource_to(System, Auth, Path, Target, Our_Descriptor, Their_Descriptor),
    do_or_die(
        open_descriptor(Our_Descriptor, Commit_Info0, Our_Transaction, [], Map),
        error(unresolvable_absolute_descriptor(Our_Descriptor),_)
    ),

    get_dict(parent, Our_Transaction, Our_Repo_Transaction),
    get_dict(repository_descriptor, Their_Descriptor, Their_Repo_Descriptor),
    do_or_die(
        open_descriptor(Their_Repo_Descriptor, Commit_Info0, Their_Repo_Transaction, Map, _),
        error(unresolvable_absolute_descriptor(Our_Descriptor),_)
    ),

    branch_head_commit(Our_Repo_Transaction, Our_Descriptor.branch_name, Our_Commit_Uri),
    commit_id_uri(Our_Repo_Transaction, Our_Commit_Id, Our_Commit_Uri),

    branch_head_commit(Their_Repo_Transaction, Their_Descriptor.branch_name, Their_Commit_Uri),
    commit_id_uri(Their_Repo_Transaction, Their_Commit_Id, Their_Commit_Uri),

    most_recent_common_ancestor(Our_Repo_Transaction, Their_Repo_Transaction, Our_Commit_Id, Their_Commit_Id, Common_Option, Our_Commits, Their_Commits),

    do_or_die(Common_Option = some(_),
              error(no_common_history, _)),

    combined_migration_from_commits(Our_Repo_Transaction, Our_Commits, Our_Migration),
    combined_migration_from_commits(Their_Repo_Transaction, Their_Commits, Their_Migration),

    do_or_die(append(Our_Migration, Suffix, Their_Migration),
              error(no_common_migration_prefix(Our_Migration,Their_Migration), _)),

    (   Suffix = []
    ->  Result = _{ schema_operations: 0, instance_operations: 0 }
    ;   atom_json_dict(Suffix_String, Suffix, [default_tag(json)]),
        put_dict(migration, Commit_Info0, Suffix_String, Commit_Info),
        put_dict(commit_info, Our_Transaction, Commit_Info, Our_Final_Transaction),

        perform_instance_migration_on_transaction(Our_Final_Transaction, Suffix, Result, Options)
    ).
