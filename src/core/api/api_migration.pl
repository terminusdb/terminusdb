:- module(api_migration, [
              api_hypothetical_migration/4,
              api_migrate_resource/5,
              api_migrate_resource_to/6,
              api_transform_schema/3
          ]).

:- use_module(core(account)).
:- use_module(core(document/migration)).
:- use_module(core(util)).
:- use_module(core(transaction)).

operations_string_to_list(Operations_String, Operations) :-
    term_string(Term, Operations_String, [variable_names(VNames)]),
    do_or_die(
        VNames = [],
        error(malformed_operations_string)),
    xfy_list(',', Term, Operations).

api_migrate_resource(System, Auth, Path, Commit_Info0, Operations_String) :-
    resolve_descriptor_auth(write, System, Auth, Path, instance, _Descriptor),
    resolve_descriptor_auth(write, System, Auth, Path, schema, Descriptor),
    operations_string_to_list(Operations_String, Operations),
    put_dict(migration, Commit_Info0, Operations_String, Commit_Info),
    perform_instance_migration(Descriptor, Commit_Info, Operations).

api_hypothetical_migration(_System, _Auth, _Path, _New_Schema) :-
    throw(unimplemented).

api_transform_schema(_Schema, _Operations, _New_Schema) :-
    throw(unimplemented).

auth_check_migrate_resource_to(System, Auth, Path, Target, Our_Descriptor, Their_Descriptor) :-
    resolve_absolute_typed_string_descriptor_ex(Path, branch_descriptor, Our_Descriptor),
    check_descriptor_auth(System, Our_Descriptor, '@schema':'Action/schema_read_access', Auth),
    check_descriptor_auth(System, Our_Descriptor, '@schema':'Action/schema_write_access', Auth),
    check_descriptor_auth(System, Our_Descriptor, '@schema':'Action/commit_write_access', Auth),

    resolve_absolute_typed_string_descriptor_ex(Target, branch_descriptor, Their_Descriptor),
    check_descriptor_auth(System, Their_Descriptor, '@schema':'Action/schema_read_access', Auth),
    check_descriptor_auth(System, Their_Descriptor, '@schema':'Action/commit_read_access', Auth).

schema_migration_for_commit(Transaction, Commit_Id, Migration_String) :-
    schema_change_for_commit(Transaction, Commit_Id, Change),
    (   Change = no_change
    ->  fail
    ;   Change = no_migration
    ->  throw(error(no_migration_at_commit(Commit_Id), _))
    ;   Change = migration(Migration_String)
    ->  true
    ;   throw(error(unexpected_change_type(Change)))).

combined_migration_from_commits(Transaction, Commits, Migration_String) :-
    convlist(schema_migration_for_commit(Transaction),
             Commits,
             Migrations),
    merge_separator_split(Migration_String, ',', Migrations).

api_migrate_resource_to(System, Auth, Path, Target, Commit_Info, Result) :-
    max_transaction_retries(Max),
    between(0, Max, _),

    api_migrate_resource_to_(System, Auth, Path, Target, Commit_Info, Result),
    !.
api_migrate_resource_to(_System, _Auth, _Path, _Target, _Commit_Info, _Result) :-
    throw(error(transaction_retry_exceeded, _)).

api_migrate_resource_to_(System, Auth, Path, Target, Commit_Info0, Result) :-
    % find recent common ancestor
    % determine all migrations that happened in Target since that point
    % - glue together
    % determine all migrations that happened in us since that point
    % - glue together
    % - determine that this is prefix of the target migrations
    % - determine the suffix
    % apply suffix

    auth_check_migrate_resource_to(System, Auth, Path, Target, Our_Descriptor, Their_Descriptor),
    open_descriptor(Our_Descriptor, Commit_Info0, Our_Transaction, [], Map),
    get_dict(parent, Our_Transaction, Our_Repo_Transaction),

    get_dict(repository_descriptor, Their_Descriptor, Their_Repo_Descriptor),
    open_descriptor(Their_Repo_Descriptor, Commit_Info0, Their_Repo_Transaction, Map, _),

    branch_head_commit(Our_Repo_Transaction, Our_Descriptor.branch_name, Our_Commit_Uri),
    commit_id_uri(Our_Repo_Transaction, Our_Commit_Id, Our_Commit_Uri),

    branch_head_commit(Their_Repo_Transaction, Their_Descriptor.branch_name, Their_Commit_Uri),
    commit_id_uri(Their_Repo_Transaction, Their_Commit_Id, Their_Commit_Uri),

    most_recent_common_ancestor(Our_Repo_Transaction, Their_Repo_Transaction, Our_Commit_Id, Their_Commit_Id, Common_Option, Our_Commits, Their_Commits),

    do_or_die(Common_Option = some(_),
              error(no_common_history, _)),

    combined_migration_from_commits(Our_Repo_Transaction, Our_Commits, Our_Migration_String),
    combined_migration_from_commits(Their_Repo_Transaction, Their_Commits, Their_Migration_String),

    do_or_die(string_concat(Our_Migration_String, Suffix0, Their_Migration_String),
              error(migration_impossible, _)),

    (   Suffix0 = ""
    ->  Result = no_migration
    ;   (   string_concat(",", Suffix, Suffix0)
        ->  true
        ;   Suffix = Suffix0),

        put_dict(migration, Commit_Info0, Suffix, Commit_Info),
        put_dict(commit_info, Our_Transaction, Commit_Info, Our_Final_Transaction),

        operations_string_to_list(Suffix, Operations),
        perform_instance_migration_on_transaction(Our_Final_Transaction, Operations),

        Result = migration).


