:- module(api_migration, [
              api_hypothetical_migration/4,
              api_migrate_resource/5,
              api_transform_schema/3
          ]).

:- use_module(core(account)).
:- use_module(core(document/migration)).
:- use_module(core(util)).

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
