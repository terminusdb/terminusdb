:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- use_module(library(filesex)).

root_path(Path) :-
    prolog_load_context(file, File),
    relative_file_name(Path, File, '../').

add_terminus_home_path :-
    root_path(RootDir),
    atom_concat(RootDir, 'src', SrcDir),
    asserta(user:file_search_path(terminus_home, SrcDir)).

:- add_terminus_home_path.

add_library_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/library',Lib),
    asserta(user:file_search_path(library, Lib)).

:- add_library_path.

:- use_module(library(prolog_pack)).

add_core_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/core',Core),
    asserta(user:file_search_path(core, Core)).

:- add_core_path.

add_server_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/server',Server),
    asserta(user:file_search_path(server, Server)).

:- add_server_path.

add_cli_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/cli',Core),
    asserta(user:file_search_path(cli, Core)).

:- add_cli_path.

add_jwt_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/prolog_jwt/prolog',Library),
    asserta(user:file_search_path(library, Library)).

:- add_jwt_path.

add_config_path :-
    % Global directory
    % asserta(user:file_search_path(config, '/etc')),
    % home or app directory
    user:file_search_path(app_preferences, App_Config),
    asserta(user:file_search_path(config, App_Config)),
    % relative path
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/config',Config),
    asserta(user:file_search_path(config, Config)).

:- add_config_path.
:- use_module(config(terminus_config)).

add_pack_path :-
    (   pack_dir(PackDir)
    ->  attach_packs(PackDir, [duplicate(replace)])
    ;   true).

:- add_pack_path.

add_test_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/test',Config),
    asserta(user:file_search_path(test, Config)).

:- add_test_path.

add_plugin_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/plugins',Config),
    asserta(user:file_search_path(plugins, Config)).

:- add_plugin_path.

add_ontology_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/terminus-schema',Ontology),
    asserta(user:file_search_path(ontology, Ontology)).

:- add_ontology_path.

add_template_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir,'/utils',Template),
    asserta(user:file_search_path(template, Template)).

:- add_template_path.

add_rust_path :-
    user:file_search_path(terminus_home, Dir),
    atom_concat(Dir, '/rust',Rust),
    asserta(user:file_search_path(foreign, Rust)).

:- add_rust_path.

add_enterprise_path :-
    root_path(Dir),
    atom_concat(Dir, 'terminusdb-enterprise/prolog', Enterprise),
    asserta(user:file_search_path(enterprise, Enterprise)).

add_enterprise_test_path :-
    root_path(Dir),
    atom_concat(Dir, 'terminusdb-enterprise/test', Enterprise),
    asserta(user:file_search_path(enterprise_test, Enterprise)).

:- if(getenv("TERMINUSDB_ENTERPRISE", true)).
:- add_enterprise_path.
:- add_enterprise_test_path.
:- set_prolog_flag(terminusdb_enterprise, true).
:- endif.
