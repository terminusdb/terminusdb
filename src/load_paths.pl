:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- use_module(library(filesex)).

:- set_prolog_flag(terminusdb_monolithic_module, true).

% Ensure SWI-Prolog library path is set correctly in snap environment
add_swi_library_path :-
    (   getenv('SWI_HOME_DIR', SwiHome),
        % SECURITY: Only trust paths within snap environment
        getenv('SNAP', SnapRoot),
        % Verify SWI_HOME_DIR is within SNAP directory (prevent path injection)
        sub_atom(SwiHome, 0, _, _, SnapRoot)
    ->  % In snap environment - add SWI-Prolog library paths
        directory_file_path(SwiHome, 'library', LibDir),
        (   exists_directory(LibDir)
        ->  % Append (not prepend) to avoid shadowing system libraries
            assertz(user:file_search_path(library, LibDir))
        ;   true)
    ;   true). % Not in snap or validation failed, use default SWI-Prolog paths

:- add_swi_library_path.


% The top-level directory of the repository.
top_level_directory(Path) :-
    prolog_load_context(file, File),
    relative_file_name(Path, File, '..').

add_terminus_home_path :-
    top_level_directory(TopDir),
    directory_file_path(TopDir, 'src', SrcDir),
    asserta(user:file_search_path(terminus_home, SrcDir)).

:- add_terminus_home_path.

add_library_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'library',Lib),
    asserta(user:file_search_path(library, Lib)).

:- add_library_path.

:- use_module(library(prolog_pack)).

add_core_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'core',Core),
    asserta(user:file_search_path(core, Core)).

:- add_core_path.

add_server_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'server',Server),
    asserta(user:file_search_path(server, Server)).

:- add_server_path.

add_cli_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'cli',Core),
    asserta(user:file_search_path(cli, Core)).

:- add_cli_path.

add_jwt_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'prolog_jwt/prolog',Library),
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
    directory_file_path(Dir,'config',Config),
    asserta(user:file_search_path(config, Config)).

:- add_config_path.
pack_dir(Value) :-
    getenv('TERMINUSDB_SERVER_PACK_DIR', Value).

add_pack_path :-
    (   pack_dir(PackDir)
    ->  attach_packs(PackDir, [duplicate(replace)])
    ;   true).

:- add_pack_path.

add_test_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'test',Config),
    asserta(user:file_search_path(test, Config)).

:- add_test_path.

add_plugin_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'plugins',Config),
    asserta(user:file_search_path(plugins, Config)).

:- add_plugin_path.

add_ontology_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'terminus-schema',Ontology),
    asserta(user:file_search_path(ontology, Ontology)).

:- add_ontology_path.

add_template_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir,'utils',Template),
    asserta(user:file_search_path(template, Template)).

:- add_template_path.

add_rust_path :-
    user:file_search_path(terminus_home, Dir),
    directory_file_path(Dir, 'rust',Rust),
    asserta(user:file_search_path(foreign, Rust)).

:- add_rust_path.

add_enterprise_path :-
    top_level_directory(Dir),
    directory_file_path(Dir, 'terminusdb-enterprise/prolog', Enterprise),
    asserta(user:file_search_path(enterprise, Enterprise)).

add_enterprise_test_path :-
    top_level_directory(Dir),
    directory_file_path(Dir, 'terminusdb-enterprise/test', Enterprise),
    asserta(user:file_search_path(enterprise_test, Enterprise)).

assert_dashboard_path(Dir) :-
    directory_file_path(Dir, 'assets', Assets),
    asserta(user:file_search_path(dashboard, Dir)),
    asserta(user:file_search_path(assets, Assets)).

add_dashboard_path :-
    getenv('TERMINUSDB_DASHBOARD_PATH', Dir),
    assert_dashboard_path(Dir).
add_dashboard_path :-
    top_level_directory(Dir),
    directory_file_path(Dir, 'dashboard', Dashboard),
    assert_dashboard_path(Dashboard).

:- add_dashboard_path.

:- if(getenv("TERMINUSDB_ENTERPRISE", true)).
:- add_enterprise_path.
:- add_enterprise_test_path.
:- set_prolog_flag(terminusdb_enterprise, true).
:- endif.
