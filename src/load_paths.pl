:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

% library(http/json) and library(http/json_convert) moved to library(json*)
% in SWI 10. The old paths still work on both 9 and 10 via compat stubs, so
% keep them and silence the move notice.
:- multifile user:message_hook/3.
:- dynamic user:message_hook/3.
user:message_hook(deprecated(moved_library(_,_)), _, _).

:- use_module(library(filesex)).

:- set_prolog_flag(terminusdb_monolithic_module, true).

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
    (   getenv('TERMINUSDB_PLUGINS_PATH', Value),
        absolute_file_name(Value, Config)
    ->  true
    ;   user:file_search_path(terminus_home, Dir),
        directory_file_path(Dir,'plugins',Config)
    ),
    asserta(user:file_search_path(plugins, Config)).

:- add_plugin_path.

% Built-in plugins are always loaded. They live in builtin_plugins/ at the
% repository root (a sibling of src/) unless TERMINUSDB_BUILTIN_PLUGINS_PATH
% overrides the location (installed deployments, Docker).
%
% The plugins(...) alias resolves built-in plugins FIRST, then user plugins,
% so that use_module(plugins(name)) inside plugin code finds core modules
% before user-supplied ones of the same name.
add_builtin_plugin_path :-
    (   getenv('TERMINUSDB_BUILTIN_PLUGINS_PATH', Value),
        absolute_file_name(Value, Config)
    ->  true
    ;   user:file_search_path(terminus_home, Dir),
        directory_file_path(Dir, '..', Parent),
        directory_file_path(Parent, 'builtin_plugins', Config)
    ),
    asserta(user:file_search_path(builtin_plugins, Config)),
    % asserta puts the built-in dir ahead of the plugins/ entry asserted above.
    asserta(user:file_search_path(plugins, Config)).

:- add_builtin_plugin_path.

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
    asserta(user:file_search_path(foreign, Rust)),
    % foreign(no_save) binaries bake these paths at build time and are not
    % relocatable. Assert each known install location as a candidate, tried
    % before the build path; the first existing file wins.
    asserta(user:file_search_path(foreign, '/snap/terminusdb/current/src/rust')),
    asserta(user:file_search_path(foreign, '/app/terminusdb/src/rust')).

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

:- multifile user:prolog_load_file/2.

%% user:prolog_load_file(+Module:Spec, +Options) is semidet.
%
%  Standalone saved states embed every module loaded at build time but
%  keep no source-file bookkeeping, so a use_module in a runtime-loaded
%  plugin always resolves through the filesystem — where the baked
%  build paths are dead (snap, Docker) and a relocated copy would hit
%  "no permission to redefine module" on load. When Spec expands to the
%  very file a module was already loaded from, skip the load and run
%  only the import wiring, exactly as '$already_loaded' would.
user:prolog_load_file(Context:Spec, Options) :-
    compound(Spec),
    functor(Spec, _, 1),
    memberchk(if(not_loaded), Options),
    expand_file_search_path(Spec, Path),
    file_name_extension(Path, pl, File),
    module_property(Module, file(File)),
    !,
    system:'$import_from_loaded_module'(Module, Context, Options).
