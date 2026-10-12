:- module(api_info, [info/3]).

:- use_module(core(util)).
:- use_module(core(triple)).
:- use_module(config(terminus_config), [terminusdb_version/1,
                                        is_memory_mode/0,
                                        is_enterprise/0,
                                        indexer_backend/1]).

:- use_module(library(terminus_store), [terminus_store_version/1]).

% indexer_available/0 is a semidet predicate exported by plugin_api.
% It succeeds when the Rust indexer FFI predicates are registered.
% We use autoload instead of explicit import to avoid load-order issues.
:- autoload(core(plugin_api), [indexer_available/0]).

info(_System_DB, Auth, Info) :-
    terminusdb_version(TerminusDB_Version),
    terminus_store_version(TerminusDB_Store_Version),
    current_prolog_flag(terminusdb_git_hash, Git_Hash),
    % In memory mode, there's no storage version file - use current database_version
    (   is_memory_mode
    ->  database_version(Storage_Version)
    ;   get_db_version(Storage_Version)
    ),
    number_string(Storage_Version, Storage_Version_String),
    server_edition(Edition),
    http_engine(Http_Engine),

    indexer_info(Indexer_Info),
    (   is_anonymous_authority(Auth)
    ->  Info = _{
               authority: Auth,
               edition: Edition,
               http_engine: Http_Engine,
               indexer: Indexer_Info,
               storage:
               _{
                   version: Storage_Version_String
               }
           }
    ;   Info = _{
               authority: Auth,
               edition: Edition,
               http_engine: Http_Engine,
               indexer: Indexer_Info,
               terminusdb :
               _{
                   version : TerminusDB_Version,
                   git_hash : Git_Hash
               },
               terminusdb_store :
               _{
                   version : TerminusDB_Store_Version
               },
               storage :
               _{
                   version: Storage_Version_String
               }
           }
    ).

%% server_edition(-Edition) is det.
%
%  Reports which TerminusDB edition is running. The value is the atom
%  `community` for the open source build and `enterprise` when the
%  enterprise extension is loaded (terminusdb_enterprise flag set).
server_edition(enterprise) :-
    is_enterprise,
    !.
server_edition(community).

%% http_engine(-Engine) is det.
%
%  Reports which HTTP engine is serving requests. The value is the atom
%  `metal` when the Rust webserver is active (the default) and `swipl`
%  when the traditional SWI-Prolog HTTP server is selected via
%  TERMINUSDB_SERVER_BACKEND=swipl.
http_engine(Engine) :-
    (   getenv('TERMINUSDB_SERVER_BACKEND', BackendEnv)
    ->  atom_string(BackendEnv, BackendAtom),
        downcase_atom(BackendAtom, Backend),
        (   Backend == swipl
        ->  Engine = swipl
        ;   Engine = metal
        )
    ;   Engine = metal
    ).

%% indexer_info(-Info) is det.
%
%  Reports the indexer backend configuration and availability.
%  - backend: the configured backend atom (none, http_vectorlink, http_legacy_vectorlink)
%  - configured: true when backend is not 'none'
%  - available: true when the Rust indexer FFI predicates are registered
%    (i.e. the indexer runtime is loaded and operational)
indexer_info(Info) :-
    (   catch(indexer_backend(Backend), _, Backend = none)
    ->  true
    ;   Backend = none
    ),
    (   Backend \= none
    ->  Configured = true
    ;   Configured = false
    ),
    (   catch(indexer_available, _, fail)
    ->  Available = true
    ;   Available = false
    ),
    Info = _{
        backend: Backend,
        configured: Configured,
        available: Available
    }.

%% is_anonymous_authority(+Auth) is semidet.
%
%  True when Auth is the anonymous user authority, i.e. the request was
%  made without any authentication credentials. The anonymous authority
%  URI is the same one asserted by authenticate/3 in routes.pl when no
%  authentication information is submitted.
is_anonymous_authority('terminusdb://system/data/User/anonymous') :- !.
is_anonymous_authority(doc:anonymous) :- !.

:- begin_tests(api_info, []).
:- use_module(core(util/test_utils)).
:- use_module(core(triple/constants), [super_user_authority/1]).
:- use_module(config(terminus_config), [set_memory_mode/0,
                                        is_memory_mode/0]).

% server_edition/1 is pure logic over the terminusdb_enterprise flag.
% It reports the atom `community` for the open source build and
% `enterprise` when the enterprise extension is loaded.
test(server_edition_is_atom) :-
    server_edition(Edition),
    atom(Edition).

test(server_edition_matches_enterprise_flag) :-
    server_edition(Edition),
    (   is_enterprise
    ->  Edition == enterprise
    ;   Edition == community
    ).

% http_engine/1 reports the HTTP engine serving requests. The default is
% `metal` (the Rust webserver); `swipl` is selected only when
% TERMINUSDB_SERVER_BACKEND=swipl is set.
test(http_engine_default_is_metal,
     [setup(unsetenv('TERMINUSDB_SERVER_BACKEND')),
      cleanup(unsetenv('TERMINUSDB_SERVER_BACKEND'))]) :-
    http_engine(Engine),
    Engine == metal.

test(http_engine_swipl_when_env_set,
     [setup(setenv('TERMINUSDB_SERVER_BACKEND', 'swipl')),
      cleanup(unsetenv('TERMINUSDB_SERVER_BACKEND'))]) :-
    http_engine(Engine),
    Engine == swipl.

test(http_engine_metal_when_env_set,
     [setup(setenv('TERMINUSDB_SERVER_BACKEND', 'rust')),
      cleanup(unsetenv('TERMINUSDB_SERVER_BACKEND'))]) :-
    http_engine(Engine),
    Engine == metal.

test(http_engine_case_insensitive,
     [setup(setenv('TERMINUSDB_SERVER_BACKEND', 'SWIPL')),
      cleanup(unsetenv('TERMINUSDB_SERVER_BACKEND'))]) :-
    http_engine(Engine),
    Engine == swipl.

% is_anonymous_authority/1 recognises both the anonymous URI used by the
% route handler and the doc:anonymous term.
test(is_anonymous_authority_uri) :-
    is_anonymous_authority('terminusdb://system/data/User/anonymous').
test(is_not_anonymous_authority_for_admin) :-
    \+ is_anonymous_authority('terminusdb://system/data/User/admin').

% indexer_info/1 always produces a dict with backend, configured, and available.
test(indexer_info_produces_dict) :-
    indexer_info(Info),
    get_dict(backend, Info, _),
    get_dict(configured, Info, _),
    get_dict(available, Info, _).

% indexer_info/1 reports configured=false when backend is none.
test(indexer_info_none_backend_not_configured,
     [setup(terminus_config:clear_indexer_backend_config),
      cleanup(terminus_config:clear_indexer_backend_config)]) :-
    indexer_info(Info),
    get_dict(backend, Info, Backend),
    Backend == none,
    get_dict(configured, Info, false).

% info/3 for a logged-in user includes all sections.
test(info_logged_in_includes_all_sections,
     [setup((setup_temp_store(State),
             set_memory_mode)),
      cleanup((terminus_config:retractall(memory_mode_enabled),
               teardown_temp_store(State)))
     ]) :-
    super_user_authority(Auth),
    info(_System_DB, Auth, Info),
    get_dict(authority, Info, Auth),
    get_dict(edition, Info, _),
    get_dict(http_engine, Info, _),
    get_dict(indexer, Info, _),
    get_dict(terminusdb, Info, _),
    get_dict(terminusdb_store, Info, _),
    get_dict(storage, Info, _).

% info/3 for an anonymous user omits terminusdb and terminusdb_store
% but still includes the indexer section.
test(info_anonymous_omits_version_sections,
     [setup((setup_temp_store(State),
             set_memory_mode)),
      cleanup((terminus_config:retractall(memory_mode_enabled),
               teardown_temp_store(State)))
     ]) :-
    info(_System_DB, 'terminusdb://system/data/User/anonymous', Info),
    get_dict(authority, Info, 'terminusdb://system/data/User/anonymous'),
    get_dict(edition, Info, _),
    get_dict(http_engine, Info, _),
    get_dict(indexer, Info, _),
    get_dict(storage, Info, _),
    \+ get_dict(terminusdb, Info, _),
    \+ get_dict(terminusdb_store, Info, _).

:- end_tests(api_info).
