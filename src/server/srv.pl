:- module(srv, [
              start_server/1
          ]).

:- use_module(core(plugins)).
:- use_module(core(appserver_hooks)).

%% start_server(+Port) is det.
%
%  Start the Rust webserver on Port. Static paths are normalized first, then
%  appserver_start is called. This is used by the main server when
%  TERMINUSDB_SERVER_BACKEND=rust.
start_server(Port) :-
    appserver_hooks:normalize_static_paths,
    '$appserver':appserver_start(Port).
